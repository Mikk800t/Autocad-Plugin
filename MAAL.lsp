;;; ============================================================
;;; MAAL.lsp  -  De nødvendige mål, sat på automatisk
;;; Kommando: FM  (lang: FACADEMAAL)
;;;
;;; Brug:
;;;  1. Skriv FM
;;;  2. Markér det der skal have mål (fx med et vindue hen over det hele)
;;;  3. Målene tegnes i laget Maal
;;;
;;; Scriptet finder selv "enheden" - den størrelse der går igen flest
;;; gange (mursten, flise, blok ...) - og vælger ud fra mønsteret:
;;;
;;; FLISER (enhederne står i lige rækker og kolonner)
;;;  Under og til venstre: tilskåret flise | hele fliser | tilskåret flise
;;;  + samlet mål med antal. Tekst: flisestørrelse, fuge og antal stk.
;;;
;;; MURVÆRK (enhederne står i forbandt)
;;;  Under:   piller, åbninger og mellemrum + samlet længde
;;;  Venstre: samlet højde helt til toppen
;;;  Gesims:  ét mål pr. hjørne - hvor meget gesimsen krager ud
;;;  Gavl:    højde, taghældning og tagkantens bredde ved foden
;;;  Tekst:   stenstørrelse, fuge og skiftehøjde
;;;
;;; KUN STREGER (ingen enheder, fx et omrids eller en plan)
;;;  Kæder med alle lange lodrette/vandrette streger + samlede mål
;;;
;;; RUNDT MURVÆRK (sten set oppefra, lagt i en cirkel eller bue - fx
;;; tegnet med MURRUND i BUER.lsp; også kileformede/huggede sten)
;;;  Hel cirkel: radius (snoren), diameter udvendig og indvendig
;;;  Bue:        radius udv./indv., vinkel og buelængde
;;;  Tekst:      sten, antal pr. skifte, fuge inderst/yderst, evt. hvor
;;;              meget stenene hugges, omkreds (2 x pi x r) eller
;;;              buelængde (2 x pi x r x v / 360)
;;;  OBS-tekst hvis fugerne ikke holder reglerne (standard 7 mm inderst,
;;;  20 mm yderst - kan ændres i MURRUND-dialogen).
;;;
;;; Mål der ville blive meget små, slås sammen. Løse hjælpestreger og
;;; fugerne mellem enhederne måles ikke.
;;;
;;; Gamle mål i laget Maal, der er med i markeringen, slettes først,
;;; så kommandoen kan køres igen, når tegningen er ændret.
;;; Målene bruger den aktuelle målestil (DIMSTYLE).
;;; Én gang U fortryder hele kommandoen.
;;; ============================================================

(vl-load-com)

(setq *fm-lagnavn* "Maal"
      *fm-bundzone* 100.0     ; ting der starter under dette (mm over bunden) står på bunden
      *fm-fugemax*  20.0      ; mellemrum mindre end dette er en fuge
      *fm-niveaufri* 100.0    ; højder tættere end dette slås sammen
      *fm-kantfri*  40.0      ; kanter tættere end dette slås sammen (ingen bittesmå mål)
      *fm-udkrag*   150.0     ; største udkragning pr. skifte der måles
      *fm-pi*       "\\U+03C0")  ; tegnet pi i tekster

;; Rund mur: fugereglerne deles med MURRUND i BUER.lsp (kan ændres i dens dialog)
(if (not *rund-fugemin*)  (setq *rund-fugemin*  7.0))    ; mindste fuge inderst
(if (not *rund-fugemaks*) (setq *rund-fugemaks* 20.0))   ; største fuge yderst

;; ---------- Små hjælpere ----------
(defun fm:tal (x d) (vl-string-subst "," "." (rtos x 2 d)))

;; Boks = (x1 y1 x2 y2 ename)
(defun fm:x1 (b) (car b))
(defun fm:y1 (b) (cadr b))
(defun fm:x2 (b) (caddr b))
(defun fm:y2 (b) (cadddr b))
(defun fm:b  (b) (- (fm:x2 b) (fm:x1 b)))
(defun fm:h  (b) (- (fm:y2 b) (fm:y1 b)))

(defun fm:boks (e / obj mn mx)
  (setq obj (vlax-ename->vla-object e))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
    (progn
      (setq mn (vlax-safearray->list mn)
            mx (vlax-safearray->list mx))
      (list (car mn) (cadr mn) (car mx) (cadr mx) e)))
)

;; Er objektet en lige, lukket firkant (4 hjørner, ingen buer, ikke drejet)?
(defun fm:rektangel-p (ed b / pts)
  (and (= (cdr (assoc 0 ed)) "LWPOLYLINE")
       (= 1 (logand 1 (cdr (assoc 70 ed))))
       (= 4 (cdr (assoc 90 ed)))
       (not (vl-some '(lambda (p) (and (= (car p) 42) (/= 0.0 (cdr p)))) ed))
       (progn
         (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (p) (= (car p) 10)) ed)))
         (vl-every '(lambda (p)
                      (and (or (< (abs (- (car p) (fm:x1 b))) 0.5) (< (abs (- (car p) (fm:x2 b))) 0.5))
                           (or (< (abs (- (cadr p) (fm:y1 b))) 0.5) (< (abs (- (cadr p) (fm:y2 b))) 0.5))))
                   pts)))
)

;; Kandidat = (boks type rektangel-p entget)
(defun fm:ca (x y) (< (abs (- x y)) 2.0))

;; Den størrelse (b h) der går igen flest gange (mindst 4 gange) = enheden
;; (sten, flise, blok ...). nil hvis ingen går igen.
(defun fm:hyppigst (kand / tael a k bedst)
  (foreach k kand
    (setq a (list (fix (+ 0.5 (fm:b (car k)))) (fix (+ 0.5 (fm:h (car k))))))
    (if (setq bedst (assoc a tael))
      (setq tael (subst (cons a (1+ (cdr bedst))) bedst tael))
      (setq tael (cons (cons a 1) tael))))
  (setq bedst nil)
  (foreach t1 tael (if (or (not bedst) (> (cdr t1) (cdr bedst))) (setq bedst t1)))
  (if (and bedst (>= (cdr bedst) 4))
    (list (float (car (car bedst))) (float (cadr (car bedst)))))
)

;; Er kandidaten en enhed? Mursten (lille blok/figur), en hel enhed
;; (også drejet) eller en tilskåret enhed (samme bredde eller højde, men mindre).
;; En firkant på fx 312 x 400 er en kasse/ramme, ikke en enhed.
(defun fm:enhed-p (k / b typ rekt w h mx mn mw mh)
  (setq b (car k) typ (cadr k) rekt (caddr k)
        w (fm:b b) h (fm:h b) mx (max w h) mn (min w h))
  (if modal (setq mw (car modal) mh (cadr modal)))
  (and (>= mn 15.0)
       (or (and (<= mx 400.0) (or (= typ "INSERT") (<= mn 130.0) (not rekt)))
           (and modal
                (or (and (fm:ca w mw) (fm:ca h mh))
                    (and (fm:ca w mh) (fm:ca h mw))
                    (and rekt (fm:ca w mw) (< h mh))
                    (and rekt (fm:ca h mh) (< w mw))))))
)

;; Ikke-enheder bruges som streger
(defun fm:som-streger (k / b)
  (setq b (car k))
  (if (= (cadr k) "LWPOLYLINE")
    (foreach sg (fm:segmenter (cadddr k)) (fm:gem-seg (car sg) (cadr sg)))
    (progn
      (fm:gem-seg (list (fm:x1 b) (fm:y1 b)) (list (fm:x2 b) (fm:y1 b)))
      (fm:gem-seg (list (fm:x1 b) (fm:y2 b)) (list (fm:x2 b) (fm:y2 b)))
      (fm:gem-seg (list (fm:x1 b) (fm:y1 b)) (list (fm:x1 b) (fm:y2 b)))
      (fm:gem-seg (list (fm:x2 b) (fm:y1 b)) (list (fm:x2 b) (fm:y2 b)))))
)

;; Fjern enheder der ligger oven i hinanden (fx sten + skravering)
(defun fm:uden-dubletter (lst / res n)
  (foreach b (vl-sort lst '(lambda (p q) (if (fm:ca (fm:x1 p) (fm:x1 q)) (< (fm:y1 p) (fm:y1 q))
                                           (< (fm:x1 p) (fm:x1 q)))))
    (setq n 0)
    (if (not (vl-some '(lambda (r) (and (< (setq n (1+ n)) 30)
                                        (fm:ca (fm:x1 r) (fm:x1 b)) (fm:ca (fm:y1 r) (fm:y1 b))
                                        (fm:ca (fm:x2 r) (fm:x2 b)) (fm:ca (fm:y2 r) (fm:y2 b))))
                      res))
      (setq res (cons b res))))
  res
)

(defun fm:median (lst) (if lst (nth (/ (length lst) 2) (vl-sort lst '<))))

;; Sorter tal og slå dem sammen der ligger inden for 'tol' af hinanden
(defun fm:unikke (lst tol / res)
  (foreach v (vl-sort lst '<)
    (if (or (not res) (> (- v (car res)) tol))
      (setq res (cons v res))))
  (reverse res)
)

(defun fm:har (lst x) (vl-some '(lambda (v) (< (abs (- v x)) 1.0)) lst))

;; Laget Maal oprettes, hvis det ikke findes
(defun fm:lag ()
  (if (not (tblsearch "LAYER" *fm-lagnavn*))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 *fm-lagnavn*)
                   '(70 . 0) '(62 . 4) '(6 . "Continuous"))))
)

(defun fm:3d (p) (vlax-3d-point (list (car p) (cadr p) 0.0)))

;; Lineært mål p1-p2, målelinjen gennem tp. rot = 0 vandret, pi/2 lodret.
;; tekst = nil (kun målet) eller tekst under målet.
(defun fm:maal (p1 p2 tp rot tekst)
  (fm:gem-dim (vl-catch-all-apply 'vla-AddDimRotated
                (list spc (fm:3d p1) (fm:3d p2) (fm:3d tp) rot))
              (if tekst (strcat "<>\\X" tekst)))
)

;; Nyt mål i laget Maal (evt. med egen tekst). Returnerer målet eller nil.
(defun fm:gem-dim (o txt)
  (if (not (vl-catch-all-error-p o))
    (progn
      (vla-put-Layer o *fm-lagnavn*)
      (if txt (vla-put-TextOverride o txt))
      (setq *fm-antal* (1+ *fm-antal*))
      o))
)

;; ---------- Streger ----------
;; Stykker fra LINE og LWPOLYLINE som liste af (p q)
(defun fm:segmenter (ed / typ pts bl res i n)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "LINE")
     (setq res (list (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed))))))
    ((= typ "LWPOLYLINE")
     (foreach d ed
       (cond ((= (car d) 10) (setq pts (cons (cdr d) pts) bl (cons 0.0 bl)))
             ((= (car d) 42) (setq bl (cons (cdr d) (cdr bl))))))
     (setq pts (reverse pts) bl (reverse bl) n (length pts) i 0)
     (while (< i (if (= 1 (logand 1 (cdr (assoc 70 ed)))) n (1- n)))
       (if (= 0.0 (nth i bl))
         (setq res (cons (list (nth i pts) (nth (rem (1+ i) n) pts)) res)))
       (setq i (1+ i)))))
  res
)

;; Gem lodrette stykker som (x ya yb) og vandrette som (y xa xb)
(defun fm:gem-seg (p q)
  (cond
    ((and (< (abs (- (car p) (car q))) 0.5) (>= (abs (- (cadr p) (cadr q))) 40.0))
     (setq vseg (cons (list (car p) (min (cadr p) (cadr q)) (max (cadr p) (cadr q))) vseg)))
    ((and (< (abs (- (cadr p) (cadr q))) 0.5) (>= (abs (- (car p) (car q))) 40.0))
     (setq hseg (cons (list (cadr p) (min (car p) (car q)) (max (car p) (car q))) hseg)))
    (T (fm:gem-skraa p q)))
)

;; Skrå stykker (15-75 grader, mindst 100 mm) gemmes til at finde gavle
(defun fm:gem-skraa (p q / v)
  (if (>= (distance p q) 100.0)
    (progn
      (setq v (angle p q))
      (if (>= v pi) (setq v (- v pi)))                 ; retning 0..180 grader
      (if (or (and (> v (* pi (/ 15.0 180.0))) (< v (* pi (/ 75.0 180.0))))
              (and (> v (* pi (/ 105.0 180.0))) (< v (* pi (/ 165.0 180.0)))))
        (setq dseg (cons (list (list (car p) (cadr p)) (list (car q) (cadr q)) v) dseg)))))
)

;; ---------- Skifter ----------
;; Stenene i et skifte lagt sammen til sammenhængende stykker (xa xb)
(defun fm:intervaller (bokse / res)
  (foreach b (vl-sort bokse '(lambda (a c) (< (fm:x1 a) (fm:x1 c))))
    (if (and res (<= (fm:x1 b) (+ (cadr (car res)) *fm-fugemax*)))
      (setq res (cons (list (car (car res)) (max (cadr (car res)) (fm:x2 b))) (cdr res)))
      (setq res (cons (list (fm:x1 b) (fm:x2 b)) res))))
  (reverse res)
)

;; Skifter nedefra: liste af (y1 y2 stykker)
(defun fm:raekker (normal / rk cur)
  (foreach s (vl-sort normal '(lambda (a b) (< (fm:y1 a) (fm:y1 b))))
    (if (and cur (< (abs (- (fm:y1 s) (car cur))) (/ hs 2.0)))
      (setq cur (list (car cur) (max (cadr cur) (fm:y2 s)) (cons s (caddr cur))))
      (progn
        (if cur (setq rk (cons cur rk)))
        (setq cur (list (fm:y1 s) (fm:y2 s) (list s))))))
  (if cur (setq rk (cons cur rk)))
  (mapcar '(lambda (r) (list (car r) (cadr r) (fm:intervaller (caddr r)))) (reverse rk))
)

;; Ligger skifte j lige oven på skifte i?
(defun fm:naboer (i j)
  (and (>= i 0) (>= j 0) (nth i rows) (nth j rows)
       (< (- (car (nth j rows)) (car (nth i rows))) (* 1.5 hs)))
)

;; Huller mellem stykkerne i et skifte (bredere end 100 mm)
(defun fm:huller (row / res forrige)
  (foreach iv (caddr row)
    (if (and forrige (> (- (car iv) (cadr forrige)) 100.0))
      (setq res (cons (list (cadr forrige) (car iv)) res)))
    (setq forrige iv))
  (reverse res)
)

(defun fm:antal-skifter (ya yb / n m)
  (setq n 0)
  (foreach r rows
    (setq m (/ (+ (car r) (cadr r)) 2.0))
    (if (and (> m ya) (< m yb)) (setq n (1+ n))))
  n
)

(defun fm:skifte-tekst (ya yb) nil)   ; skiftetal vises ikke (for rodet)

;; ---------- Åbninger i murværket ----------
;; Et hul (mindst 400 mm bredt) der går igennem mindst 2 skifter med
;; samme kanter. Kanterne kommer med i målene under facaden, og højderne
;; sættes inde i åbningen: brystning, højde til overligger/vederlag og
;; evt. pilhøjde op til buens top.
(defun fm:aabninger (/ i j g ybund ytop mid top xm lv k a c ok)
  (setq i 0)
  (while (< i (length rows))
    (foreach g (fm:huller (nth i rows))
      ;; kun start af et hul (skiftet under har ikke samme hul)
      (if (not (and (fm:naboer (1- i) i)
                    (vl-some '(lambda (h) (and (< (abs (- (car h) (car g))) 1.0)
                                               (< (abs (- (cadr h) (cadr g))) 1.0)))
                             (fm:huller (nth (1- i) rows)))))
        (progn
          (setq j i)
          (while (and (fm:naboer j (1+ j))
                      (vl-some '(lambda (h) (and (< (abs (- (car h) (car g))) 1.0)
                                                 (< (abs (- (cadr h) (cadr g))) 1.0)))
                               (fm:huller (nth (1+ j) rows))))
            (setq j (1+ j)))
          (if (and (>= (- j i) 2) (>= (- (cadr g) (car g)) 400.0))
            (progn
              ;; bund: top af skiftet under, ellers bunden
              (setq ybund (if (fm:naboer (1- i) i) (cadr (nth (1- i) rows)) ymin))
              (if (< (- ybund ymin) *fm-bundzone*) (setq ybund ymin))
              ;; top: bund af skiftet over, ellers toppen af hullet
              (setq ytop (if (fm:naboer j (1+ j)) (car (nth (1+ j) rows)) (cadr (nth j rows))))
              ;; overligger/bue over hullet: første sten (eller bue-blok) over midten
              (setq mid (/ (+ (car g) (cadr g)) 2.0) top nil)
              (foreach s (append sten blokke)
                (if (and (< (fm:x1 s) mid) (> (fm:x2 s) mid) (>= (fm:y1 s) (- ytop 1.0))
                         (or (not top) (< (fm:y1 s) top)))
                  (setq top (fm:y1 s))))
              ;; intet murværk lige ovenover = mellemrum mellem to mure, ikke en åbning
              (if (or (not top) (> (- top ytop) (* 0.6 (- (cadr g) (car g)))))
                (setq top nil ok nil)
                (setq ok T))
              (if ok (progn
              (setq xs (append xs (list (car g) (cadr g))))
              (setq *fm-aab* (1+ *fm-aab*)))))))))
    (setq i (1+ i)))
)

;; Kanten (venstre = 'car, højre = 'cadr) af de stykker i skifte k der
;; overlapper stykket iv
(defun fm:kant-i (k iv fn / m)
  (if (and (>= k 0) (nth k rows))
    (progn
      (setq m (vl-remove-if-not
                '(lambda (u) (and (< (car u) (- (cadr iv) 1.0)) (> (cadr u) (+ (car iv) 1.0))))
                (caddr (nth k rows))))
      (if m (if (eq fn 'car) (apply 'min (mapcar 'car m)) (apply 'max (mapcar 'cadr m))))))
)

;; Står muren lige (samme kant) i de 2 skifter under skifte i?
(defun fm:lige-under (i iv kant fn / k1 k2)
  (and (fm:naboer (- i 2) (1- i)) (fm:naboer (1- i) i)
       (setq k1 (fm:kant-i (1- i) iv fn))
       (setq k2 (fm:kant-i (- i 2) iv fn))
       (< (abs (- k1 kant)) 1.0) (< (abs (- k2 kant)) 1.0))
)

;; ---------- Udkragning (gesims) ----------
;; Hvor et skifte stikker 3-150 mm ud over skiftet under, sættes et lille mål.
;; Kun hvor muren står lige nedenunder (ikke i gavl og ved buer).
;; Gesimsens bund og top gemmes som højder.
(defun fm:udkragning (/ i up lo m lowL lowR d trin)
  (setq i 0)
  (while (< (1+ i) (length rows))
    (if (fm:naboer i (1+ i))
      (progn
        (setq up (nth (1+ i) rows) lo (nth i rows))
        (foreach iv (caddr up)
          (setq m (vl-remove-if-not
                    '(lambda (u) (and (< (car u) (- (cadr iv) 1.0)) (> (cadr u) (+ (car iv) 1.0))))
                    (caddr lo)))
          (if m
            (progn
              (setq lowL (apply 'min (mapcar 'car m))
                    lowR (apply 'max (mapcar 'cadr m)))
              ;; venstre side
              (setq d (- lowL (car iv)))
              (if (and (>= d 3.0) (<= d *fm-udkrag*) (not (fm:tilbage (+ i 2) iv lowL 'car))
                       (or (member i *fm-trin*) (fm:lige-under i iv lowL 'car)))
                (setq trin (cons (list (1+ i) 'car (car iv) lowL) trin)
                      *fm-trin* (cons (1+ i) *fm-trin*)))
              ;; højre side
              (setq d (- (cadr iv) lowR))
              (if (and (>= d 3.0) (<= d *fm-udkrag*) (not (fm:tilbage (+ i 2) iv lowR 'cadr))
                       (or (member i *fm-trin*) (fm:lige-under i iv lowR 'cadr)))
                (setq trin (cons (list (1+ i) 'cadr (cadr iv) lowR) trin)
                      *fm-trin* (cons (1+ i) *fm-trin*))))))))
    (setq i (1+ i)))
  (fm:gesims-maal (reverse trin))
)

;; Trin = (skifte side yderkant inderkant). Trin i skifter lige over
;; hinanden i samme hjørne lægges sammen til ÉT mål: fra muren ud til
;; gesimsens yderste kant, sat lige under gesimsen.
(defun fm:gesims-maal (trin / grupper g inder yder y)
  (foreach tr trin
    (if (setq g (vl-some '(lambda (gr)
                            (if (and (eq (cadr (car gr)) (cadr tr))
                                     (= (car (car gr)) (1- (car tr)))
                                     (fm:ca (caddr (car gr)) (cadddr tr)))
                              gr))
                         grupper))
      (setq grupper (subst (cons tr g) g grupper))
      (setq grupper (cons (list tr) grupper))))
  (foreach gr grupper
    (setq inder (cadddr (last gr))                 ; murens kant under gesimsen
          yder  (caddr (car gr))                   ; gesimsens yderste kant
          y     (car (nth (car (last gr)) rows)))  ; underkant af gesimsen
    (fm:maal (list inder y) (list yder y)
             (list (/ (+ inder yder) 2.0) (- y (* 0.6 afst))) 0.0 nil))
)

;; Går kanten tilbage i skiftet ovenover? (tandet kant, ikke en gesims)
(defun fm:tilbage (k iv gl fn / m)
  (if (fm:naboer (1- k) k)
    (progn
      (setq m (vl-remove-if-not
                '(lambda (u) (and (< (car u) (- (cadr iv) 1.0)) (> (cadr u) (+ (car iv) 1.0))))
                (caddr (nth k rows))))
      (and m (vl-some '(lambda (u) (< (abs (- (apply fn (list u)) gl)) 1.0)) m))))
)


;; ---------- Gavle ud fra de skrå tagkanter ----------
;; Finder op til 5 gavle. For hver: højde, taghældning og tagkantens bredde.
;; Findes ingen skrå streger, prøves der med skifterne (fm:gavl).
(defun fm:gavle (/ rest n g)
  (setq rest (vl-remove-if-not
               '(lambda (sg) (and (>= (min (car (car sg)) (car (cadr sg))) (- bx1 1000.0))
                                  (<= (max (car (car sg)) (car (cadr sg))) (+ bx2 1000.0))))
               dseg)
        n 0)
  (while (and rest (< n 5) (setq g (fm:gavl-streger rest)))
    (setq n (1+ n)
          rest (vl-remove-if
                 '(lambda (sg) (and (>= (min (car (car sg)) (car (cadr sg))) (- (car g) 1.0))
                                    (<= (max (car (car sg)) (car (cadr sg))) (+ (cadr g) 1.0))))
                 rest)))
  (if (= n 0) (fm:gavl))
)

(defun fm:gavl-streger (lst / pts top ax lv hv lp rp yf xa xb inder vink o c1 grad)
  (setq pts (apply 'append (mapcar '(lambda (sg) (list (car sg) (cadr sg))) lst))
        top (apply 'max (mapcar 'cadr pts))
        ax  (fm:median (mapcar 'car (vl-remove-if-not '(lambda (q) (> (cadr q) (- top 5.0))) pts))))
  ;; stigende mod højre (venstre tagflade) og faldende mod højre (højre tagflade)
  (setq lv (vl-remove-if-not '(lambda (sg) (and (< (caddr sg) (/ pi 2.0))
                                                (<= (max (car (car sg)) (car (cadr sg))) (+ ax 5.0))))
                             lst)
        hv (vl-remove-if-not '(lambda (sg) (and (> (caddr sg) (/ pi 2.0))
                                                (>= (min (car (car sg)) (car (cadr sg))) (- ax 5.0))))
                             lst))
  (if (and lv hv)
    (progn
      (setq lp (apply 'append (mapcar '(lambda (sg) (list (car sg) (cadr sg))) lv))
            rp (apply 'append (mapcar '(lambda (sg) (list (car sg) (cadr sg))) hv))
            yf (apply 'min (mapcar 'cadr lp))
            xa (apply 'min (mapcar 'car (vl-remove-if-not '(lambda (q) (< (cadr q) (+ yf 5.0))) lp)))
            xb (apply 'max (mapcar 'car (vl-remove-if-not
                                           '(lambda (q) (< (cadr q) (+ (apply 'min (mapcar 'cadr rp)) 5.0)))
                                           rp))))
      (if (and (>= (- top yf) 300.0) (>= (- ax xa) 300.0))
        (progn
          ;; gavlens højde (til venstre for gavlen)
          (fm:maal (list xa yf) (list ax top) (list (- xa (* 0.6 afst)) (/ (+ yf top) 2.0)) (/ pi 2.0) nil)
          ;; taghældning ved venstre fod
          (setq vink (atan (- top yf) (- ax xa))
                grad (rtos (/ (* vink 180.0) pi) 2 0)
                c1   (list xa yf))
          (setq o (vl-catch-all-apply 'vla-AddDimAngular
                    (list spc (fm:3d c1) (fm:3d (polar c1 0.0 (* 0.5 (- ax xa))))
                          (fm:3d (list ax top)) (fm:3d (polar c1 (/ vink 2.0) (* 0.35 (- ax xa)))))))
          (if (not (vl-catch-all-error-p o))
            (progn
              (vla-put-Layer o *fm-lagnavn*)
              (vla-put-TextOverride o (strcat grad "%%d"))
              (setq *fm-antal* (1+ *fm-antal*))))
          ;; tagkantens bredde ved foden: fra yderkanten til den inderste skrå kant
          (setq inder (vl-remove-if-not '(lambda (q) (and (< (cadr q) (+ yf 20.0)) (> (car q) (+ xa 20.0))
                                                          (< (car q) (+ xa 600.0))))
                                        lp))
          (if inder
            (progn
              (setq inder (apply 'max (mapcar 'car inder)))
              (fm:maal (list xa yf) (list inder yf)
                       (list (/ (+ xa inder) 2.0) (+ yf (* 0.5 afst))) 0.0 nil)))
          (princ (strcat "\nGavl fundet: højde " (fm:tal (- top yf) 0) " mm, hældning " grad " grader."))
          (list xa xb)))))
)

;; ---------- Gavl ----------
;; Stykket i skifte k der dækker x
(defun fm:iv-ved (k x / ivs v h)
  (setq ivs (caddr (nth k rows)))
  (cond
    ((car (vl-remove-if-not '(lambda (u) (and (<= (car u) x) (>= (cadr u) x))) ivs)))
    ;; hul lige ved x: brug stykkerne på begge sider, hvis hullet er lille
    ((and (setq v (vl-remove-if-not '(lambda (u) (< (cadr u) x)) ivs))
          (setq h (vl-remove-if-not '(lambda (u) (> (car u) x)) ivs))
          (setq v (last v) h (car h))
          (< (- (car h) (cadr v)) 600.0))
     (list (car v) (cadr h))))
)

;; Finder den højeste gavl (skifter der bliver smallere op mod toppen)
;; og sætter: gavlens højde, taghældning og bredden på tagkanten ved foden.
;; Returnerer gavlfodens højde, eller nil hvis der ingen gavl er.
(defun fm:gavl (/ top ts ax k w nw fod n gs xa xb yf fodiv iv1 vink o c1)
  (if (and sten rows)
    (progn
      (setq top (apply 'max (mapcar 'fm:y2 sten))
            ts  (vl-remove-if-not '(lambda (s) (> (fm:y2 s) (- top 1.0))) sten)
            ax  (/ (apply '+ (mapcar '(lambda (s) (/ (+ (fm:x1 s) (fm:x2 s)) 2.0)) ts))
                   (length ts))
            k   (1- (length rows))
            n   0)
      ;; øverste skifte midt i gavlen
      (while (and (>= k 0) (not (fm:iv-ved k ax))) (setq k (1- k)))
      (if (>= k 0)
        (progn
          ;; ned gennem gavlen, så længe skifterne bliver bredere
          (setq w (- (cadr (fm:iv-ved k ax)) (car (fm:iv-ved k ax))) fod k)
          (while (and (fm:naboer (1- fod) fod) (fm:iv-ved (1- fod) ax)
                      (> (setq nw (- (cadr (fm:iv-ved (1- fod) ax)) (car (fm:iv-ved (1- fod) ax))))
                         (+ w 20.0)))
            (setq fod (1- fod) w nw n (1+ n)))))
      ;; en gavl har mindst 3 skifter der bliver smallere
      (if (and (>= k 0) (>= n 3))
        (progn
          (setq fodiv (fm:iv-ved fod ax)
                yf    (cadr (nth fod rows))                 ; gavlens fod = top af det bredeste skifte
                gs    (vl-remove-if-not
                        '(lambda (s) (and (>= (fm:y1 s) (- yf hs))
                                          (> (/ (+ (fm:x1 s) (fm:x2 s)) 2.0) (- (car fodiv) 300.0))
                                          (< (/ (+ (fm:x1 s) (fm:x2 s)) 2.0) (+ (cadr fodiv) 300.0))))
                        sten)
                xa    (apply 'min (mapcar 'fm:x1 gs))
                xb    (apply 'max (mapcar 'fm:x2 gs))
                ax    (/ (+ xa xb) 2.0))
          ;; gavlens højde (til venstre for gavlen)
          (fm:maal (list xa yf) (list ax top) (list (- xa (* 0.6 afst)) (/ (+ yf top) 2.0)) (/ pi 2.0)
                   (fm:skifte-tekst yf top))
          ;; taghældning ved venstre fod
          (setq vink (atan (- top yf) (- ax xa))
                c1   (list xa yf))
          (setq o (vl-catch-all-apply 'vla-AddDimAngular
                    (list spc (fm:3d c1) (fm:3d (polar c1 0.0 (* 0.5 (- ax xa))))
                          (fm:3d (list ax top)) (fm:3d (polar c1 (/ vink 2.0) (* 0.35 (- ax xa)))))))
          (if (not (vl-catch-all-error-p o))
            (progn
              (vla-put-Layer o *fm-lagnavn*)
              (vla-put-TextOverride o (strcat (rtos (/ (* vink 180.0) pi) 2 0) "%%d"))
              (setq *fm-antal* (1+ *fm-antal*))))
          ;; tagkantens bredde ved foden: fra gavlens yderkant til første gavlskifte
          (if (and (fm:naboer fod (1+ fod)) (setq iv1 (fm:iv-ved (1+ fod) ax))
                   (> (- (car iv1) xa) 20.0))
            (fm:maal (list xa yf) (list (car iv1) yf)
                     (list (/ (+ xa (car iv1)) 2.0) (+ yf (* 0.5 afst))) 0.0 nil))
          (princ (strcat "\nGavl fundet: højde " (fm:tal (- top yf) 0) " mm, hældning "
                         (rtos (/ (* vink 180.0) pi) 2 0) " grader."))
          yf))))
)

;; ---------- Kædemål ----------
;; Vandret kæde gennem punkterne pts (x), målt ved y = ym, målelinje i y = yd.
;; tfn = nil eller funktion (i a c) der giver teksten under målet.
(defun fm:kaede-v (pts ym yd tfn / i a c)
  (setq i 0)
  (while (< i (1- (length pts)))
    (setq a (nth i pts) c (nth (1+ i) pts))
    (fm:maal (list a ym) (list c ym) (list (/ (+ a c) 2.0) yd) 0.0
             (if tfn (apply tfn (list i a c))))
    (setq i (1+ i)))
)
;; Lodret kæde gennem punkterne pts (y), målt ved x = xm, målelinje i x = xd.
(defun fm:kaede-l (pts xm xd tfn / i a c)
  (setq i 0)
  (while (< i (1- (length pts)))
    (setq a (nth i pts) c (nth (1+ i) pts))
    (fm:maal (list xm a) (list xm c) (list xd (/ (+ a c) 2.0)) (/ pi 2.0)
             (if tfn (apply tfn (list i a c))))
    (setq i (1+ i)))
)

;; Almindelig tekst i laget Maal
(defun fm:tekst (p txt / o)
  (setq o (vl-catch-all-apply 'vla-AddText (list spc txt (fm:3d p) th)))
  (if (not (vl-catch-all-error-p o)) (vla-put-Layer o *fm-lagnavn*))
)

;; ---------- Fliser (enheder i gitter) ----------
;; Står enhederne i lige rækker OG lige kolonner (ikke i forbandt)?
(defun fm:gitter-p (/ cx cy)
  (if (>= (length sten) 4)
    (progn
      (setq cx (fm:unikke (mapcar 'fm:x1 sten) 1.0)
            cy (fm:unikke (mapcar 'fm:y1 sten) 1.0))
      (and (>= (length cx) 2) (>= (length cy) 2)
           (<= (* (length cx) (length cy)) (* 1.3 (length sten))))))
)

;; Bredden af de enheder der starter ved v (fn = 'fm:x1 ...)
(defun fm:kant-str (v fn sz)
  (apply 'max (cons 0.0 (mapcar sz (vl-remove-if-not '(lambda (u) (fm:ca (apply fn (list u)) v)) sten))))
)

;; Mål på et flisefelt: kæde med tilskåret flise | hele fliser | tilskåret flise,
;; samlet mål og en tekst med flisestørrelse, fuge og antal.
(defun fm:fliser (/ x0 x1 y0 y1 cx cy mw mh fw lw fh lh fgx fgy px py nk nr skaar)
  (setq x0 (apply 'min (mapcar 'fm:x1 sten)) x1 (apply 'max (mapcar 'fm:x2 sten))
        y0 (apply 'min (mapcar 'fm:y1 sten)) y1 (apply 'max (mapcar 'fm:y2 sten))
        cx (fm:unikke (mapcar 'fm:x1 sten) 1.0)
        cy (fm:unikke (mapcar 'fm:y1 sten) 1.0)
        mw (if modal (car modal)  (fm:median (mapcar 'fm:b sten)))
        mh (if modal (cadr modal) (fm:median (mapcar 'fm:h sten)))
        fw (fm:kant-str x0 'fm:x1 'fm:b)  lw (fm:kant-str x1 'fm:x2 'fm:b)
        fh (fm:kant-str y0 'fm:y1 'fm:h)  lh (fm:kant-str y1 'fm:y2 'fm:h)
        fgx (if (> (length cx) 2) (max 0.0 (- (fm:median (mapcar '- (cddr cx) (cdr (reverse (cdr (reverse cx)))))) mw))
                0.0)
        fgy (if (> (length cy) 2) (max 0.0 (- (fm:median (mapcar '- (cddr cy) (cdr (reverse (cdr (reverse cy)))))) mh))
                0.0)
        nk (length cx) nr (length cy)
        skaar (length (vl-remove-if '(lambda (u) (or (and (fm:ca (fm:b u) mw) (fm:ca (fm:h u) mh))
                                                     (and (fm:ca (fm:b u) mh) (fm:ca (fm:h u) mw))))
                                    sten)))
  (setq *fm-raekkeord* "rækker")
  ;; vandret: tilskåret | hele | tilskåret
  (setq px (if (> nk 2) (fm:unikke (list x0 (+ x0 fw) (- x1 lw) x1) 1.0) (list x0 x1)))
  (fm:kaede-v px y0 (- y0 afst)
              '(lambda (i a c)
                 (cond ((and (= i 0) (> (length px) 2) (< fw (- mw 1.0))) "tilskåret")
                       ((and (= i (- (length px) 2)) (> (length px) 2) (< lw (- mw 1.0))) "tilskåret")
                       ((and (> (length px) 3) (= i 1)) (strcat (itoa (- nk 2)) " fliser")))))
  (if (> (length px) 2) (fm:maal (list x0 y0) (list x1 y0) (list (/ (+ x0 x1) 2.0) (- y0 (* 2.0 afst))) 0.0
                                 (strcat (itoa nk) " fliser")))
  ;; lodret: tilskåret | hele | tilskåret
  (setq py (if (> nr 2) (fm:unikke (list y0 (+ y0 fh) (- y1 lh) y1) 1.0) (list y0 y1)))
  (fm:kaede-l py x0 (- x0 afst)
              '(lambda (i a c)
                 (cond ((and (= i 0) (> (length py) 2) (< fh (- mh 1.0))) "tilskåret")
                       ((and (= i (- (length py) 2)) (> (length py) 2) (< lh (- mh 1.0))) "tilskåret")
                       ((and (> (length py) 3) (= i 1)) (strcat (itoa (- nr 2)) " fliser")))))
  (if (> (length py) 2) (fm:maal (list x0 y0) (list x0 y1) (list (- x0 (* 2.0 afst)) (/ (+ y0 y1) 2.0)) (/ pi 2.0)
                                 (strcat (itoa nr) " fliser")))
  ;; tekst under målene: flisestørrelse, fuge og antal
  (fm:tekst (list x0 (- y0 (* 3.0 afst)))
            (strcat "Fliser " (fm:tal mw 0) " x " (fm:tal mh 0) " mm, fuge " (fm:tal (max fgx fgy) 1)
                    " mm - " (itoa nk) " x " (itoa nr) " = " (itoa (length sten)) " stk"
                    (if (> skaar 0) (strcat " (" (itoa skaar) " tilskårne)") "")))
  (princ (strcat "\nFlisefelt: " (itoa nk) " x " (itoa nr) " fliser, " (itoa skaar) " tilskårne."))
)

;; ---------- Kun streger (ingen sten/fliser) ----------
;; Omrids: alle lange lodrette og vandrette streger giver kæder under og til venstre.
(defun fm:stregmaal (/ px py x0 y0)
  (setq *fm-raekkeord* "rækker"
        x0 (apply 'min (mapcar 'fm:x1 alle))
        y0 (apply 'min (mapcar 'fm:y1 alle)))
  (foreach v vseg (if (>= (- (caddr v) (cadr v)) 300.0) (setq px (cons (car v) px))))
  (foreach hz hseg (if (>= (- (caddr hz) (cadr hz)) 300.0) (setq py (cons (car hz) py))))
  (setq px (fm:unikke (append px (list x0 (apply 'max (mapcar 'fm:x2 alle)))) *fm-kantfri*)
        py (fm:unikke (append py (list y0 (apply 'max (mapcar 'fm:y2 alle)))) *fm-kantfri*))
  (fm:kaede-v px y0 (- y0 afst) nil)
  (if (> (length px) 2) (fm:maal (list (car px) y0) (list (last px) y0)
                                 (list (/ (+ (car px) (last px)) 2.0) (- y0 (* 2.0 afst))) 0.0 nil))
  (fm:kaede-l py x0 (- x0 afst) nil)
  (if (> (length py) 2) (fm:maal (list x0 (car py)) (list x0 (last py))
                                 (list (- x0 (* 2.0 afst)) (/ (+ (car py) (last py)) 2.0)) (/ pi 2.0) nil))
)

;; ---------- Rundt murværk ----------
;; Sten set oppefra er (drejede) firkanter eller kileformede (huggede) sten.
;; Rekt = (cx cy lang kort vinkel ename inderbredde)
;; vinkel = retningen af den lange side. inderbredde = den smalle ende på en
;; kileformet sten (nil på en almindelig firkantet sten).

(defun fm:midt (p q) (list (/ (+ (car p) (car q)) 2.0) (/ (+ (cadr p) (cadr q)) 2.0)))

;; Kileformet sten: p1-p2 og p3-p0 er parallelle (inder- og yderside) og
;; p0-p1 og p2-p3 er lige lange. Giver (cx cy lang kort vinkel inderbredde) eller nil.
(defun fm:kile (p0 p1 p2 p3 / b1 b2 kryds m1 m2 h bred)
  (setq b1 (distance p1 p2) b2 (distance p3 p0) bred (max b1 b2))
  (if (and (> (min b1 b2) 5.0)
           (> (abs (- b1 b2)) 1.0)
           (< (abs (- (distance p0 p1) (distance p2 p3))) 1.0))
    (progn
      ;; parallelle sider: krydsproduktet af retningerne er ca. 0
      (setq kryds (- (* (- (car p2) (car p1)) (- (cadr p0) (cadr p3)))
                     (* (- (cadr p2) (cadr p1)) (- (car p0) (car p3))))
            m1 (fm:midt p1 p2)
            m2 (fm:midt p3 p0)
            h  (distance m1 m2))
      (if (and (< (abs kryds) (* 0.002 b1 b2)) (>= h 15.0))
        (list (/ (+ (car m1) (car m2)) 2.0) (/ (+ (cadr m1) (cadr m2)) 2.0)
              (max h bred) (min h bred)
              (if (>= h bred) (angle m2 m1) (angle p1 p2))
              (min b1 b2)))))
)

;; Lukket polylinje med 4 hjørner: firkant med rette hjørner (må gerne være
;; drejet) eller kileformet sten
(defun fm:rekt-poly (ed / pts p0 p1 p2 p3 s1 s2 k)
  (if (and (= 1 (logand 1 (cdr (assoc 70 ed))))
           (= 4 (cdr (assoc 90 ed)))
           (not (vl-some '(lambda (p) (and (= (car p) 42) (/= 0.0 (cdr p)))) ed)))
    (progn
      (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (p) (= (car p) 10)) ed))
            p0 (nth 0 pts) p1 (nth 1 pts) p2 (nth 2 pts) p3 (nth 3 pts)
            s1 (distance p0 p1) s2 (distance p1 p2))
      (cond
        ((and (>= (min s1 s2) 15.0)
              (< (abs (- s1 (distance p2 p3))) 1.0)
              (< (abs (- s2 (distance p3 p0))) 1.0)
              (< (abs (- (distance p0 p2) (distance p1 p3))) 1.0))
         (list (/ (+ (car p0) (car p2)) 2.0) (/ (+ (cadr p0) (cadr p2)) 2.0)
               (max s1 s2) (min s1 s2)
               (if (>= s1 s2) (angle p0 p1) (angle p1 p2))
               (cdr (assoc -1 ed)) nil))
        ((setq k (cond ((fm:kile p0 p1 p2 p3)) ((fm:kile p1 p2 p3 p0))))
         (list (nth 0 k) (nth 1 k) (nth 2 k) (nth 3 k) (nth 4 k) (cdr (assoc -1 ed)) (nth 5 k))))))
)

;; Sten-blok. Er den drejet skævt, måles en kopi uden drejning (kopien slettes igen).
(defun fm:rekt-blok (e / obj rot ip b cp mn mx m w h)
  (setq obj (vlax-ename->vla-object e)
        rot (vla-get-Rotation obj))
  (if (< (abs (- (/ rot (/ pi 2.0)) (fix (+ 0.5 (/ rot (/ pi 2.0)))))) 1e-4)
    ;; lige (0, 90, 180 eller 270 grader): kassen passer
    (if (setq b (fm:boks e))
      (list (/ (+ (fm:x1 b) (fm:x2 b)) 2.0) (/ (+ (fm:y1 b) (fm:y2 b)) 2.0)
            (max (fm:b b) (fm:h b)) (min (fm:b b) (fm:h b))
            (if (>= (fm:b b) (fm:h b)) 0.0 (/ pi 2.0)) e nil))
    (progn
      (setq ip (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))
            ip (list (car ip) (cadr ip))
            cp (vl-catch-all-apply 'vla-Copy (list obj)))
      (if (not (vl-catch-all-error-p cp))
        (progn
          (vla-put-Rotation cp 0.0)
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list cp 'mn 'mx))))
            (setq mn (vlax-safearray->list mn) mx (vlax-safearray->list mx)
                  w  (- (car mx) (car mn)) h (- (cadr mx) (cadr mn))
                  m  (list (/ (+ (car mn) (car mx)) 2.0) (/ (+ (cadr mn) (cadr mx)) 2.0))
                  m  (polar ip (+ rot (angle ip m)) (distance ip m))))
          (vla-Delete cp)
          (if (and w (>= (min w h) 15.0))
            (list (car m) (cadr m) (max w h) (min w h) (if (>= w h) rot (+ rot (/ pi 2.0))) e nil))))))
)

;; Den cirkel der passer bedst gennem punkterne: (cx cy r). nil hvis de ligger på linje.
(defun fm:cirkel (pts / n mx my sxx syy sxy sxz syz sz x y z det dd ee)
  (setq n  (float (length pts))
        mx (/ (apply '+ (mapcar 'car pts)) n)
        my (/ (apply '+ (mapcar 'cadr pts)) n)
        sxx 0.0 syy 0.0 sxy 0.0 sxz 0.0 syz 0.0 sz 0.0)
  (foreach p pts
    (setq x (- (car p) mx) y (- (cadr p) my) z (+ (* x x) (* y y))
          sxx (+ sxx (* x x)) syy (+ syy (* y y)) sxy (+ sxy (* x y))
          sxz (+ sxz (* x z)) syz (+ syz (* y z)) sz (+ sz z)))
  (setq det (- (* sxx syy) (* sxy sxy)))
  (if (> det (* 1e-6 (+ sxx syy) (+ sxx syy)))
    (progn
      (setq dd (/ (+ (* (- sxz) syy) (* sxy syz)) det)
            ee (/ (+ (* (- syz) sxx) (* sxz sxy)) det))
      (list (- mx (/ dd 2.0)) (- my (/ ee 2.0))
            (sqrt (max 0.0 (+ (/ (* dd dd) 4.0) (/ (* ee ee) 4.0) (/ sz n)))))))
)

;; Fugen mellem to sten i afstanden rad fra centrum.
;; w = stenens bredde langs muren, tv = vinklen fra sten til sten.
(defun fm:fuge (rad w tv) (- (* 2.0 rad (sin (/ tv 2.0))) (* w (cos (/ tv 2.0)))))

;; Finder en rund mur: mindst 5 ens sten, der ligger i en cirkel og peger ind
;; mod centrum. Giver (centrum type bredde dybde r-inder a r-yder antal deling
;; hel start vinkel inderbredde) eller nil.
;; bredde = stenens mål langs muren (yderst), dybde = murens tykkelse, a = afstand
;; til stenenes yderside, r-yder = til de yderste hjørner (snoren/skabelonen),
;; inderbredde = stenens bredde inderst (mindre end bredden, hvis stenene er hugget).
(defun fm:rund-find (rekter / tael s bedst grp cf c nk nl v res ang difs g n
                              tv hel a0 vv typ w d dc a ri rr psi wi)
  ;; hyppigste stenstørrelse
  (foreach r rekter
    (setq s (list (fix (+ 0.5 (caddr r))) (fix (+ 0.5 (cadddr r)))))
    (if (setq bedst (assoc s tael))
      (setq tael (subst (cons s (1+ (cdr bedst))) bedst tael))
      (setq tael (cons (cons s 1) tael))))
  (setq bedst nil)
  (foreach t1 tael (if (or (not bedst) (> (cdr t1) (cdr bedst))) (setq bedst t1)))
  (if (and bedst (>= (cdr bedst) 5))
    (setq grp (vl-remove-if-not '(lambda (r) (and (fm:ca (caddr r) (car (car bedst)))
                                                 (fm:ca (cadddr r) (cadr (car bedst)))))
                                rekter)
          cf  (fm:cirkel (mapcar '(lambda (r) (list (car r) (cadr r))) grp))))
  ;; alle stenene skal ligge på cirklen
  (if (and cf (> (caddr cf) 200.0) (< (caddr cf) 50000.0)
           (setq c (list (car cf) (cadr cf)))
           (vl-every '(lambda (r) (< (abs (- (distance c (list (car r) (cadr r))) (caddr cf)))
                                     (+ 15.0 (* 0.01 (caddr cf)))))
                     grp))
    (progn
      ;; lang side ind mod centrum = kopper, lang side langs muren = løbere
      (setq nk 0 nl 0)
      (foreach r grp
        (setq v (rem (abs (- (angle c (list (car r) (cadr r))) (nth 4 r))) pi))
        (if (> v (/ pi 2.0)) (setq v (- pi v)))
        (cond ((< v (/ pi 18.0)) (setq nk (1+ nk)))
              ((> v (/ (* 8.0 pi) 18.0)) (setq nl (1+ nl)))))
      (if (>= (max nk nl) (* 0.9 (length grp)))
        (progn
          ;; samme sten tegnet to gange tælles én gang
          (foreach r grp
            (if (not (vl-some '(lambda (q) (< (distance (list (car q) (cadr q)) (list (car r) (cadr r))) 1.0))
                              res))
              (setq res (cons r res))))
          (setq n    (length res)
                ang  (vl-sort (mapcar '(lambda (r) (angle c (list (car r) (cadr r)))) res) '<)
                difs (append (mapcar '- (cdr ang) ang)
                             (list (- (+ (car ang) (* 2.0 pi)) (last ang))))
                g    (apply 'max difs)
                typ  (if (> nk nl) "kopper" "løbere")
                w    (if (> nk nl) (cadddr (car res)) (caddr (car res)))
                d    (if (> nk nl) (caddr (car res)) (cadddr (car res)))
                dc   (fm:median (mapcar '(lambda (r) (distance c (list (car r) (cadr r)))) res))
                a    (+ dc (/ d 2.0))
                ri   (- dc (/ d 2.0))
                rr   (sqrt (+ (* a a) (* 0.25 w w)))
                psi  (atan (* 0.5 w) a)
                wi   (fm:median (mapcar '(lambda (r) (if (nth 6 r) (nth 6 r) w)) res))
                hel  (< g (* 1.8 (fm:median difs))))
          (if hel
            (setq tv (/ (* 2.0 pi) n) a0 0.0 vv (* 2.0 pi))
            ;; bue: fra stenen efter det store hul rundt til stenen før det
            (setq tv (/ (- (* 2.0 pi) g) (1- n))
                  vv (+ (- (* 2.0 pi) g) (* 2.0 psi))
                  a0 (- (nth (rem (1+ (vl-position g difs)) n) ang) psi)))
          ;; en bue skal gå over mindst 20 grader
          (if (or hel (>= (- (* 2.0 pi) g) (/ pi 9.0)))
            (list c typ w d ri a rr n tv hel a0 vv wi))))))
)

;; Mål og tekst på en rund mur (data fra fm:rund-find)
(defun fm:rund-maal (data / c typ w d ri a rr n tv hel a0 vv wi fi fo am o lst k x0 y0 grad lin)
  (mapcar 'set '(c typ w d ri a rr n tv hel a0 vv wi) data)
  (setq fi   (fm:fuge ri wi tv)
        fo   (fm:fuge a w tv)
        grad (fm:tal (/ (* vv 180.0) pi) 0))
  (if hel
    (progn
      ;; radius = snoren fra centrum ud til ydersiden
      (fm:gem-dim (vl-catch-all-apply 'vla-AddDimRadial
                    (list spc (fm:3d c) (fm:3d (polar c (/ pi 4.0) rr)) (* 0.6 afst)))
                  "R<>")
      ;; diameter indvendig gennem centrum og udvendig under muren
      (if (setq o (fm:maal (polar c pi ri) (polar c 0.0 ri) c 0.0 nil))
        (vla-put-TextOverride o "%%c<>\\Xindvendig"))
      (if (setq o (fm:maal (polar c pi rr) (polar c 0.0 rr) (polar c (* 1.5 pi) (+ rr afst)) 0.0 nil))
        (vla-put-TextOverride o "%%c<>\\Xudvendig"))
      (setq x0 (- (car c) rr)
            y0 (- (cadr c) rr (* 2.0 afst))))
    (progn
      (setq am (+ a0 (/ vv 2.0)))
      ;; radius yderst og inderst
      (fm:gem-dim (vl-catch-all-apply 'vla-AddDimRadial
                    (list spc (fm:3d c) (fm:3d (polar c (+ a0 (* 0.7 vv)) rr)) (* 0.6 afst)))
                  "R<> udv.")
      (if (setq o (fm:gem-dim (vl-catch-all-apply 'vla-AddDimRadial
                                (list spc (fm:3d c) (fm:3d (polar c (+ a0 (* 0.3 vv)) ri)) 0.0))
                              "R<> indv."))
        (vl-catch-all-apply 'vla-put-TextPosition
                            (list o (fm:3d (polar c (+ a0 (* 0.3 vv)) (* 0.6 ri))))))
      ;; vinklen buen spænder over
      (fm:gem-dim (vl-catch-all-apply 'vla-AddDimAngular
                    (list spc (fm:3d c) (fm:3d (polar c a0 rr)) (fm:3d (polar c (+ a0 vv) rr))
                          (fm:3d (polar c am (* 0.35 ri)))))
                  (strcat grad "%%d"))
      ;; buelængden yderst
      (fm:gem-dim (vl-catch-all-apply 'vla-AddDimArc
                    (list spc (fm:3d c) (fm:3d (polar c a0 rr)) (fm:3d (polar c (+ a0 vv) rr))
                          (fm:3d (polar c am (+ rr (* 0.6 afst))))))
                  nil)
      ;; teksten sættes under det hele
      (setq lst (list c) k 0)
      (repeat 25
        (setq lst (cons (polar c (+ a0 (* k (/ vv 24.0))) (+ rr afst)) lst) k (1+ k)))
      (setq x0 (apply 'min (mapcar 'car lst))
            y0 (- (apply 'min (mapcar 'cadr lst)) afst))))
  ;; tekst: sten, fuger og omkreds/buelængde (formlerne fra rundt murværk)
  (setq lin (list
              (strcat "Rund mur: " typ " " (fm:tal (max w d) 0) " x " (fm:tal (min w d) 0) " mm, "
                      (itoa n) " sten pr. skifte, murtykkelse " (fm:tal d 0) " mm")
              (strcat "Fuge " (fm:tal fi 0) " mm inderst og " (fm:tal fo 0) " mm yderst"
                      " - næste skifte drejes en halv sten (" (fm:tal (/ (* tv 90.0) pi) 1) " grader)")
              (if hel
                (strcat "Omkreds: udv. 2 x " *fm-pi* " x " (fm:tal rr 0) " = " (fm:tal (* 2.0 pi rr) 0)
                        " mm, indv. 2 x " *fm-pi* " x " (fm:tal ri 0) " = " (fm:tal (* 2.0 pi ri) 0) " mm")
                (strcat "Buelængde: udv. 2 x " *fm-pi* " x " (fm:tal rr 0) " x " grad " / 360 = "
                        (fm:tal (* rr vv) 0) " mm, indv. 2 x " *fm-pi* " x " (fm:tal ri 0) " x " grad
                        " / 360 = " (fm:tal (* ri vv) 0) " mm"))))
  (if (< wi (- w 0.5))
    (setq lin (append lin (list (strcat "Stenene hugges kileformede: " (fm:tal wi 0) " mm inderst og "
                                        (fm:tal w 0) " mm yderst (" (fm:tal (- w wi) 0)
                                        " mm hugges af hver sten)")))))
  (if (> fo (+ *rund-fugemaks* 0.5))
    (setq lin (append lin (list (strcat "OBS: fugen yderst er over " (fm:tal *rund-fugemaks* 0)
                                        " mm - vælg større radius" (if (= typ "løbere") ", kopper" "")
                                        " eller hug stenene kileformede")))))
  (if (< fi (- *rund-fugemin* 0.5))
    (setq lin (append lin (list (strcat "OBS: fugen inderst er under " (fm:tal *rund-fugemin* 0)
                                        " mm - der er for mange sten")))))
  (foreach s lin
    (fm:tekst (list x0 y0) s)
    (setq y0 (- y0 (* 1.8 th))))
  (princ (strcat "\nRund mur (" (if hel "hel cirkel" (strcat "bue " grad " grader")) "): "
                 (itoa n) " " typ ", radius " (fm:tal rr 0) " mm, fuge "
                 (fm:tal fi 0) "-" (fm:tal fo 0) " mm."))
)

;; ---------- Murværk ----------
(defun fm:murvaerk (/ fx1 fx2 fb fg)
  (setq *fm-raekkeord* "skifter")
  ;; facadens udstrækning: sten, blokke og lodrette streger inden for stenene
  (setq fx1 (apply 'min (mapcar 'fm:x1 sten)) fx2 (apply 'max (mapcar 'fm:x2 sten))
        fb  (append sten blokke))
  (foreach v vseg
    (if (and (>= (car v) (- fx1 1.0)) (<= (car v) (+ fx2 1.0)))
      (setq fb (cons (list (car v) (cadr v) (car v) (caddr v) nil) fb))))
  (setq ymin (apply 'min (mapcar 'fm:y1 fb))
        ymax (apply 'max (mapcar 'fm:y2 fb))
        yz   (+ ymin *fm-bundzone*))
  ;; ---------- skifter (liggende sten med den almindelige højde) ----------
  (setq normal (vl-remove-if-not '(lambda (s) (> (fm:b s) (fm:h s))) sten))
  (setq hs (if normal (nth (/ (length normal) 2) (vl-sort (mapcar 'fm:h normal) '<)) 54.0))
  (setq normal (vl-remove-if-not '(lambda (s) (< (abs (- (fm:h s) hs)) 2.0)) normal))
  (setq rows (fm:raekker normal))

  ;; ---------- det der står på bunden ----------
  ;; kun sten og lange streger (rammer, mure) der står på bunden
  (setq bund (vl-remove-if-not '(lambda (s) (<= (fm:y1 s) yz)) sten))
  (foreach iv (fm:intervaller bund) (setq xs (append xs iv)))
  (foreach v vseg
    (if (and (<= (cadr v) yz) (>= (- (caddr v) (cadr v)) 300.0))
      (setq xs (cons (car v) xs))))
  ;; facadens bredde = det der står på bunden (ellers det hele)
  (if xs
    (setq bx1 (apply 'min xs) bx2 (apply 'max xs))
    (setq bx1 (apply 'min (mapcar 'fm:x1 alle)) bx2 (apply 'max (mapcar 'fm:x2 alle))))

  ;; soldaterskifte/rulskifte i bunden (sten højere end almindelige sten)
  (setq soldater (vl-remove-if-not '(lambda (s) (> (fm:h s) (* 1.5 hs))) bund))
  (if soldater
    (setq soldattop  (apply 'max (mapcar 'fm:y2 soldater))
          soldatnavn (if (> (fm:h (car soldater)) (fm:b (car soldater))) "soldaterskifte" "rulskifte")))

  ;; ---------- højder til venstre: bund, soldaterskifte, murkroner, gesims ----------
  (setq ys (list ymin))
  (if soldattop (setq ys (cons soldattop ys)))
  ;; toppen af rammer og mure tegnet med streger, der står på bunden
  (foreach v vseg
    (if (and (<= (cadr v) yz) (>= (- (caddr v) (cadr v)) 300.0))
      (setq ys (cons (caddr v) ys))))
  ;; murkrone: øverste skifte i en mur (mindst 500 mm bred, intet ovenpå)
  (setq i 0)
  (while (< i (length rows))
    (foreach iv (caddr (nth i rows))
      (if (and (>= (- (cadr iv) (car iv)) 500.0)
               (not (and (fm:naboer i (1+ i)) (fm:kant-i (1+ i) iv 'car))))
        (setq ys (cons (cadr (nth i rows)) ys))))
    (setq i (1+ i)))
  ;; åbninger i murværket (mål inde i åbningerne)
  (fm:aabninger)
  ;; gesims (små udkragningsmål + gesimsens bund og top)
  (fm:udkragning)
  ;; gavl (egne mål) - uden gavl går kæden helt op til toppen
  (fm:gavle)

  ;; ---------- vandret under facaden ----------
  (setq xs (fm:unikke (vl-remove-if '(lambda (x) (or (< x (- bx1 1.0)) (> x (+ bx2 1.0)))) xs)
                      *fm-kantfri*))
  (if (> (abs (- (last xs) bx2)) 1.0) (setq xs (append (reverse (cdr (reverse xs))) (list bx2))))
  (setq i 0)
  (while (< i (1- (length xs)))
    (setq a (nth i xs) c (nth (1+ i) xs))
    (fm:maal (list a ymin) (list c ymin) (list (/ (+ a c) 2.0) (- ymin afst)) 0.0 nil)
    (setq i (1+ i)))
  (if (> (length xs) 2)
    (fm:maal (list bx1 ymin) (list bx2 ymin)
             (list (/ (+ bx1 bx2) 2.0) (- ymin (* 2.0 afst))) 0.0 nil))

  ;; ---------- lodret til venstre for facaden ----------
  ;; kun den samlede højde helt til toppen (også gavlen)
  (setq xl (apply 'min (cons bx1 (mapcar 'fm:x1 sten))))
  (fm:maal (list xl ymin) (list xl ymax) (list (- xl afst) (/ (+ ymin ymax) 2.0)) (/ pi 2.0) nil)

  ;; ---------- tekst med stenstørrelse og fuge ----------
  (if (and normal (> (length rows) 1))
    (progn
      (setq a (fm:median (mapcar 'fm:b normal))
            c (fm:median (vl-remove-if '(lambda (d) (> d (* 1.5 hs)))
                                       (mapcar '(lambda (r1 r2) (- (car r2) (car r1)))
                                               (reverse (cdr (reverse rows))) (cdr rows)))))
      (setq fg (fm:median (vl-remove-if '(lambda (d) (or (< d 3.0) (> d *fm-fugemax*)))
                         (apply 'append
                                (mapcar '(lambda (r / ss2 res)
                                           (setq ss2 (vl-sort (vl-remove-if-not
                                                                '(lambda (s) (and (fm:ca (fm:y1 s) (car r))
                                                                                  (fm:ca (fm:h s) hs)))
                                                                normal)
                                                              '(lambda (p q) (< (fm:x1 p) (fm:x1 q)))))
                                           (mapcar '(lambda (p q) (- (fm:x1 q) (fm:x2 p)))
                                                   (reverse (cdr (reverse ss2))) (cdr ss2)))
                                        rows)))))
      (if c
        (fm:tekst (list bx1 (- ymin (* 3.0 afst)))
                  (strcat "Sten " (fm:tal a 0) " x " (fm:tal hs 0) " mm"
                          (if fg (strcat ", studsfuge " (fm:tal fg 0) " mm") "")
                          ", skifte " (fm:tal c 1) " mm")))))
  (princ (strcat "\nMurværk: " (itoa (length sten)) " sten, " (itoa (length rows)) " skifter, "
                 (itoa *fm-aab*) " åbning(er)."))
)

;; ============================================================
;; Kommandoen
;; ============================================================
;; Sætter mål på det der er i markeringen ss.
;; egen = T: kommandoen har sit eget fortryd-punkt (ét U fjerner målene).
(defun fm:koer (ss egen / *error* doc spc th afst i e ed typ b r alle sten vseg hseg
                  gamle hs normal rows ymin ymax yz bund bx1 bx2 xs ys
                  soldater soldattop soldatnavn xl a c yg *fm-trin*
                  kand modal blokke dseg rekter rund)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
      (princ (strcat "\nFejl: " msg)))
    (vla-EndUndoMark doc)
    (princ)
  )

  (if egen (vla-StartUndoMark doc))

  (setq spc  (if (= (getvar "cvport") 1) (vla-get-PaperSpace doc) (vla-get-ModelSpace doc))
        th   (* (getvar "DIMTXT") (if (= 0.0 (getvar "DIMSCALE")) 1.0 (getvar "DIMSCALE")))
        afst (max (* 3.5 th) 200.0)               ; afstand mellem målelinjerne
        gamle 0
        *fm-antal* 0
        *fm-aab* 0
        *fm-raekkeord* "skifter")

  ;; ---------- sortér markeringen ----------
  (setq i 0)
  (repeat (sslength ss)
    (setq e (ssname ss i) ed (entget e) typ (cdr (assoc 0 ed)))
    (cond
      ((= typ "DIMENSION")
       (if (= (strcase (cdr (assoc 8 ed))) (strcase *fm-lagnavn*))
         (if (not (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list e))))
           (setq gamle (1+ gamle)))))
      ((and (= typ "TEXT") (= (strcase (cdr (assoc 8 ed))) (strcase *fm-lagnavn*)))
       (vl-catch-all-apply 'entdel (list e)))           ; gammel tekst fra FM
      ((= typ "HATCH")
       (if (setq b (fm:boks e)) (setq kand (cons (list b typ T ed) kand))))
      ((member typ '("TEXT" "MTEXT" "LEADER" "MULTILEADER" "ATTDEF"
                     "POINT" "XLINE" "RAY" "WIPEOUT" "IMAGE" "VIEWPORT"))
       nil)
      ((setq b (fm:boks e))
       (setq alle (cons b alle))
       ;; firkanter og blokke gemmes også drejet (til rundt murværk)
       (if (setq r (cond ((= typ "LWPOLYLINE") (fm:rekt-poly ed))
                         ((= typ "INSERT")     (fm:rekt-blok e))))
         (setq rekter (cons r rekter)))
       (cond
         ;; lukkede figurer og blokke kan være enheder (sten, fliser ...)
         ((or (member typ '("INSERT" "POLYLINE" "REGION" "SOLID"))
              (and (= typ "LWPOLYLINE") (= 1 (logand 1 (cdr (assoc 70 ed))))))
          (setq kand (cons (list b typ (fm:rektangel-p ed b) ed) kand)))
         ((member typ '("LINE" "LWPOLYLINE"))
          (foreach sg (fm:segmenter ed) (fm:gem-seg (car sg) (cadr sg)))))))
    (setq i (1+ i)))

  (if (not alle)
    (progn (princ "\nIntet at sætte mål på i markeringen.") (if egen (vla-EndUndoMark doc)) (exit)))

  (fm:lag)
  (if (setq rund (fm:rund-find rekter))
    ;; ---------- rundt murværk (sten i en cirkel eller bue) ----------
    (fm:rund-maal rund)
    (progn
      ;; ---------- find enheden (den størrelse der går igen) ----------
      (setq modal (fm:hyppigst kand))
      (foreach k kand
        (if (fm:enhed-p k)
          (progn
            (setq sten (cons (car k) sten))
            (if (= (cadr k) "LWPOLYLINE")
              (foreach sg (fm:segmenter (cadddr k)) (fm:gem-skraa (car sg) (cadr sg)))))
          (if (/= (cadr k) "HATCH") (progn
            (if (= (cadr k) "INSERT") (setq blokke (cons (car k) blokke)))
            (fm:som-streger k)))))

      (setq sten (fm:uden-dubletter sten))       ; sten + skravering af samme sten = én
      (setq ymin (apply 'min (mapcar 'fm:y1 alle))
            ymax (apply 'max (mapcar 'fm:y2 alle))
            yz   (+ ymin *fm-bundzone*))

      (cond
        ((fm:gitter-p) (fm:fliser))             ; fliser/enheder i lige rækker og kolonner
        (sten          (fm:murvaerk))           ; murværk i forbandt
        (T             (fm:stregmaal)))))       ; kun streger: omrids

  (if egen (vla-EndUndoMark doc))
  (princ (strcat "\nFM færdig: " (itoa *fm-antal*) " mål tegnet i laget " *fm-lagnavn*
                 (if (> gamle 0) (strcat " (" (itoa gamle) " gamle mål slettet)") "")
                 (if egen ". Ét U fjerner dem igen." ".")))
  (princ)
)

(defun c:FACADEMAAL (/ ss)
  (princ "\nMarkér det der skal have mål (facade, flisefelt, rund mur, plan ...): ")
  (if (setq ss (ssget)) (fm:koer ss T))
  (princ)
)

(defun c:FM () (c:FACADEMAAL))

(princ "\nMAAL.lsp indlæst. Kommando: FM (FACADEMAAL) - sætter de nødvendige mål på det du markerer.")
(princ)
