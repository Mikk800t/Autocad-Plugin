;;; ============================================================
;;; BUER.lsp  -  Segmentbuer, kurvehanksbuer, kløverbladsbuer og rundt murværk
;;; Kommandoer: BUER (menu), SEGBUE, KURVEBUE, KLOVERBUE, MURRUND, BUETRIM, BUEMAAL, BUESKABELON, BUEHJAELP
;;; Korte: SB, KB, KLB, MRU, BT, BMA, BSK, BHJ
;;; MURRUND (rundt murværk set oppefra) er beskrevet ved sin egen sektion.
;;; (Erstatter SEGBUE.lsp - indlæs kun denne fil)
;;;
;;; Regler fra materialet "Buer og stik":
;;;  - Fuge i bund (underside) mindst 7 mm
;;;  - Fuge i top (overside) højst 20 mm
;;;  - Ulige antal sten (slutsten i midten)
;;;  - Overkant bue skal ramme en langfuge
;;;  - Segmentbue: de øverste hjørner skal ramme en langfuge
;;;  - Kurvehanksbue: nederste lejefuge skal ramme en langfuge,
;;;    top måles 1 sten ned fra overkant skifte, inddeling
;;;    på oversiden. Fugerne er kileformede (min. 7 mm i bund,
;;;    max. 20 mm i top), og kun de sten der skal, kileskæres
;;;  - Kløverbladsbue: 3 lige store cirkler (R = spændvidde/4), en
;;;    halvcirkelbue øverst og en kvartcirkelbue i hver side. En
;;;    låsesten i hvert knæk. Spændvidde fals til fals, konstrueres
;;;    oppefra: overkant på langfuge - buehøjde - 1/4 - 1/4 = buens start.
;;;
;;; Brug (begge kommandoer):
;;;  1. Klik venstre vederlag (cirka)
;;;  2. Klik højre vederlag (cirka)
;;;  3. Klik overkant bue - PRÆCIST på en langfuge
;;;  Scriptet flytter selv vederlaget op/ned så reglerne holder.
;;;
;;; SEGBUE og KURVEBUE tegner også en fuge langs toppen af buen.
;;; I SEGBUE trimmes stenene, så de følger buen i top og bund.
;;; Til sidst kan murstenene bag buen trimmes, så de slutter
;;; præcist ved fugen langs toppen. BUETRIM gør det samme på en
;;; bue der allerede er tegnet. Murstenene skal være lukkede
;;; polylinjer eller blokke, og synlige på skærmen.
;;;
;;; Skiftehøjde: 66,7 mm (6 skifter = 400 mm). Kan ændres
;;; under [Indstillinger]. Tegningen forventes at være i mm og
;;; tegnet som opstalt med vandrette skifter.
;;; ============================================================

(vl-load-com)

;; ---------- Standardværdier (huskes i tegningen) ----------
(if (not *sb-stenh*)   (setq *sb-stenh*   54.0))          ; stenhøjde
(if (not *sb-dybde*)   (setq *sb-dybde*  228.0))          ; 1/1 sten
(if (not *sb-type*)    (setq *sb-type*   "Hele"))         ; Hele / Skiftevis
(if (not *sb-delfuge*) (setq *sb-delfuge* 12.0))          ; fuge mellem 2 halve
(if (not *sb-skifte*)  (setq *sb-skifte* (/ 400.0 6.0)))  ; skiftehøjde
(if (not *sb-topfuge*) (setq *sb-topfuge* (- *sb-skifte* *sb-stenh*))) ; fuge langs top af segmentbue
(setq *sb-minfuge*  7.0                                   ; regler fra materialet
      *sb-maxfuge* 20.0)

;; Hver bue har sine EGNE indstillinger. De gemmes her og lægges
;; ind i *sb-...* når kommandoen starter (sb:brug).
(setq *sb-navne* '(*sb-skifte* *sb-stenh* *sb-dybde* *sb-type* *sb-delfuge*
                   *sb-topfuge* *sb-minfuge* *sb-maxfuge*))
(if (not *sb-saet*)
  (setq *sb-saet*
    (list (cons "SEGMENTBUE"    (mapcar 'eval *sb-navne*))
          (cons "KURVEHANKSBUE" (mapcar 'eval *sb-navne*)))))
;; Kløverbladsbuen er som standard en halvstens bue (108 mm)
(if (not (assoc "KLOVERBLADSBUE" *sb-saet*))
  (setq *sb-saet*
    (append *sb-saet*
            (list (cons "KLOVERBLADSBUE"
                        (list *sb-skifte* *sb-stenh* 108.0 "Hele" *sb-delfuge*
                              *sb-topfuge* *sb-minfuge* *sb-maxfuge*))))))

;; Hent indstillingerne for en bue ("SEGMENTBUE" / "KURVEHANKSBUE")
;; *sb-vis* = den bue indstillingsdialogen viser - nulstilles for hver kommando
(defun sb:brug (navn / v)
  (setq *sb-aktiv* navn
        *sb-vis*   nil
        v (cdr (assoc navn *sb-saet*)))
  (if v (mapcar 'set *sb-navne* v))
)

;; Gem de nuværende indstillinger for den bue der er i gang
(defun sb:gem ()
  (if *sb-aktiv*
    (setq *sb-saet*
      (cons (cons *sb-aktiv* (mapcar 'eval *sb-navne*))
            (vl-remove (assoc *sb-aktiv* *sb-saet*) *sb-saet*))))
)

;; ============================================================
;; Fælles hjælpefunktioner
;; ============================================================

(defun sb:tal (x d) (vl-string-subst "," "." (rtos x 2 d)))
(defun sb:2d (p) (list (car p) (cadr p)))
(defun sb:tan (x) (/ (sin x) (cos x)))
(defun sb:asin (x) (atan x (sqrt (max 0.0 (- 1.0 (* x x))))))

;; ============================================================
;; Små billeder i dialogerne (fælles for alle vinduer)
;; ============================================================
;; Punkterne er pixels (reelle tal). Farverne er AutoCAD-farvenumre:
;;  255 hvid mur, 254 lysegrå langfuger, -15 dialogens grå (åbningen),
;;  252 mørkegrå åbningens sider, 92 grøn langfuge buen skal ramme (94 = grøn prik),
;;  22/24 teglrød hel sten, 40/42 gul kileskåret sten, 150/154 blå låsesten.

;; Pixel inden for billedet (0 .. maks-1)
(defun bill:ind (v maks) (max 0 (min (1- maks) (fix (+ v 0.5)))))

;; Fyld en polygon med vandrette streger
(defun bill:fyld (pl bw bh farve / n y yc i p q xs xa xb)
  (setq n (length pl)
        y (max 0 (fix (apply 'min (mapcar 'cadr pl)))))
  (while (<= y (min (1- bh) (fix (apply 'max (mapcar 'cadr pl)))))
    (setq yc (+ y 0.5) xs nil i 0)
    (while (< i n)
      (setq p (nth i pl)
            q (nth (rem (1+ i) n) pl))
      (if (or (and (<= (cadr p) yc) (> (cadr q) yc))
              (and (<= (cadr q) yc) (> (cadr p) yc)))
        (setq xs (cons (+ (car p) (/ (* (- yc (cadr p)) (- (car q) (car p)))
                                     (- (cadr q) (cadr p))))
                       xs)))
      (setq i (1+ i)))
    ;; vl-sort kan fjerne ens tal - vl-sort-i beholder dem
    (setq xs (mapcar '(lambda (j) (nth j xs)) (vl-sort-i xs '<)))
    (while (cdr xs)
      (setq xa (max 0 (fix (+ (car xs) 0.5)))
            xb (min (1- bw) (fix (- (cadr xs) 0.5))))
      (if (>= xb xa) (vector_image xa y xb y farve))
      (setq xs (cddr xs)))
    (setq y (1+ y)))
)

;; Streg fra p til q, klippet til billedet (stykker udenfor tegnes ikke)
(defun bill:streg (p q bw bh farve / x0 y0 dx dy t0 t1 ok k pp qq r)
  (setq x0 (car p) y0 (cadr p)
        dx (- (car q) x0) dy (- (cadr q) y0)
        t0 0.0 t1 1.0 ok T)
  (foreach k (list (list (- dx) x0) (list dx (- bw 1.0 x0))
                   (list (- dy) y0) (list dy (- bh 1.0 y0)))
    (setq pp (car k) qq (cadr k))
    (cond ((equal pp 0.0 1e-12) (if (< qq 0.0) (setq ok nil)))
          ((< pp 0.0) (setq r (/ qq pp))
                      (if (> r t1) (setq ok nil) (setq t0 (max t0 r))))
          (T (setq r (/ qq pp))
             (if (< r t0) (setq ok nil) (setq t1 (min t1 r))))))
  (if ok
    (vector_image (bill:ind (+ x0 (* t0 dx)) bw) (bill:ind (+ y0 (* t0 dy)) bh)
                  (bill:ind (+ x0 (* t1 dx)) bw) (bill:ind (+ y0 (* t1 dy)) bh) farve))
)

;; Sten: fyldt polygon med kant
(defun bill:sten (pl bw bh fyld kant / i n)
  (bill:fyld pl bw bh fyld)
  (setq i 0 n (length pl))
  (while (< i n)
    (bill:streg (nth i pl) (nth (rem (1+ i) n) pl) bw bh kant)
    (setq i (1+ i)))
)

;; n+1 punkter på en cirkelbue om c med radius r fra vinkel a0 til a1
(defun bill:bue (c r a0 a1 n / i res)
  (setq i 0)
  (while (<= i n)
    (setq res (cons (polar c (+ a0 (* (- a1 a0) (/ (float i) n))) r) res)
          i   (1+ i)))
  (reverse res)
)

;; Rund prik midt i et lille billede: farve (94 grøn, 12 rød) eller nil = tom
(defun bill:prik (key farve / bw bh i pl)
  (setq bw (dimx_tile key) bh (dimy_tile key) i 0)
  (start_image key)
  (fill_image 0 0 bw bh -15)
  (if farve
    (progn
      (repeat 16
        (setq pl (cons (polar (list (/ bw 2.0) (/ bh 2.0)) (* i (/ pi 8.0)) (* 0.36 (min bw bh))) pl)
              i  (1+ i)))
      (bill:fyld pl bw bh farve)))
  (end_image)
)

;; Lokalt punkt (x langs vederlagslinjen, y op) -> UCS
(defun sb:lok (m x y) (list (+ (car m) x) (+ (cadr m) y) (caddr m)))

;; Polylinje fra UCS-punkter med bulges. Returnerer ename.
(defun sb:pl (pts bulges lukket)
  (entmake
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
            (cons 90 (length pts)) (cons 70 (if lukket 1 0)))
      (apply 'append
        (mapcar '(lambda (p b) (list (cons 10 (sb:2d (trans p 1 0))) (cons 42 b)))
                pts bulges))
    )
  )
  (entlast)
)

;; Linje mellem to UCS-punkter. Returnerer ename.
(defun sb:linje (a b)
  (entmake (list '(0 . "LINE") (cons 10 (trans a 1 0)) (cons 11 (trans b 1 0))))
  (entlast)
)

;; Saml objekter i en gruppe PREFIX1, PREFIX2 ...
;; Objekterne lægges i én ad gangen, så ét objekt der driller ikke
;; ødelægger hele gruppen. Returnerer navnet, evt. med antal fejl.
(defun sb:gruppe (doc enames prefix / grps i navn grp res fejl besked ssf ekko)
  ;; kun objekter der stadig findes, og hvert objekt kun én gang
  (foreach e enames
    (if (and (entget e) (not (member e res))) (setq res (cons e res))))
  (setq enames (reverse res)
        grps   (vla-get-Groups doc)
        i      1
        fejl   0
        ssf    (ssadd))
  (while (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-Item (list grps (strcat prefix (itoa i))))))
    (setq i (1+ i)))
  (setq navn (strcat prefix (itoa i))
        grp  (vla-Add grps navn))
  (foreach e enames
    (setq res (vl-catch-all-apply 'vla-AppendItems
                (list grp
                      (vlax-make-variant
                        (vlax-safearray-fill
                          (vlax-make-safearray vlax-vbObject '(0 . 0))
                          (list (vlax-ename->vla-object e)))))))
    (if (vl-catch-all-error-p res)
      (setq besked (vl-catch-all-error-message res)
            ssf    (ssadd e ssf))))
  ;; Reserve: tilføj resten med AutoCAD's egen GROUP-kommando
  (if (> (sslength ssf) 0)
    (progn
      (setq ekko (getvar "cmdecho"))
      (setvar "cmdecho" 0)
      (vl-catch-all-apply 'command (list "_.-GROUP" "_Add" navn ssf ""))
      (setvar "cmdecho" ekko)
      ;; tæl hvor mange der stadig mangler
      (setq fejl (max 0 (- (length enames) (vla-get-Count grp))))))
  ;; PICKSTYLE 0 eller 2 = grupper vælges ikke samlet
  (if (= 0 (logand 1 (getvar "PICKSTYLE")))
    (princ (strcat "\nBemærk: gruppen " navn " vælges ikke samlet, fordi PICKSTYLE er "
                   (itoa (getvar "PICKSTYLE"))
                   ". Skriv PICKSTYLE og sæt den til 1 (eller tryk Ctrl+Shift+A).")))
  (if (> fejl 0)
    (strcat navn " (" (itoa (- (length enames) fejl)) " objekter, "
            (itoa fejl) " kunne ikke komme med: " besked ")")
    (strcat navn " (" (itoa (length enames)) " objekter)"))
)

(defun sb:indstillinger (/ v)
  (initget 6)
  (if (setq v (getdist (strcat "\nSkiftehøjde <" (sb:tal *sb-skifte* 2) ">: ")))
    (setq *sb-skifte* v))
  (initget 6)
  (if (setq v (getdist (strcat "\nStenbredde (stenens bredde i buen) <" (sb:tal *sb-stenh* 1) ">: ")))
    (setq *sb-stenh* v))
  (initget 6 "HAlvsten HElsten")
  (setq v (getdist (strcat "\nBuedybde [HAlvsten/HElsten] eller mm <" (sb:tal *sb-dybde* 0) ">: ")))
  (cond
    ((= v "HAlvsten") (setq *sb-dybde* 108.0))
    ((= v "HElsten")  (setq *sb-dybde* 228.0))
    ((numberp v)      (setq *sb-dybde* v))
  )
  (initget 4)
  (if (setq v (getdist (strcat "\nFuge langs toppen af buen (0 = ingen) <"
                               (sb:tal *sb-topfuge* 1) ">: ")))
    (setq *sb-topfuge* v))
  (initget 6)
  (if (setq v (getdist (strcat "\nMindste fuge i bund <" (sb:tal *sb-minfuge* 1) ">: ")))
    (setq *sb-minfuge* v))
  (initget 6)
  (if (setq v (getdist (strcat "\nStørste fuge i top <" (sb:tal *sb-maxfuge* 1) ">: ")))
    (setq *sb-maxfuge* v))
)

(defun sb:stentype (/ v)
  (initget "Hele Skiftevis")
  (if (setq v (getkword (strcat "\nStentype [Hele/Skiftevis] <" *sb-type* ">: ")))
    (setq *sb-type* v))
  (if (= *sb-type* "Skiftevis")
    (progn
      (initget 6)
      (if (setq v (getdist (strcat "\nFuge mellem de 2 halve sten <" (sb:tal *sb-delfuge* 0) ">: ")))
        (setq *sb-delfuge* v))))
)

;; ------------------------------------------------------------
;; Dialog til indstillinger og stentype. Samme dialog til begge buer,
;; men SEGBUE og KURVEBUE har hver sine egne værdier (se sb:brug).
;; ------------------------------------------------------------

;; Tal til visning: dansk komma uden overflødige nuller (66,67 / 54 / 12,7)
(defun sb:tal-kort (x / s)
  (setq s (sb:tal x 2))
  (if (vl-string-search "," s)
    (setq s (vl-string-right-trim "," (vl-string-right-trim "0" s))))
  s
)

;; Tekst fra et felt til tal (både komma og punktum). nil hvis ugyldigt.
(defun sb:tal-ind (s)
  (distof (vl-string-translate "," "." s) 2)
)

;; Skriv dialogen til en midlertidig DCL-fil. Ledeteksterne sættes
;; fra LISP, så der ikke skal være æ, ø og å i selve filen.
;; titel = dialogens overskrift (uden æ, ø og å)
(defun sb:indst-dcl (titel / fn f)
  (setq fn (vl-filename-mktemp "buerindst" nil ".dcl")
        f  (open fn "w"))
  (write-line "sb_indst : dialog {" f)
  (write-line (strcat "  label = \"" titel "\";") f)
  (foreach l
    '("  : row {"
      "  : column {"
      "  : text { key = \"t_bue\"; width = 50; }"
      "  : boxed_column { label = \"Mur og sten\";"
      "    : row { : text { key = \"t_skifte\"; width = 34; } : edit_box { key = \"skifte\"; edit_width = 8; } }"
      "    : row { : text { key = \"t_stenh\"; width = 34; } : edit_box { key = \"stenh\"; edit_width = 8; } }"
      "  }"
      "  : boxed_column { label = \"Buedybde\";"
      "    : radio_row { key = \"dybde_r\";"
      "      : radio_button { key = \"ha\"; label = \"Halvsten (108)\"; }"
      "      : radio_button { key = \"he\"; label = \"Helsten (228)\"; }"
      "      : radio_button { key = \"an\"; label = \"Andet\"; }"
      "    }"
      "    : row { : text { key = \"t_dybde\"; width = 34; } : edit_box { key = \"dybde\"; edit_width = 8; } }"
      "  }"
      "  : boxed_column { label = \"Stentype\";"
      "    : radio_row { key = \"type_r\";"
      "      : radio_button { key = \"Hele\"; label = \"Hele sten\"; }"
      "      : radio_button { key = \"Skiftevis\"; label = \"Skiftevis (hver 2. sten = 2 halve + fuge)\"; }"
      "    }"
      "    : row { : text { key = \"t_delfuge\"; width = 34; } : edit_box { key = \"delfuge\"; edit_width = 8; } }"
      "  }"
      "  : boxed_column { label = \"Fuger\";"
      "    : row { : text { key = \"t_topfuge\"; width = 34; } : edit_box { key = \"topfuge\"; edit_width = 8; } }"
      "    : row { : text { key = \"t_minfuge\"; width = 34; } : edit_box { key = \"minfuge\"; edit_width = 8; } }"
      "    : row { : text { key = \"t_maxfuge\"; width = 34; } : edit_box { key = \"maxfuge\"; edit_width = 8; } }"
      "  }"
      "  }"
      "  : boxed_column { label = \"Buen med disse indstillinger\";"
      "    : image { key = \"billede\"; width = 64; height = 19; color = 255; }"
      "    : text { key = \"b1\"; width = 64; }"
      "    : text { key = \"b2\"; width = 64; }"
      "    : text { key = \"b3\"; width = 64; }"
      "    : text { key = \"b4\"; width = 64; }"
      "  }"
      "  }"
      "  : text { key = \"advarsel\"; width = 118; }"
      "  : row { alignment = centered; fixed_width = true;"
      "    : button { key = \"accept\"; label = \"OK\"; is_default = true; width = 14; }"
      "    : button { key = \"standard\"; label = \"Standard\"; width = 14; }"
      "    : button { key = \"cancel\"; label = \"Annuller\"; is_cancel = true; width = 14; }"
      "  }"
      "}")
    (write-line l f))
  (close f)
  fn
)

;; Sæt felterne i dialogen
(defun sb:indst-vis (skifte stenh dybde typ delfuge topfuge minfuge maxfuge)
  (set_tile "skifte"  (sb:tal-kort skifte))
  (set_tile "stenh"   (sb:tal-kort stenh))
  (set_tile "dybde"   (sb:tal-kort dybde))
  (set_tile "delfuge" (sb:tal-kort delfuge))
  (set_tile "topfuge" (sb:tal-kort topfuge))
  (set_tile "minfuge" (sb:tal-kort minfuge))
  (set_tile "maxfuge" (sb:tal-kort maxfuge))
  (set_tile "type_r"  typ)
  (sb:indst-dybde-r)
  (sb:indst-tjek)
)

;; Buedybde-feltet ændret: vælg den passende knap
(defun sb:indst-dybde-r (/ v)
  (setq v (sb:tal-ind (get_tile "dybde")))
  (set_tile "dybde_r" (cond ((and v (equal v 108.0 0.01)) "ha")
                            ((and v (equal v 228.0 0.01)) "he")
                            (T "an")))
)

;; Knap for buedybde valgt: sæt tallet
(defun sb:indst-dybde-knap (k)
  (cond ((= k "ha") (set_tile "dybde" "108"))
        ((= k "he") (set_tile "dybde" "228"))
        (T (mode_tile "dybde" 2)))                 ; markør i feltet
  (sb:indst-tjek)
)

;; Opdatér advarsel og grå felter
(defun sb:indst-tjek (/ dybde delfuge)
  (setq dybde   (sb:tal-ind (get_tile "dybde"))
        delfuge (sb:tal-ind (get_tile "delfuge")))
  (mode_tile "delfuge" (if (= (get_tile "type_r") "Skiftevis") 0 1))
  (set_tile "advarsel"
    (if (and (= (get_tile "type_r") "Skiftevis") dybde delfuge
             (< (/ (- dybde delfuge) 2.0) 90.0))
      "OBS: Skiftevis kræver Helsten - ellers tegnes hele sten."
      ""))
  (sb:indst-billede)
)

;; Et tal fra et felt. Er det ugyldigt (eller ikke over mindst), bruges gl.
(defun sb:indst-felt (key gl mindst / v)
  (setq v (sb:tal-ind (get_tile key)))
  (if (and v (> v mindst)) v gl)
)

;; Tallene i felterne i samme rækkefølge som *sb-navne*
(defun sb:indst-vaerdier (/ mn)
  (setq mn (sb:indst-felt "minfuge" *sb-minfuge* 0.0))
  (list (sb:indst-felt "skifte"  *sb-skifte*  20.0)
        (sb:indst-felt "stenh"   *sb-stenh*   10.0)
        (sb:indst-felt "dybde"   *sb-dybde*   40.0)
        (get_tile "type_r")
        (sb:indst-felt "delfuge" *sb-delfuge* 0.0)
        (sb:indst-felt "topfuge" *sb-topfuge* -0.001)
        mn
        (sb:indst-felt "maxfuge" (max *sb-maxfuge* (+ mn 1.0)) mn))
)

;; Tegn buen med tallene i felterne. En fejl i tegningen må aldrig
;; lukke dialogen - så skrives den bare under billedet.
(defun sb:indst-billede (/ svar)
  (setq svar (vl-catch-all-apply 'sb:indst-vis-bue (sb:indst-vaerdier)))
  (if (vl-catch-all-error-p svar)
    (set_tile "b2" (strcat "Kunne ikke tegne buen: " (vl-catch-all-error-message svar))))
)

;; Beregn og tegn buen. Parametrene hedder det samme som de gemte
;; indstillinger, så alle beregningerne bruger tallene fra felterne,
;; mens funktionen kører - de gemte indstillinger ændres ikke.
;; Buen er den man er i gang med (*sb-vis*), ellers et eksempel.
(defun sb:indst-vis-bue (*sb-skifte* *sb-stenh* *sb-dybde* *sb-type* *sb-delfuge*
                         *sb-topfuge* *sb-minfuge* *sb-maxfuge* / vis kv kand x s a tekst)
  (setq vis (if (equal (car *sb-vis*) *sb-aktiv*) (cdr *sb-vis*)))
  (cond
    ((= *sb-aktiv* "SEGMENTBUE")
     (setq s (if vis (car vis) 1200.0))
     (if vis
       (setq kv (sb:seg-kandidater s (cadr vis) (caddr vis))
             x  (if (cadr kv) (cadr kv) (caddr kv)))
       (setq x (sb:seg-eksempel s)))
     (if x
       (progn
         (sb:seg-billede x s)
         (setq tekst (strcat (itoa (car (caddr x))) " sten, stik " (sb:tal (cadr x) 0)
                             " mm, fuge i bund " (sb:tal (nth 2 (caddr x)) 1)
                             " og i top " (sb:tal (nth 3 (caddr x)) 1) " mm"
                             (if (= (sb:seg-fejl (caddr x)) 0.0) " - OK" " - IKKE OK"))))))
    ((= *sb-aktiv* "KLOVERBLADSBUE")
     (setq a (if vis (car vis) 600.0)
           s (* 2.0 a)
           x (car (kl:kandidater a 0.0)))
     (if x
       (progn
         (kl:billede x)
         (setq tekst (strcat (itoa (kl:antal-sten (nth 3 x))) " sten, fuge i top "
                             (kl:fugetop (nth 3 x)) " og i bund " (kl:fugebund (nth 3 x))
                             " mm, " (itoa (nth 9 (nth 3 x))) " kileskårne")))))
    (T
     (setq a    (if vis (car vis) 600.0)
           s    (* 2.0 a)
           kand (kb:kandidater a))
     (if kand
       (progn
         (setq x (nth (kb:bedste a kand) kand))
         (kb:billede x a)
         (setq tekst (strcat (itoa (nth 2 x)) " sten, pilhøjde " (sb:tal (cadr x) 0)
                             " mm, fuge i top " (sb:tal (nth 3 x) 1) " og i bund "
                             (kb:bundtekst x) " mm, " (itoa (nth 7 x)) " kileskårne"))))))
  (if (not x)
    (progn
      (start_image "billede")
      (fill_image 0 0 (dimx_tile "billede") (dimy_tile "billede") 255)
      (end_image)))
  (set_tile "b1" (if vis
                   (strcat "Din bue: spændvidde " (sb:tal s 0) " mm")
                   (strcat "Eksempel med spændvidde " (sb:tal s 0)
                           " mm (din egen bue vises, når vederlagene er klikket)")))
  (set_tile "b2" (if tekst (strcat "Forslag: " tekst) "Ingen bue passer med disse tal."))
  (set_tile "b3" (cond ((= *sb-aktiv* "SEGMENTBUE")
                        "Rød = sten    Grøn = overkant og den langfuge hjørnerne skal ramme")
                       ((= *sb-aktiv* "KLOVERBLADSBUE")
                        "Rød = hel sten   Gul = kileskåret   Blå = låsesten   + = centre")
                       (T "Rød = hel sten    Gul = kileskåret    + = cirklernes centre")))
)

;; OK: kontrollér felterne og gem. Fejl vises i dialogen.
(defun sb:indst-gem (/ skifte stenh dybde delfuge topfuge minfuge maxfuge fejl)
  (setq skifte  (sb:tal-ind (get_tile "skifte"))
        stenh   (sb:tal-ind (get_tile "stenh"))
        dybde   (sb:tal-ind (get_tile "dybde"))
        delfuge (sb:tal-ind (get_tile "delfuge"))
        topfuge (sb:tal-ind (get_tile "topfuge"))
        minfuge (sb:tal-ind (get_tile "minfuge"))
        maxfuge (sb:tal-ind (get_tile "maxfuge")))
  (setq fejl
    (cond
      ((not (and skifte (> skifte 0.0)))   "Skiftehøjde skal være et tal over 0.")
      ((not (and stenh (> stenh 0.0)))     "Stenhøjde skal være et tal over 0.")
      ((not (and dybde (> dybde 0.0)))     "Buedybde skal være et tal over 0.")
      ((not (and delfuge (> delfuge 0.0))) "Fuge mellem de 2 halve skal være over 0.")
      ((not (and topfuge (>= topfuge 0.0))) "Fuge langs toppen skal være 0 eller mere.")
      ((not (and minfuge (> minfuge 0.0))) "Mindste fuge skal være over 0.")
      ((not (and maxfuge (> maxfuge minfuge))) "Største fuge skal være større end mindste fuge.")))
  (if fejl
    (set_tile "advarsel" (strcat "FEJL: " fejl))
    (progn
      (setq *sb-skifte* skifte *sb-stenh* stenh *sb-dybde* dybde
            *sb-delfuge* delfuge *sb-topfuge* topfuge
            *sb-minfuge* minfuge *sb-maxfuge* maxfuge
            *sb-type* (get_tile "type_r"))
      (sb:gem)
      (done_dialog 1)))
)

;; Åbn dialogen. Virker dialogen ikke, spørges i kommandolinjen.
;; Gælder kun den bue der er i gang (*sb-aktiv*).
(defun sb:indst-dialog (/ fn id seg klo)
  (setq seg (= *sb-aktiv* "SEGMENTBUE")
        klo (= *sb-aktiv* "KLOVERBLADSBUE")
        fn  (sb:indst-dcl (cond (seg "Indstillinger - Segmentbue")
                                (klo "Indstillinger - Kloeverbladsbue")
                                (T   "Indstillinger - Kurvehanksbue")))
        id  (load_dialog fn))
  (if (or (< id 0) (not (new_dialog "sb_indst" id)))
    (progn
      (if (>= id 0) (unload_dialog id))
      (vl-file-delete fn)
      (sb:indstillinger)
      (sb:stentype)
      (sb:gem))
    (progn
      (set_tile "t_bue" (cond (seg "Gælder kun segmentbuer (SEGBUE).")
                              (klo "Gælder kun kløverbladsbuer (KLOVERBUE).")
                              (T   "Gælder kun kurvehanksbuer (KURVEBUE).")))
      (set_tile "t_skifte"  "Skiftehøjde (mm):")
      (set_tile "t_stenh"   "Stenhøjde - stenens bredde i buen (mm):")
      (set_tile "t_dybde"   "Buedybde (mm):")
      (set_tile "t_delfuge" "Fuge mellem de 2 halve sten (mm):")
      (set_tile "t_topfuge" "Fuge langs toppen af buen (0 = ingen):")
      (set_tile "t_minfuge" "Mindste fuge i bund (mm):")
      (set_tile "t_maxfuge" "Største fuge i top (mm):")
      (set_tile "b4"        "Tegningen skifter, når du går videre til næste felt.")
      (sb:indst-vis *sb-skifte* *sb-stenh* *sb-dybde* *sb-type* *sb-delfuge*
                    *sb-topfuge* *sb-minfuge* *sb-maxfuge*)
      (action_tile "dybde_r"  "(sb:indst-dybde-knap $value)")
      (action_tile "dybde"    "(sb:indst-dybde-r) (sb:indst-tjek)")
      (action_tile "type_r"   "(sb:indst-tjek)")
      (action_tile "delfuge"  "(sb:indst-tjek)")
      (action_tile "skifte"   "(sb:indst-tjek)")
      (action_tile "stenh"    "(sb:indst-tjek)")
      (action_tile "topfuge"  "(sb:indst-tjek)")
      (action_tile "minfuge"  "(sb:indst-tjek)")
      (action_tile "maxfuge"  "(sb:indst-tjek)")
      (action_tile "standard" (strcat "(sb:indst-vis (/ 400.0 6.0) 54.0 " (if klo "108.0" "228.0")
                                      " \"Hele\" 12.0 (- (/ 400.0 6.0) 54.0) 7.0 20.0)"))
      (action_tile "accept"   "(sb:indst-gem)")
      (action_tile "cancel"   "(done_dialog 0)")
      (start_dialog)
      (unload_dialog id)
      (vl-file-delete fn)
      (princ (strcat "\nIndstillinger: skifte " (sb:tal-kort *sb-skifte*)
                     " mm, buedybde " (sb:tal-kort *sb-dybde*)
                     " mm, sten: " *sb-type*
                     ", fuge " (sb:tal-kort *sb-minfuge*) "-" (sb:tal-kort *sb-maxfuge*) " mm"))))
  (princ)
)

;; Klik punkt med mulighed for Indstillinger og Stentype (begge åbner dialogen)
(defun sb:punkt (base tekst / p)
  (while
    (progn
      (initget "Indstillinger Stentype")
      (setq p (if base (getpoint base tekst) (getpoint tekst)))
      (member p '("Indstillinger" "Stentype"))
    )
    (sb:indst-dialog)
  )
  p
)

;; Find den langfuge i muren der ligger nærmest det klikkede vederlag.
;; Kigger på murstenene (lukkede polylinjer eller blokke) omkring de
;; 2 klik og bruger overkanten af stenene (overkant af et skifte).
;; Returnerer y (UCS) eller nil hvis der ikke er mursten i nærheden.
;; Murstenene skal være synlige på skærmen.
(defun kb:langfuge (p1 p2 yv / tops ss i obj mn mx h y bedst)
  (foreach p (list p1 p2)
    (setq ss (ssget "_C" (list (- (car p) 400.0) (- yv (* 2.0 *sb-skifte*)))
                         (list (+ (car p) 400.0) (+ yv (* 2.0 *sb-skifte*)))
                    '((0 . "LWPOLYLINE,INSERT")))
          i 0)
    (if ss
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)) i (1+ i))
        (if (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
          (progn
            (setq mn (trans (vlax-safearray->list mn) 0 1)
                  mx (trans (vlax-safearray->list mx) 0 1)
                  h  (- (cadr mx) (cadr mn)))
            ;; kun ting på størrelse med en mursten (høj som en sten)
            (if (and (> h (* 0.7 *sb-stenh*)) (< h (* 1.3 *sb-stenh*))
                     (< (- (car mx) (car mn)) 600.0))
              (setq tops (cons (cadr mx) tops))))))))
  (foreach y tops
    (if (and (<= (abs (- y yv)) (* 0.75 *sb-skifte*))
             (or (not bedst) (< (abs (- y yv)) (abs (- bedst yv)))))
      (setq bedst y)))
  bedst
)

;; Fælles indtastning: returnerer (x1 x2 yV0 yT z) eller nil
;; forslagfn (eller nil): funktion (halv-spændvidde vederlag-y) der viser
;; forslag til overkant og returnerer (ok y), (klik y), (indst), (type) eller nil.
(defun sb:input (navn forslagfn / p1 p2 pk mid fy fr yv ys)
  (sb:brug navn)
  (princ (strcat "\n--- " navn " ---  Sten: " *sb-type*
                 "   Skifte: " (sb:tal *sb-skifte* 1) " mm"
                 "   Fuge: " (sb:tal *sb-minfuge* 0) "-" (sb:tal *sb-maxfuge* 0) " mm"))
  (if (and (setq p1 (sb:punkt nil "\nVenstre vederlag (cirka) eller [Indstillinger/Stentype]: "))
           (setq p2 (sb:punkt p1 "\nHøjre vederlag (cirka) eller [Indstillinger/Stentype]: ")))
    (progn
      (setq mid (list (/ (+ (car p1) (car p2)) 2.0) (cadr p1) (caddr p1)))
      ;; Overkant: forslag i en dialog (KURVEBUE), ellers klik
      (setq fr (list 'klik nil)
            yv (/ (+ (cadr p1) (cadr p2)) 2.0))
      (if forslagfn
        (progn
          ;; Vederlaget rettes til nærmeste langfuge i muren, så
          ;; forslaget til overkant også ligger på en langfuge
          (if (setq ys (kb:langfuge p1 p2 yv))
            (princ (strcat "\nVederlag rettet til langfuge i muren: "
                           (if (> (- ys yv) 0) "+" "") (sb:tal (- ys yv) 0) " mm"))
            (progn
              (setq ys yv)
              (princ "\nFandt ingen mursten ved vederlaget - bruger dit klik.")
              (princ "\n(Vælg 'Klik selv overkant' og klik PRÆCIST på en langfuge, så rettes vederlaget.)")))
          (while (member (car (setq fr (apply forslagfn
                                         (list (/ (abs (- (car p2) (car p1))) 2.0) ys))))
                         '(indst type))
            (sb:indst-dialog))))
      (cond
        ((null fr) (setq pk nil))                       ; annulleret
        ((eq (car fr) 'ok) (setq pk (list (car mid) (cadr fr) (caddr mid))))
        (T
         (setq fy (cadr fr))
         (while
           (progn
             (initget "Indstillinger Stentype")
             (setq pk (getpoint mid (strcat "\nOverkant bue - klik PÅ en langfuge, eller [Indstillinger/Stentype]"
                                            (if fy " <Enter = forslag>" "") ": ")))
             (member pk '("Indstillinger" "Stentype")))
           (sb:indst-dialog))
         (if (and (null pk) fy) (setq pk (list (car mid) fy (caddr mid))))))
      (if pk
        (list (min (car p1) (car p2))
              (max (car p1) (car p2))
              (if ys ys yv)                            ; rettet vederlag (KURVEBUE)
              (cadr pk)
              (caddr p1)))
    )
  )
)

;; Er der plads til 2 halve sten + fuge?
(defun sb:skiftevis-p ()
  (if (= *sb-type* "Skiftevis")
    (if (< (/ (- *sb-dybde* *sb-delfuge*) 2.0) 90.0)
      (progn (princ "\nBuedybden er for lille til 2 halve sten - tegner hele sten.") nil)
      T))
)

;; ============================================================
;; SEGMENTBUE
;; ============================================================

;; Beregn bue for spændvidde sp og stik h.
;; Returnerer (n gam fi fe r tot beta) eller nil.
;; n = flest mulige sten (ulige) med fuge i bund >= min
;; (giver også den mindste fuge i top).
(defun sb:seg-beregn (sp h / r tot ri re half beta n gam)
  (setq r    (/ (+ (* sp sp 0.25) (* h h)) (* 2.0 h))
        tot  (* 2.0 (atan (/ sp 2.0) (- r h)))
        ri   r
        re   (+ r *sb-dybde*)
        half (/ *sb-stenh* 2.0)
        beta (* 2.0 (atan half (sqrt (- (* ri ri) (* half half)))))
        n    (fix (/ (* tot ri) (+ *sb-stenh* *sb-minfuge*))))
  (if (= (rem n 2) 0) (setq n (1- n)))
  (setq gam (/ (- tot (* n beta)) (1+ n)))
  (while (and (> n 1) (< (* ri gam) *sb-minfuge*))
    (setq n   (- n 2)
          gam (/ (- tot (* n beta)) (1+ n))))
  (if (>= n 3)
    (list n gam (* ri gam)
          (- (* 2.0 re (sin (/ (+ beta gam) 2.0))) *sb-stenh*)
          r tot beta))
)

;; Hvor meget fugereglerne overskrides (0 = OK)
(defun sb:seg-fejl (res)
  (if res
    (+ (max 0.0 (- (nth 3 res) *sb-maxfuge*))
       (max 0.0 (- *sb-minfuge* (nth 2 res))))
    1e9)
)

;; Højde fra de øverste hjørner op til overkant bue i midten
(defun sb:seg-hjoerne (sp h / r)
  (setq r (/ (+ (* sp sp 0.25) (* h h)) (* 2.0 h)))
  (+ h (* *sb-dybde* (- 1.0 (/ (- r h) r))))
)

;; Find stik h så hjørnerne ligger 'maal' under overkant (halvering)
(defun sb:seg-loes (sp maal / lo hi mid i)
  (setq lo 0.5 hi (/ sp 2.0) i 0)
  (if (>= (sb:seg-hjoerne sp hi) maal)
    (progn
      (while (< i 60)
        (setq mid (/ (+ lo hi) 2.0))
        (if (< (sb:seg-hjoerne sp mid) maal) (setq lo mid) (setq hi mid))
        (setq i (1+ i)))
      (/ (+ lo hi) 2.0)
    )
  )
)

;; ------------------------------------------------------------
;; SEGBUE: løsninger og valg i en dialog med forhåndsvisning
;; ------------------------------------------------------------

;; Alle løsninger hvor hjørnerne ligger k skifter under overkant.
;; Returnerer (kandidater valgt bedst). Kandidat = (k stik res vederlag-y).
;; valgt = overholder reglerne og ligger tættest på de klikkede vederlag.
(defun sb:seg-kandidater (s yT yV0 / k h res kand valgt bedst)
  (setq k 1)
  (while (setq h (sb:seg-loes s (* k *sb-skifte*)))
    (setq res  (sb:seg-beregn s h)
          kand (cons (list k h res (- yT h *sb-dybde*)) kand)
          k    (1+ k)))
  (setq kand (reverse (vl-remove-if-not '(lambda (x) (caddr x)) kand)))
  (foreach x kand
    (if (and (= (sb:seg-fejl (caddr x)) 0.0)
             (or (not valgt) (< (abs (- (cadddr x) yV0)) (abs (- (cadddr valgt) yV0)))))
      (setq valgt x))
    (if (or (not bedst) (< (sb:seg-fejl (caddr x)) (sb:seg-fejl (caddr bedst))))
      (setq bedst x)))
  (list kand valgt bedst)
)

;; Kolonnerne i en liste: adskilt med tab i dialogen (listen har
;; tabs, så kolonnerne står lige med almindelig skrift), udfyldt med
;; mellemrum i kommandolinjen
(defun sb:kolonner (celler bredder tab / res c)
  (if tab
    (progn
      (setq res (car celler))
      (foreach c (cdr celler) (setq res (strcat res "\t" c)))
      res)
    (apply 'strcat (mapcar 'kb:pad celler bredder)))
)

(defun sb:seg-overskrift (tab)
  (sb:kolonner '("Hjørner" "Stik" "Radius" "Sten" "Fuge bund" "Fuge top" "Vederlag" "Regler" "")
               '(13 9 10 6 11 10 10 9 0) tab)
)
(defun sb:seg-linje (x yV0 forvalgt tab / res)
  (setq res (caddr x))
  (sb:kolonner
    (list (strcat (itoa (car x)) (if (= (car x) 1) " skifte" " skifter"))
          (strcat (sb:tal (cadr x) 0) " mm")
          (strcat (sb:tal (nth 4 res) 0) " mm")
          (itoa (car res))
          (strcat (sb:tal (nth 2 res) 1) " mm")
          (strcat (sb:tal (nth 3 res) 1) " mm")
          (strcat (if (> (- (cadddr x) yV0) 0) "+" "") (sb:tal (- (cadddr x) yV0) 0) " mm")
          (if (= (sb:seg-fejl res) 0.0) "OK" "IKKE OK")
          (cond ((not forvalgt) "") (tab "Forslag") (T "<- forslag")))
    '(13 9 10 6 11 10 10 9 0)
    tab)
)

;; Dialogen. Ingen æ, ø og å i filen - de tekster sættes fra LISP.
;; "Valgt bue" er en tabel: ledetekst | prik (grøn/rød) | værdi.
(defun sb:seg-dcl-fil (/ fn f prik)
  (setq fn   (vl-filename-mktemp "segbue" nil ".dcl")
        f    (open fn "w")
        prik "; width = 2; height = 1; fixed_width = true; fixed_height = true; color = dialog_background; }")
  (foreach l
    '("sb_segvalg : dialog {"
      "  label = \"Segmentbue\";"
      "  : boxed_column { label = \"Bue og sten\";"
      "    : text { key = \"hoved\"; width = 100; }"
      "    : text { key = \"hoved2\"; width = 100; }"
      "  }"
      "  : boxed_column { label = \"Mulige buer - klik en linje\";"
      "    : list_box { key = \"liste\"; height = 10; width = 100; tabs = \"11 20 30 36 47 57 67 77\"; }"
      "  }"
      "  : row {"
      "    : boxed_column { label = \"Tegning af buen\";"
      "      : image { key = \"billede\"; width = 64; height = 17; color = 255; }"
      "      : text { key = \"legende\"; width = 64; }"
      "    }"
      "    : boxed_column { label = \"Valgt bue\";")
    (write-line l f))
  (foreach i '("1" "2" "3" "4" "5" "6" "7" "8")
    (write-line (strcat "      : row { : text { key = \"n" i "\"; width = 15; }"
                        " : image { key = \"p" i "\"" prik
                        " : text { key = \"v" i "\"; width = 22; } }")
                f))
  (foreach l
    (list "      spacer;"
          (strcat "      : row { : image { key = \"p9\"" prik " : text { key = \"v9\"; width = 36; } }")
          "    }"
          "  }"
          "  : row {"
          "    : button { key = \"indst\"; label = \"Indstillinger og stentype...\"; width = 30; fixed_width = true; }"
          "    : spacer { width = 1; }"
          "    : button { key = \"accept\"; label = \"Tegn buen\"; is_default = true; width = 16; fixed_width = true; }"
          "    : button { key = \"cancel\"; label = \"Annuller\"; is_cancel = true; width = 14; fixed_width = true; }"
          "  }"
          "}")
    (write-line l f))
  (close f)
  fn
)

;; Billede af segmentbuen. Lokalt: (0, 0) = overkant bue i midten.
;; Hvid mur med grå langfuger, åbningen i dialogens grå, teglrøde sten
;; (skiftevis: hver 2. sten er 2 halve). Murstenene står ikke i buen og
;; fugen langs toppen. Grønne linjer = overkant og den langfuge
;; hjørnerne skal ramme.
(defun sb:seg-billede (x s / bw bh D res n gam r beta h k ri re c yv a2 half hs delt mid
                         hx ymin ytop sk oy y0 pt lin i phi y)
  (setq bw   (dimx_tile "billede")
        bh   (dimy_tile "billede")
        D    *sb-dybde*
        h    (cadr x)  k (car x)  res (caddr x)
        n    (nth 0 res) gam (nth 1 res) r (nth 4 res) beta (nth 6 res)
        ri   r  re (+ r D)
        c    (list 0.0 (- (+ r D)))
        yv   (- (+ h D))                                   ; vederlaget
        a2   (atan (- r h) (/ s 2.0))
        half (/ *sb-stenh* 2.0)
        hs   (/ (- D *sb-delfuge*) 2.0)
        delt (and (= *sb-type* "Skiftevis") (>= hs 90.0))
        mid  (/ (1- n) 2)
        hx   (+ (* re (cos a2)) 60.0)
        ymin (- (* 1.25 yv) 20.0)
        ytop (+ *sb-topfuge* 30.0)
        sk   (min (/ (- bw 12.0) (* 2.0 hx)) (/ (- bh 12.0) (- ytop ymin)))
        oy   (+ (/ bh 2.0) (* sk (/ (+ ymin ytop) 2.0)))    ; buen midt i billedet
        hx   (/ bw 2.0 sk)                                 ; billedets kanter
        y0   (/ (- oy bh) sk))
  (defun pt (p) (list (+ (/ bw 2.0) (* (car p) sk)) (- oy (* (cadr p) sk))))
  (defun lin (p q farve) (bill:streg (pt p) (pt q) bw bh farve))
  (start_image "billede")
  (fill_image 0 0 bw bh 255)
  ;; murens langfuger - hvert skifte fra overkant
  (setq y (* (1+ (fix (/ oy sk *sb-skifte*))) *sb-skifte*))
  (while (> y y0)
    (lin (list (- hx) y) (list hx y) 254)
    (setq y (- y *sb-skifte*)))
  ;; åbningen
  (bill:fyld (mapcar 'pt (append (list (list (/ s 2.0) y0))
                                 (bill:bue c ri a2 (- pi a2) 16)
                                 (list (list (- (/ s 2.0)) y0))))
             bw bh -15)
  ;; overkant og den langfuge hjørnerne skal ramme
  (lin (list (- hx) 0.0) (list hx 0.0) 92)
  (lin (list (- hx) (- (* k *sb-skifte*))) (list hx (- (* k *sb-skifte*))) 92)
  ;; buen og fugen langs toppen står fri af murstenene
  (bill:fyld (mapcar 'pt (append (bill:bue c ri a2 (- pi a2) 16)
                                 (bill:bue c (+ re *sb-topfuge*) (- pi a2) a2 16)))
             bw bh 255)
  ;; åbningens sider og de skrå vederlag
  (lin (list (/ s 2.0) yv) (list (/ s 2.0) y0) 252)
  (lin (list (- (/ s 2.0)) yv) (list (- (/ s 2.0)) y0) 252)
  (lin (list (/ s 2.0) yv) (polar c a2 (+ re *sb-topfuge*)) 252)
  (lin (list (- (/ s 2.0)) yv) (polar c (- pi a2) (+ re *sb-topfuge*)) 252)
  ;; stenene
  (setq i 0)
  (while (< i n)
    (setq phi (+ a2 (* gam (1+ i)) (* beta (+ i 0.5))))
    (if (and delt (= (rem (abs (- i mid)) 2) 0))
      (progn
        (bill:sten (mapcar 'pt (sb:seg-sten-pl c phi ri (+ ri hs) half)) bw bh 22 24)
        (bill:sten (mapcar 'pt (sb:seg-sten-pl c phi (+ ri hs *sb-delfuge*) re half)) bw bh 22 24))
      (bill:sten (mapcar 'pt (sb:seg-sten-pl c phi ri re half)) bw bh 22 24))
    (setq i (1+ i)))
  (end_image)
)

;; Hjørnerne af én sten i billedet: bredde 2 x half, fra radius r0 til r1
(defun sb:seg-sten-pl (c phi r0 r1 half / d0 d1)
  (setq d0 (sb:asin (/ half r0))
        d1 (sb:asin (/ half r1)))
  (list (polar c (+ phi d0) r0) (polar c (+ phi d1) r1)
        (polar c (- phi d1) r1) (polar c (- phi d0) r0))
)

;; Eksempel-bue til indstillingerne, før vederlagene er klikket: den der
;; overholder reglerne med stik tættest på 1/8 af spændvidden
(defun sb:seg-eksempel (s / x ok okx k)
  (foreach k (car (sb:seg-kandidater s 0.0 0.0))
    (setq ok  (= (sb:seg-fejl (caddr k)) 0.0)
          okx (and x (= (sb:seg-fejl (caddr x)) 0.0)))
    (if (or (not x)
            (and ok (not okx))
            (and (eq ok okx)
                 (if ok
                   (< (abs (- (cadr k) (/ s 8.0))) (abs (- (cadr x) (/ s 8.0))))
                   (< (sb:seg-fejl (caddr k)) (sb:seg-fejl (caddr x))))))
      (setq x k)))
  x
)

;; Vis den valgte linje i tabellen "Valgt bue" og i tegningen.
;; Prikkerne: grøn = overholder reglen, rød = gør ikke.
(defun sb:seg-vis-valgt (nr / x res ok okb okt)
  (setq nr (atoi nr))
  (if (> nr 0)
    (progn
      (setq x   (nth (1- nr) *sb-kand*) res (caddr x)
            okb (>= (nth 2 res) *sb-minfuge*)
            okt (<= (nth 3 res) *sb-maxfuge*)
            ok  (= (sb:seg-fejl res) 0.0))
      (set_tile "v1" (strcat (itoa (car x)) (if (= (car x) 1) " skifte" " skifter") " under overkant"))
      (set_tile "v2" (strcat (sb:tal (cadr x) 0) " mm"))
      (set_tile "v3" (strcat (sb:tal (nth 4 res) 0) " mm"))
      (set_tile "v4" (strcat (itoa (car res)) "   (slutsten nr. " (itoa (1+ (/ (1- (car res)) 2))) ")"))
      (set_tile "v5" (strcat (sb:tal (nth 2 res) 1) " mm   (mindst " (sb:tal *sb-minfuge* 0) ")"))
      (set_tile "v6" (strcat (sb:tal (nth 3 res) 1) " mm   (højst " (sb:tal *sb-maxfuge* 0) ")"))
      (set_tile "v7" (strcat (if (> (- (cadddr x) *sb-yv0*) 0) "+" "")
                             (sb:tal (- (cadddr x) *sb-yv0*) 0) " mm"))
      (set_tile "v8" (strcat (sb:tal (* 180.0 (/ (nth 5 res) pi)) 1) " grader"))
      (set_tile "v9" (if ok "Overholder alle regler" "Overholder IKKE alle regler"))
      (bill:prik "p5" (if okb 94 12))
      (bill:prik "p6" (if okt 94 12))
      (bill:prik "p9" (if ok 94 12))
      (sb:seg-billede x *sb-s*))
    (progn
      (set_tile "liste" (itoa (1+ *sb-forv*)))
      (sb:seg-vis-valgt (itoa (1+ *sb-forv*)))))
)

;; Vælg løsning. Returnerer kandidaten, 'indst eller nil (annuller).
(defun sb:seg-vaelg (kand valgt bedst s yV0 / forv fn id svar x i)
  (setq forv (if valgt valgt bedst) i 0)
  (foreach k kand (if (eq k forv) (setq *sb-forv* i)) (setq i (1+ i)))
  (setq fn (sb:seg-dcl-fil)
        id (load_dialog fn))
  (if (or (< id 0) (not (new_dialog "sb_segvalg" id)))
    ;; ingen dialog - som før i kommandolinjen
    (progn
      (if (>= id 0) (unload_dialog id))
      (vl-file-delete fn)
      (if valgt
        valgt
        (progn
          (princ "\n\nINGEN løsning overholder alle regler med denne spændvidde og buedybde:")
          (princ (strcat "\n  " (sb:seg-overskrift nil)))
          (foreach x kand
            (if (< (car x) 9) (princ (strcat "\n  " (sb:seg-linje x yV0 (eq x bedst) nil)))))
          (initget "Brug Annuller")
          (if (= (getkword "\nTegn den bedste løsning alligevel? [Brug/Annuller] <Annuller>: ") "Brug")
            bedst))))
    (progn
      (setq *sb-kand* kand *sb-s* s *sb-yv0* yV0 *sb-valg* nil)
      (set_tile "hoved"
        (strcat "Spændvidde " (sb:tal s 0) " mm   Buedybde " (sb:tal *sb-dybde* 0)
                " mm   Sten: " *sb-type* "   Skifte " (sb:tal *sb-skifte* 2)
                " mm   Fuge " (sb:tal *sb-minfuge* 0) "-" (sb:tal *sb-maxfuge* 0) " mm"))
      (set_tile "hoved2"
        (if valgt
          "Forslag = overholder alle regler og ligger tættest på de vederlag du klikkede."
          "INGEN løsning overholder alle regler - forslaget er den der er tættest på."))
      (set_tile "legende" "Rød = sten    Grøn = overkant og den langfuge hjørnerne skal ramme")
      (mapcar 'set_tile '("n1" "n2" "n3" "n4" "n5" "n6" "n7" "n8")
              '("Hjørner:" "Stik:" "Radius:" "Antal sten:" "Fuge i bund:" "Fuge i top:"
                "Vederlag flyttet:" "Buens vinkel:"))
      (start_list "liste")
      (add_list (sb:seg-overskrift T))
      (foreach x kand (add_list (sb:seg-linje x yV0 (eq x forv) T)))
      (end_list)
      (set_tile "liste" (itoa (1+ *sb-forv*)))
      (sb:seg-vis-valgt (itoa (1+ *sb-forv*)))
      (action_tile "liste"  "(sb:seg-vis-valgt $value) (if (= $reason 4) (progn (setq *sb-valg* $value) (done_dialog 1)))")
      (action_tile "accept" "(setq *sb-valg* (get_tile \"liste\")) (done_dialog 1)")
      (action_tile "indst"  "(done_dialog 3)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq svar (start_dialog))
      (unload_dialog id)
      (vl-file-delete fn)
      (cond
        ((= svar 1)
         (setq x (atoi *sb-valg*))
         (if (< x 1) forv (nth (1- x) kand)))
        ((= svar 3) 'indst)
        (T nil))))
)

(defun c:SEGBUE (/ *error* doc in x1 x2 yV0 yT z s k h res kand valgt bedst
                   yV m p1 p2 n gam fi fe r tot beta ri re half c a1 a2
                   b ents hs skiftevis mid antdelt i phi gnavn svar trimres omrids topf kv)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )

  (if (not (setq in (sb:input "SEGMENTBUE" nil))) (exit))
  (setq x1 (nth 0 in) x2 (nth 1 in) yV0 (nth 2 in) yT (nth 3 in) z (nth 4 in)
        s  (- x2 x1))

  ;; Løsninger (hjørnerne 1, 2, 3 ... skifter under overkant) - vælges
  ;; i en dialog med forhåndsvisning. Indstillinger regner forfra.
  (setq valgt 'indst)
  (while (eq valgt 'indst)
    (setq kv (sb:seg-kandidater s yT yV0))
    (if (not (car kv))
      (progn (princ "\nIngen mulig bue - tjek at overkant ligger over vederlagene.") (exit)))
    (setq *sb-vis* (list "SEGMENTBUE" s yT yV0))          ; buen vises i indstillingerne
    (setq valgt (sb:seg-vaelg (car kv) (cadr kv) (caddr kv) s yV0))
    (if (eq valgt 'indst) (sb:indst-dialog)))
  (if (not valgt) (exit))

  ;; --- Geometri for valgt løsning ---
  (setq k     (nth 0 valgt)
        h     (nth 1 valgt)
        res   (nth 2 valgt)
        yV    (nth 3 valgt)
        n     (nth 0 res)  gam (nth 1 res)  fi (nth 2 res)  fe (nth 3 res)
        r     (nth 4 res)  tot (nth 5 res)  beta (nth 6 res)
        ri    r
        re    (+ r *sb-dybde*)
        half  (/ *sb-stenh* 2.0)
        p1    (list x1 yV z)
        p2    (list x2 yV z)
        m     (list (/ (+ x1 x2) 2.0) yV z)
        c     (polar m (* -0.5 pi) (- r h))
        a1    (angle c p1)
        a2    (angle c p2))

  ;; --- Tegn ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  (setq b (sb:tan (/ tot 4.0)))
  (setq omrids (sb:pl (list p2 p1 (polar c a1 re) (polar c a2 re))
                      (list b 0.0 (- b) 0.0) T)
        ents   (list omrids)
        topf   nil)

  (setq skiftevis (sb:skiftevis-p)
        hs        (/ (- *sb-dybde* *sb-delfuge*) 2.0)
        mid       (/ (1- n) 2)
        antdelt   0
        i         0)
  (while (< i n)
    (setq phi (+ a2 (* gam (1+ i)) (* beta (+ i 0.5))))
    (if (and skiftevis (= (rem (abs (- i mid)) 2) 0))
      (setq ents (cons (sb:seg-sten c phi ri (+ ri hs) half) ents)
            ents (cons (sb:seg-sten c phi (+ ri hs *sb-delfuge*) re half) ents)
            antdelt (1+ antdelt))
      (setq ents (cons (sb:seg-sten c phi ri re half) ents)))
    (setq i (1+ i)))

  ;; Fuge langs toppen af buen (fra hjørne til hjørne)
  (if (> *sb-topfuge* 0.0)
    (setq topf (sb:pl (list (polar c a1 re)
                            (polar c a2 re)
                            (polar c a2 (+ re *sb-topfuge*))
                            (polar c a1 (+ re *sb-topfuge*)))
                      (list (- b) 0.0 b 0.0)
                      T)
          ents (cons topf ents)))

  ;; Trim murstenene bag buen
  (initget "Ja Nej")
  (if (/= (getkword "\nTrim murstenene bag buen? [Ja/Nej] <Ja>: ") "Nej")
    (progn
      (setq trimres (sb:trim doc (list (list c ri (+ re *sb-topfuge*) a2 a1)) ents))
      ;; Efter trim: fjern topfugens streger og omridsets buer i top
      ;; og bund, så fugerne mellem stenene står åbne. Kun
      ;; vederlagslinjerne i siderne bliver.
      (setq ents (vl-remove omrids ents))
      (entdel omrids)
      (if topf (progn (setq ents (vl-remove topf ents)) (entdel topf)))
      (setq ents (cons (sb:linje p1 (polar c a1 re)) ents)
            ents (cons (sb:linje p2 (polar c a2 re)) ents))
    )
  )

  (setq gnavn (vl-catch-all-apply 'sb:gruppe (list doc ents "SEGBUE")))
  ;; gem buens mål i tegningen, så BUEMAAL kan finde dem
  (vl-catch-all-apply 'sb:data-gem
    (list gnavn "SEG" (append (trans c 1 0) (list ri re a2 a1 *sb-topfuge*)
                              (trans p1 1 0) (trans p2 1 0) (list h k n s *sb-dybde* *sb-stenh*))))
  (vla-EndUndoMark doc)

  ;; --- Resultat ---
  (princ "\n==================== SEGMENTBUE ====================")
  (princ (strcat "\n  Spændvidde:              " (sb:tal s 0) " mm"))
  (princ (strcat "\n  Stik:                    " (sb:tal h 0) " mm"))
  (princ (strcat "\n  Radius (underkant):      " (sb:tal ri 0) " mm"))
  (princ (strcat "\n  Vederlag flyttet:        " (if (> (- yV yV0) 0) "+" "")
                 (sb:tal (- yV yV0) 0) " mm i forhold til klik"))
  (princ (strcat "\n  Øverste hjørner:         " (itoa k) " skifte(r) under overkant - på langfuge"))
  (princ (strcat "\n  Antal sten:              " (itoa n)
                 "  (slutsten = nr. " (itoa (1+ mid)) ")"))
  (if skiftevis
    (princ (strcat "\n  Stentype:                Skiftevis - " (itoa antdelt) " delte, "
                   (itoa (- n antdelt)) " hele"))
    (princ "\n  Stentype:                Hele sten"))
  (princ (strcat "\n  Fuge i bund:             " (sb:tal fi 1) " mm  (min "
                 (sb:tal *sb-minfuge* 0) ")  " (if (< fi *sb-minfuge*) "IKKE OK" "OK")))
  (princ (strcat "\n  Fuge i top:              " (sb:tal fe 1) " mm  (max "
                 (sb:tal *sb-maxfuge* 0) ")  " (if (> fe *sb-maxfuge*) "IKKE OK" "OK")))
  (if trimres
    (progn
      (princ (strcat "\n  Trimmet omkring buen:    " (itoa (car trimres)) " trimmet, "
                     (itoa (cadr trimres)) " slettet"))
      (princ (strcat "\n  Finish:                  " (itoa (caddr trimres)) " dobbelte sten, "
                     (itoa (cadddr trimres)) " småstumper fjernet"))))
  (princ (strcat "\n  Fuge langs toppen:       "
                 (if (> *sb-topfuge* 0.0) (strcat (sb:tal *sb-topfuge* 1) " mm") "ingen")))
  (princ (strcat "\n  Gruppe:                  "
                 (if (vl-catch-all-error-p gnavn) (strcat "kunne ikke oprettes: " (vl-catch-all-error-message gnavn)) gnavn)))
  (princ "\n====================================================")
  (princ)
)

;; Segmentbue-sten mellem radius r1 og r2 på retningen phi.
;; Siderne er lige (parallelle med radius), men stenen trimmes
;; i top og bund, så den følger buen og fugen langs toppen.
(defun sb:seg-sten (c phi r1 r2 half / da1 da2)
  (setq da1 (sb:asin (/ half r1))
        da2 (sb:asin (/ half r2)))
  (sb:pl (list (polar c (+ phi da1) r1)
               (polar c (+ phi da2) r2)
               (polar c (- phi da2) r2)
               (polar c (- phi da1) r1))
         (list 0.0 (- (sb:tan (/ da2 2.0))) 0.0 (sb:tan (/ da1 2.0)))
         T)
)

;; ============================================================
;; KURVEHANKSBUE (3 eller 5 centre)
;; ============================================================

;; Buerne fra venstre vederlag (-a, 0) mod højre. angs = vinkler for
;; overgangene (fra pi ned til 0), rads = radius for hvert stykke.
;; Hvert nyt centrum ligger på linjen gennem det forrige centrum og
;; overgangspunktet, så buerne har fælles tangent (ingen knæk).
;; Returnerer liste af (cx cy radius startvinkel slutvinkel), lokalt
;; med vederlagets midte som (0, 0).
(defun kb:buer (a angs rads / px py lst a0 a1 cx cy)
  (setq px (- a) py 0.0)
  (foreach r rads
    (setq a0 (car angs) a1 (cadr angs) angs (cdr angs)
          cx (- px (* r (cos a0)))
          cy (- py (* r (sin a0)))
          lst (cons (list cx cy r a0 a1) lst)
          px (+ cx (* r (cos a1)))
          py (+ cy (* r (sin a1)))))
  (reverse lst)
)

;; 5 centre: forhold pilhøjde/halv spændvidde når radierne vokser med
;; faktoren q fra side mod top (r, q*r, q*q*r, q*r, r), 36 grader hver
(defun kb:5-forhold (q / c1 c2 c3 s1 s2 s3)
  (setq c1 (+ 1.0 (cos (* 0.8 pi)))
        c2 (- (cos (* 0.6 pi)) (cos (* 0.8 pi)))
        c3 (- (cos (* 0.6 pi)))
        s1 (sin (* 0.8 pi))
        s2 (- (sin (* 0.6 pi)) (sin (* 0.8 pi)))
        s3 (- 1.0 (sin (* 0.6 pi))))
  (/ (+ s1 (* q s2) (* q q s3)) (+ c1 (* q c2) (* q q c3)))
)

;; Geometri for halv spændvidde a og pilhøjde b (underkant).
;;  Pilhøjde >= 1/4 af spændvidden: 3 centre. Sidebuer på 60 grader
;;    når pilhøjden tillader det, ellers fladere.
;;  Pilhøjde <  1/4 af spændvidden: 5 centre. 5 buer på hver 36 grader,
;;    radierne vokser med samme faktor fra siderne mod toppen.
;; Returnerer listen af buer (se kb:buer) eller nil hvis umulig.
(defun kb:geo (a b / tt phi r1 d lo hi md i q)
  (cond
    ((>= (/ b (* 2.0 a)) 0.25)
     (setq tt  (min 0.267949 (* 0.5 (/ b a)))
           phi (- (/ pi 2.0) (* 2.0 (atan tt)))
           r1  (/ (- b (* a tt)) (- 1.0 tt))
           d   (/ (- a r1) (cos phi)))
     (kb:buer a (list pi (- pi phi) phi 0.0) (list r1 (+ r1 d) r1)))
    ((> (/ b a) (+ (kb:5-forhold 1000.0) 0.001))
     ;; find q ved halvering (forholdet falder når q vokser)
     (setq lo 1.0 hi 1000.0 i 0)
     (while (< i 100)
       (setq md (sqrt (* lo hi)))
       (if (> (kb:5-forhold md) (/ b a)) (setq lo md) (setq hi md))
       (setq i (1+ i)))
     (setq q  (sqrt (* lo hi))
           r1 (/ a (+ (+ 1.0 (cos (* 0.8 pi)))
                      (* q (- (cos (* 0.6 pi)) (cos (* 0.8 pi))))
                      (* q q (- (cos (* 0.6 pi)))))))
     (kb:buer a (list pi (* 0.8 pi) (* 0.6 pi) (* 0.4 pi) (* 0.2 pi) 0.0)
                (list r1 (* q r1) (* q q r1) (* q r1) r1)))
  )
)

;; Punkt på oversiden målt med buelængde u fra venstre vederlag.
;; Returnerer (cx cy radius vinkel) - radius er undersidens radius.
(defun kb:pos (u geo a / res l)
  (foreach bue geo
    (if (not res)
      (progn
        (setq l (* (- (nth 3 bue) (nth 4 bue)) (+ (nth 2 bue) *sb-dybde*)))
        (if (or (<= u l) (eq bue (last geo)))
          (setq res (list (car bue) (cadr bue) (nth 2 bue)
                          (- (nth 3 bue) (/ u (+ (nth 2 bue) *sb-dybde*)))))
          (setq u (- u l))))))
  res
)

;; Hjørne ved en fuge: side +1 = venstre kant, -1 = højre kant.
;; off = afstand fra undersiden (0 = underside, dybde = overside).
;; Fugen holdes lige bred hele vejen (stenene kileskæres).
;; Returnerer (x y vinkel cx cy)
(defun kb:hj (jp off side j / rho th)
  (setq rho (+ (nth 2 jp) off)
        th  (+ (nth 3 jp) (* side (sb:asin (/ (/ j 2.0) rho)))))
  (list (+ (car jp) (* rho (cos th)))
        (+ (cadr jp) (* rho (sin th)))
        th (car jp) (cadr jp))
)

;; Bulge mellem to hjørner hvis de har samme centrum
(defun kb:bulge (h1 h2)
  (if (and (equal (nth 3 h1) (nth 3 h2) 1e-6) (equal (nth 4 h1) (nth 4 h2) 1e-6))
    (sb:tan (/ (- (nth 2 h2) (nth 2 h1)) 4.0))
    0.0)
)

;; Tegn én kurvehanks-sten mellem fuge ja og jb, fra off1 til off2
;; Fugens bredde i afstanden off fra undersiden: w i bunden, j i toppen
(defun kb:fb (w j off)
  (+ w (* (- j w) (/ off *sb-dybde*)))
)

;; Tegn én sten mellem fuge ja og jb, fra off1 til off2.
;; j = fuge i toppen, wa/wb = fugernes bredde i bunden (kileformede fuger)
;; Stenens 4 hjørner (lokalt): underside fuge a, overside fuge a,
;; overside fuge b, underside fuge b
(defun kb:hjoerner (ja jb off1 off2 j wa wb)
  (list (kb:hj ja off1 -1 (kb:fb wa j off1))
        (kb:hj ja off2 -1 (kb:fb wa j off2))
        (kb:hj jb off2  1 (kb:fb wb j off2))
        (kb:hj jb off1  1 (kb:fb wb j off1)))
)

(defun kb:sten (m ja jb off1 off2 j wa wb / q q1 q2 q3 q4)
  (setq q  (kb:hjoerner ja jb off1 off2 j wa wb)
        q1 (nth 0 q) q2 (nth 1 q) q3 (nth 2 q) q4 (nth 3 q))
  (sb:pl (mapcar '(lambda (q) (sb:lok m (car q) (cadr q))) (list q1 q2 q3 q4))
         (list 0.0 (kb:bulge q2 q3) 0.0 (kb:bulge q4 q1))
         T)
)

;; Buelængde på oversiden
(defun kb:laengde (geo / l)
  (setq l 0.0)
  (foreach g geo
    (setq l (+ l (* (- (nth 3 g) (nth 4 g)) (+ (nth 2 g) *sb-dybde*)))))
  l
)

;; Inddeling på oversiden: hele sten + ens fuge i toppen. Ulige antal.
;; Som ved segmentbuen vælges flest mulige sten, men fugen i toppen skal
;; være så bred, at stenene i den store topbue kan stå hele (54 mm) med
;; mindst min-fugen (7 mm) i bunden - kun sidebuernes sten kileskæres.
;; Fugen i toppen er mindst murens lejefuge og højst max-fugen (20 mm).
;; Kan det ikke lade sig gøre, vælges den bredeste fuge der er tilladt.
;; Returnerer (antal fuge) eller nil.
(defun kb:inddel (geo / le rmax maal n0 n tst bedstn bedstj)
  (setq le   (kb:laengde geo)
        rmax 0.0)
  (foreach g geo (setq rmax (max rmax (nth 2 g))))
  (setq maal (max (- *sb-skifte* *sb-stenh*)
                  (- (* (+ *sb-stenh* *sb-minfuge*) (/ (+ rmax *sb-dybde*) rmax)) *sb-stenh*))
        n0 (fix (/ (- le maal) (+ *sb-stenh* maal)))
        n  (max 1 (- n0 8)))
  (if (= (rem n 2) 0) (setq n (1+ n)))
  (while (<= n (+ n0 8))
    (setq tst (/ (- le (* n *sb-stenh*)) (1+ n)))
    (if (and (>= tst *sb-minfuge*) (<= tst *sb-maxfuge*)
             (or (not bedstn)
                 ;; bedst: når målet og så smal som muligt, ellers så bred som muligt
                 (if (>= bedstj (- maal 1e-6))
                   (and (>= tst (- maal 1e-6)) (< tst bedstj))
                   (> tst bedstj))))
      (setq bedstn n bedstj tst))
    (setq n (+ n 2)))
  (if bedstn (list bedstn bedstj))
)

;; Fugerne i en kurvehanksbue med n sten og fugen j i toppen.
;; Inddelt på oversiden med hele sten. Fugen i bunden er mindst
;; min-fugen (7 mm) og aldrig bredere end i toppen. Den gøres så bred,
;; at stenen ikke skal være over 54 mm i bunden - ellers kileskæres
;; stenen. Yderste fuger ved vederlaget beholder bredden fra toppen.
;; Returnerer (fuger bundbredder fuge-bund-min fuge-bund-max
;;             antal-kileskårne smalleste-sten)
(defun kb:fuger (geo n j a / i fl bp dl wl w fbmin fbmax antkile minb q bredde)
  (setq i 0)
  (while (<= i n)
    (setq fl (cons (kb:pos (+ (* i (+ *sb-stenh* j)) (/ j 2.0)) geo a) fl)
          i  (1+ i)))
  (setq fl (reverse fl))
  ;; afstand mellem fugernes midte i bunden (for hver sten)
  (setq bp (mapcar '(lambda (jp) (kb:hj jp 0.0 1 0.0)) fl)
        dl (mapcar '(lambda (p q) (distance (list (car p) (cadr p)) (list (car q) (cadr q))))
                   bp (cdr bp)))
  (setq i 0 fbmin 1e9 fbmax 0.0)
  (while (<= i n)
    (setq w (if (or (= i 0) (= i n))
              j
              (min j (max *sb-minfuge*
                          (- (nth (1- i) dl) *sb-stenh*)
                          (- (nth i dl) *sb-stenh*))))
          wl (cons w wl))
    (if (and (> i 0) (< i n)) (setq fbmin (min fbmin w) fbmax (max fbmax w)))
    (setq i (1+ i)))
  (setq wl (reverse wl))
  (if (> fbmin 1e8) (setq fbmin j fbmax j))
  ;; stenenes bredde i bunden
  (setq i 0 antkile 0 minb 1e9)
  (while (< i n)
    (setq q (kb:hjoerner (nth i fl) (nth (1+ i) fl) 0.0 0.0 j (nth i wl) (nth (1+ i) wl))
          bredde (distance (list (car (nth 0 q)) (cadr (nth 0 q)))
                           (list (car (nth 3 q)) (cadr (nth 3 q))))
          minb (min minb bredde))
    (if (< bredde (- *sb-stenh* 0.5)) (setq antkile (1+ antkile)))
    (setq i (1+ i)))
  (list fl wl fbmin fbmax antkile minb)
)

;; Mulige overkanter ud fra vederlagene (a = halv spændvidde).
;; Overkant altid et helt antal skifter over vederlaget.
;; Pilhøjde under 1/5 af spændvidden tages ikke med (for flad).
;; Returnerer liste af
;;  (skifter pilhøjde antal-sten fuge-top antal-centre
;;   fuge-bund-min fuge-bund-max antal-kileskårne geo fuger bundbredder smalleste)
(defun kb:kandidater (a / k b g ind fu lst)
  (setq k 1)
  (while (<= (setq b (- (* k *sb-skifte*) *sb-dybde*)) (+ a 1e-6))
    (if (and (> b 0.0) (>= b (/ (* 2.0 a) 5.0))
             (setq g (kb:geo a b))
             (setq ind (kb:inddel g)))
      (progn
        (setq fu (kb:fuger g (car ind) (cadr ind) a))
        (setq lst (cons (list k b (car ind) (cadr ind) (length g)
                              (nth 2 fu) (nth 3 fu) (nth 4 fu) g (nth 0 fu) (nth 1 fu) (nth 5 fu))
                        lst))))
    (setq k (1+ k)))
  (reverse lst)
)

;; Forslaget er den klassiske 3-centerbue med 3 buer på 60 grader:
;; pilhøjde = spændvidde x tan(15) = ca. 0,27 x spændvidde.
;; Returnerer nummeret (0, 1, ...) på den bedste kandidat.
(defun kb:bedste (a kand / maal i nr)
  (setq maal (* 2.0 a 0.267949) i 0)
  (foreach x kand
    (if (or (not nr) (< (abs (- (cadr x) maal)) (abs (- (cadr (nth nr kand)) maal))))
      (setq nr i))
    (setq i (1+ i)))
  nr
)

;; Fyld tekst ud med mellemrum til bredden n
(defun kb:pad (s n)
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)

;; Fugen i bunden som tekst: "7,0" eller "7,0-9,6"
(defun kb:bundtekst (x)
  (if (equal (nth 5 x) (nth 6 x) 0.05)
    (sb:tal (nth 5 x) 1)
    (strcat (sb:tal (nth 5 x) 1) "-" (sb:tal (nth 6 x) 1)))
)

;; Overskrift og én linje i listen
(defun kb:overskrift ()
  (strcat (kb:pad "Overkant" 20) (kb:pad "Pilhøjde" 10) (kb:pad "Pil/sp." 8)
          (kb:pad "Cirkler" 8) (kb:pad "Sten" 6) (kb:pad "Fuge top" 10)
          (kb:pad "Fuge bund" 11) "Kilesk.")
)
(defun kb:linje (x a bedst)
  (strcat (kb:pad (strcat (itoa (car x)) " sk. (" (sb:tal (* (car x) *sb-skifte*) 0) " mm)") 20)
          (kb:pad (strcat (sb:tal (cadr x) 0) " mm") 10)
          (kb:pad (strcat "1:" (sb:tal (/ (* 2.0 a) (cadr x)) 1)) 8)
          (kb:pad (itoa (nth 4 x)) 8)
          (kb:pad (itoa (nth 2 x)) 6)
          (kb:pad (strcat (sb:tal (nth 3 x) 1) " mm") 10)
          (kb:pad (strcat (kb:bundtekst x) " mm") 11)
          (kb:pad (itoa (nth 7 x)) 5)
          (if bedst "<- forslag" ""))
)

;; Skriv dialogen til en midlertidig DCL-fil. Returnerer filnavnet.
;; (Ingen æ, ø og å i filen, så den virker i alle AutoCAD-versioner.)
(defun kb:dcl-fil (/ fn f)
  (setq fn (vl-filename-mktemp "kurvebue" nil ".dcl")
        f  (open fn "w"))
  (foreach l
    '("kb_forslag : dialog {"
      "  label = \"Kurvehanksbue - vaelg overkant\";"
      "  : boxed_column { label = \"Aabning og indstillinger\";"
      "    : text { key = \"hoved\"; width = 92; }"
      "    : text { key = \"hoved2\"; width = 92; }"
      "  }"
      "  : boxed_column { label = \"Mulige overkanter - vaelg en linje\";"
      "    : list_box { key = \"liste\"; height = 12; width = 92; fixed_width_font = true; }"
      "  }"
      "  : row {"
      "    : boxed_column { label = \"Forhaandsvisning\";"
      "      : image { key = \"billede\"; width = 58; height = 17; color = 255; }"
      "      : text { key = \"legende\"; width = 58; }"
      "    }"
      "    : boxed_column { label = \"Valgt bue\";"
      "      : text { key = \"v1\"; width = 32; }"
      "      : text { key = \"v2\"; width = 32; }"
      "      : text { key = \"v3\"; width = 32; }"
      "      : text { key = \"v4\"; width = 32; }"
      "      : text { key = \"v5\"; width = 32; }"
      "      : text { key = \"v6\"; width = 32; }"
      "      : text { key = \"v7\"; width = 32; }"
      "      : text { key = \"v8\"; width = 32; }"
      "      spacer_1;"
      "      : button { key = \"indst\"; label = \"Indstillinger og stentype...\"; }"
      "    }"
      "  }"
      "  spacer_1;"
      "  : row { alignment = centered; fixed_width = true;"
      "    : button { key = \"accept\"; label = \"Tegn buen\"; is_default = true; width = 18; }"
      "    : button { key = \"klik\"; label = \"Klik selv overkant\"; width = 22; }"
      "    : button { key = \"cancel\"; label = \"Annuller\"; is_cancel = true; width = 14; }"
      "  }"
      "}")
    (write-line l f))
  (close f)
  fn
)

;; Billede af kurvehanksbuen. Lokalt: (0, 0) = vederlagets midte.
;; Hvid mur med grå langfuger, åbningen i dialogens grå, teglrøde hele
;; sten, gule kileskårne (skiftevis: hver 2. sten er 2 halve),
;; + = cirklernes centre. Grønne linjer = vederlaget og overkant.
(defun kb:billede (x a / bw bh D tf g fl wl n ok yl sk oy hx y0 hs delt mid ind ud
                     pt lin i q farve y c del kr)
  (setq bw   (dimx_tile "billede")
        bh   (dimy_tile "billede")
        D    *sb-dybde*
        tf   *sb-topfuge*
        g    (nth 8 x) fl (nth 9 x) wl (nth 10 x) n (nth 2 x)
        ok   (* (car x) *sb-skifte*)                        ; overkant
        yl   (* 0.25 (+ (cadr x) D))                        ; mur under vederlaget
        sk   (min (/ (- bw 16.0) (* 2.0 (+ a D 40.0)))
                  (/ (- bh 12.0) (+ (cadr x) D yl 30.0)))
        oy   (+ (/ bh 2.0) (* sk (/ (- (+ (cadr x) D 30.0) yl) 2.0)))   ; buen midt i billedet
        hx   (/ bw 2.0 sk)
        y0   (/ (- oy bh) sk)
        kr   (/ 4.0 sk)                                     ; centrenes kryds: 4 pixels
        hs   (/ (- D *sb-delfuge*) 2.0)
        delt (and (= *sb-type* "Skiftevis") (>= hs 90.0))
        mid  (/ (1- n) 2)
        ;; undersiden og ydersiden (med fugen langs toppen) ved fugerne
        ind  (mapcar '(lambda (jp) (kb:hj jp 0.0 1 0.0)) fl)
        ud   (mapcar '(lambda (jp) (kb:hj jp (+ D tf) 1 0.0)) fl))
  (defun pt (p) (list (+ (/ bw 2.0) (* (car p) sk)) (- oy (* (cadr p) sk))))
  (defun lin (p q farve) (bill:streg (pt p) (pt q) bw bh farve))
  (start_image "billede")
  (fill_image 0 0 bw bh 255)
  ;; murens langfuger - vederlaget ligger på en langfuge
  (setq y (* (1+ (fix (/ oy sk *sb-skifte*))) *sb-skifte*))
  (while (> y y0)
    (lin (list (- hx) y) (list hx y) 254)
    (setq y (- y *sb-skifte*)))
  ;; åbningen
  (bill:fyld (mapcar 'pt (append (list (list (- a) y0) (list (- a) 0.0)) ind
                                 (list (list a 0.0) (list a y0))))
             bw bh -15)
  ;; vederlaget og overkant
  (lin (list (- hx) 0.0) (list hx 0.0) 92)
  (lin (list (- hx) ok) (list hx ok) 92)
  ;; buen og fugen langs toppen står fri af murstenene
  (bill:fyld (mapcar 'pt (append (list (list (- a) 0.0)) ind
                                 (list (list a 0.0) (list (+ a D tf) 0.0))
                                 (reverse ud)
                                 (list (list (- (+ a D tf)) 0.0))))
             bw bh 255)
  ;; åbningens sider
  (lin (list (- a) 0.0) (list (- a) y0) 252)
  (lin (list a 0.0) (list a y0) 252)
  ;; stenene
  (setq i 0)
  (while (< i n)
    (setq q     (kb:hjoerner (nth i fl) (nth (1+ i) fl) 0.0 D (nth 3 x) (nth i wl) (nth (1+ i) wl))
          farve (if (< (distance (list (car (nth 0 q)) (cadr (nth 0 q)))
                                 (list (car (nth 3 q)) (cadr (nth 3 q))))
                       (- *sb-stenh* 0.5))
                  40 22))
    (foreach del (if (and delt (= (rem (abs (- i mid)) 2) 0))
                   (list (list 0.0 hs) (list (+ hs *sb-delfuge*) D))
                   (list (list 0.0 D)))
      (bill:sten (mapcar 'pt (kb:hjoerner (nth i fl) (nth (1+ i) fl) (car del) (cadr del)
                                          (nth 3 x) (nth i wl) (nth (1+ i) wl)))
                 bw bh farve (if (= farve 22) 24 42)))
    (setq i (1+ i)))
  ;; cirklernes centre (kun dem der kan ses)
  (foreach c g
    (if (> (cadr c) y0)
      (progn
        (lin (list (- (car c) kr) (cadr c)) (list (+ (car c) kr) (cadr c)) 250)
        (lin (list (car c) (- (cadr c) kr)) (list (car c) (+ (cadr c) kr)) 250))))
  (end_image)
)

;; Vis den valgte linje: tekst og billede
(defun kb:vis-valgt (nr / x)
  (setq nr (atoi nr))
  (if (> nr 0)
    (progn
      (setq x (nth (1- nr) *kb-kand*))
      (set_tile "v1" (strcat "Overkant: " (itoa (car x)) " skifter (" (sb:tal (* (car x) *sb-skifte*) 0) " mm)"))
      (set_tile "v2" (strcat "Pilhøjde: " (sb:tal (cadr x) 0) " mm  (1 : "
                             (sb:tal (/ (* 2.0 *kb-a*) (cadr x)) 1) ")"))
      (set_tile "v3" (strcat "Opsnøring: " (itoa (nth 4 x)) " cirkler"))
      (set_tile "v4" (strcat "Antal sten: " (itoa (nth 2 x)) "  (slutsten nr. "
                             (itoa (1+ (/ (1- (nth 2 x)) 2))) ")"))
      (set_tile "v5" (strcat "Fuge i toppen: " (sb:tal (nth 3 x) 1) " mm  (max "
                             (sb:tal *sb-maxfuge* 0) ")"))
      (set_tile "v6" (strcat "Fuge i bunden: " (kb:bundtekst x) " mm  (min "
                             (sb:tal *sb-minfuge* 0) ")"))
      (set_tile "v7" (strcat "Kileskårne sten: " (itoa (nth 7 x)) " af " (itoa (nth 2 x))))
      (set_tile "v8" (strcat "Smalleste sten i bund: " (sb:tal (nth 11 x) 0) " mm"))
      (kb:billede x *kb-a*))
    ;; overskriften kan ikke vælges - gå til forslaget
    (progn
      (set_tile "liste" (itoa (1+ *kb-bedst*)))
      (kb:vis-valgt (itoa (1+ *kb-bedst*)))))
)

;; Forslag til overkant i en dialog.
;; Returnerer (ok y), (klik y-forslag), (indst), (type) eller nil (annuller).
(defun kb:forslag (a yV0 / kand bedst fn id svar x)
  (setq *sb-vis* (list "KURVEHANKSBUE" a)                  ; buen vises i indstillingerne
        kand  (kb:kandidater a)
        bedst (kb:bedste a kand))
  (if (not kand)
    (progn
      (alert (strcat "Ingen overkant giver en god kurvehanksbue med spændvidde "
                     (sb:tal (* 2.0 a) 0) " mm og buedybde " (sb:tal *sb-dybde* 0) " mm."
                     "\n\nKlik selv overkant, eller ændr indstillingerne."))
      (list 'klik nil))
    (progn
      (setq fn (kb:dcl-fil)
            id (load_dialog fn))
      (if (or (< id 0) (not (new_dialog "kb_forslag" id)))
        ;; ingen dialog mulig - vis tabellen i kommandolinjen
        (progn
          (if (>= id 0) (unload_dialog id))
          (vl-file-delete fn)
          (princ (strcat "\n  " (kb:overskrift)))
          (setq x 0)
          (foreach k kand
            (princ (strcat "\n  " (kb:linje k a (= x bedst))))
            (setq x (1+ x)))
          (list 'klik (+ yV0 (* (car (nth bedst kand)) *sb-skifte*))))
        (progn
          (setq *kb-kand* kand *kb-bedst* bedst *kb-valg* nil *kb-a* a)
          (set_tile "hoved"
            (strcat "Spændvidde " (sb:tal (* 2.0 a) 0) " mm   Buedybde " (sb:tal *sb-dybde* 0)
                    " mm   Sten: " *sb-type* "   Skifte " (sb:tal *sb-skifte* 2)
                    " mm   Fuge " (sb:tal *sb-minfuge* 0) "-" (sb:tal *sb-maxfuge* 0) " mm"))
          (set_tile "hoved2"
            (strcat "Forslag = klassisk 3-centerbue (pilhøjde ca. "
                    (sb:tal (* 2.0 a 0.267949) 0) " mm = 0,27 x spændvidde). "
                    "Under 1/4 af spændvidden bruges 5 cirkler."))
          (set_tile "legende" "Rød = hel sten    Gul = kileskåret    + = cirklernes centre    Grøn = vederlag og overkant")
          (start_list "liste")
          (add_list (kb:overskrift))
          (setq x 0)
          (foreach k kand
            (add_list (kb:linje k a (= x bedst)))
            (setq x (1+ x)))
          (end_list)
          (set_tile "liste" (itoa (1+ bedst)))
          (kb:vis-valgt (itoa (1+ bedst)))
          (action_tile "liste"  "(kb:vis-valgt $value) (if (= $reason 4) (progn (setq *kb-valg* $value) (done_dialog 1)))")
          (action_tile "accept" "(setq *kb-valg* (get_tile \"liste\")) (done_dialog 1)")
          (action_tile "klik"   "(done_dialog 2)")
          (action_tile "indst"  "(done_dialog 3)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq svar (start_dialog))
          (unload_dialog id)
          (vl-file-delete fn)
          (cond
            ((= svar 1)
             (setq x (atoi *kb-valg*))
             (if (< x 1) (setq x (1+ bedst)))
             (list 'ok (+ yV0 (* (car (nth (1- x) kand)) *sb-skifte*))))
            ((= svar 2) (list 'klik (+ yV0 (* (car (nth bedst kand)) *sb-skifte*))))
            ((= svar 3) (list 'indst))
            (T nil))
        )
      )
    )
  )
)

(defun c:KURVEBUE (/ *error* doc in x1 x2 yV0 yT z a k yV b geo
                     le n j tst m pts bul ents skiftevis hs mid
                     antdelt i ja jb q1 q4 bredde minb gnavn
                     ydre secs omrids topf trimres navne nr
                     fu fl wl wa wb fbmin fbmax antkile)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )

  (if (not (setq in (sb:input "KURVEHANKSBUE" 'kb:forslag))) (exit))
  (setq x1 (nth 0 in) x2 (nth 1 in) yV0 (nth 2 in) yT (nth 3 in) z (nth 4 in)
        a  (/ (- x2 x1) 2.0))

  ;; Vederlag (nederste lejefuge) flyttes til nærmeste langfuge
  (setq k  (fix (+ 0.5 (/ (- yT yV0) *sb-skifte*)))
        yV (- yT (* k *sb-skifte*))
        b  (- (* k *sb-skifte*) *sb-dybde*))          ; pilhøjde underkant

  (if (<= b 0.0)
    (progn (princ "\nOverkant ligger for tæt på vederlaget - der skal være mere end 1 sten imellem.") (exit)))
  (if (> b (+ a 1e-6))
    (progn (princ (strcat "\nBuen bliver højere end en halvcirkel (pilhøjde " (sb:tal b 0)
                          " mm > halv spændvidde " (sb:tal a 0) " mm). Vælg en lavere overkant."))
           (exit)))

  (if (not (setq geo (kb:geo a b)))
    (progn (princ (strcat "\nBuen er for flad til en kurvehanksbue (pilhøjde " (sb:tal b 0)
                          " mm). Vælg en højere overkant eller brug SEGBUE."))
           (exit)))
  (setq le (kb:laengde geo))

  ;; Inddeling på oversiden: hele sten (54) + lige store fuger
  (if (not (setq tst (kb:inddel geo)))
    (progn (princ "\nKan ikke inddele buen med fuger mellem 7 og 20 mm.") (exit)))
  (setq n (car tst) j (cadr tst))

  (setq m (list (/ (+ x1 x2) 2.0) yV z))

  ;; --- Tegn ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; Omrids: underside venstre->højre, overside højre->venstre
  (setq pts nil bul nil)
  (foreach g geo
    (setq pts (cons (polar (list (car g) (cadr g)) (nth 3 g) (nth 2 g)) pts)
          bul (cons (sb:tan (/ (- (nth 4 g) (nth 3 g)) 4.0)) bul)))
  (setq pts (cons (list a 0.0) pts)
        bul (cons 0.0 bul))
  (foreach g (reverse geo)
    (setq pts (cons (polar (list (car g) (cadr g)) (nth 4 g) (+ (nth 2 g) *sb-dybde*)) pts)
          bul (cons (sb:tan (/ (- (nth 3 g) (nth 4 g)) 4.0)) bul)))
  (setq pts (cons (list (- (+ a *sb-dybde*)) 0.0) pts)
        bul (cons 0.0 bul))
  (setq ents (list (sb:pl (mapcar '(lambda (p) (sb:lok m (car p) (cadr p))) (reverse pts))
                          (reverse bul) T))
        omrids (car ents))

  ;; Sten
  (setq skiftevis (sb:skiftevis-p)
        hs        (/ (- *sb-dybde* *sb-delfuge*) 2.0)
        mid       (/ (1- n) 2)
        antdelt   0
        minb      1e9
        antkile   0
        i         0)

  ;; Fugerne: hele sten og ens fuge j i toppen, kileformede fuger
  ;; (min. 7 mm i bunden). Kun de sten der skal, kileskæres.
  (setq fu      (kb:fuger geo n j a)
        fl      (nth 0 fu)  wl (nth 1 fu)
        fbmin   (nth 2 fu)  fbmax (nth 3 fu)
        antkile (nth 4 fu)  minb (nth 5 fu)
        i       0)
  (while (< i n)
    (setq ja (nth i fl)       jb (nth (1+ i) fl)
          wa (nth i wl)       wb (nth (1+ i) wl))
    (if (and skiftevis (= (rem (abs (- i mid)) 2) 0))
      (setq ents (cons (kb:sten m ja jb 0.0 hs j wa wb) ents)
            ents (cons (kb:sten m ja jb (+ hs *sb-delfuge*) *sb-dybde* j wa wb) ents)
            antdelt (1+ antdelt))
      (setq ents (cons (kb:sten m ja jb 0.0 *sb-dybde* j wa wb) ents)))
    (setq i (1+ i)))

  ;; Buens stykker (centrum, radius) fra højre mod venstre, i UCS.
  ;; ydre = stenens dybde + fugen langs toppen.
  (setq ydre (+ *sb-dybde* *sb-topfuge*)
        secs (mapcar '(lambda (g) (list (sb:lok m (car g) (cadr g)) (nth 2 g)
                                        (+ (nth 2 g) ydre) (nth 4 g) (nth 3 g)))
                     (reverse geo)))

  ;; Fuge langs toppen af buen (fra vederlag til vederlag)
  (if (> *sb-topfuge* 0.0)
    (setq topf (sb:zone-pl (mapcar '(lambda (s) (list (nth 0 s) (- (nth 2 s) *sb-topfuge*)
                                                      (nth 2 s) (nth 3 s) (nth 4 s)))
                                   secs))
          ents (cons topf ents)))

  ;; Trim murstenene bag buen
  (initget "Ja Nej")
  (if (/= (getkword "\nTrim murstenene bag buen? [Ja/Nej] <Ja>: ") "Nej")
    (progn
      (setq trimres (sb:trim doc secs ents))
      ;; Efter trim: fjern topfugens streger og omridsets buer i top
      ;; og bund, så fugerne mellem stenene står åbne. Kun
      ;; vederlagslinjerne i siderne bliver.
      (setq ents (vl-remove omrids ents))
      (entdel omrids)
      (if topf (progn (setq ents (vl-remove topf ents)) (entdel topf)))
      (setq ents (cons (sb:linje (sb:lok m (- a) 0.0) (sb:lok m (- (+ a *sb-dybde*)) 0.0)) ents)
            ents (cons (sb:linje (sb:lok m a 0.0) (sb:lok m (+ a *sb-dybde*) 0.0)) ents))
    )
  )

  (setq gnavn (vl-catch-all-apply 'sb:gruppe (list doc ents "KURVEBUE")))
  ;; gem buens mål i tegningen, så BUEMAAL kan finde dem
  (vl-catch-all-apply 'sb:data-gem
    (list gnavn "KURVE" (append (trans m 1 0) (list a b *sb-dybde* *sb-topfuge* k j n (length geo))
                                (apply 'append geo) (list *sb-stenh*))))
  (vla-EndUndoMark doc)

  ;; --- Resultat ---
  (princ "\n==================== KURVEHANKSBUE ====================")

  (princ "\n INDSTILLINGER")
  (princ (strcat "\n  Skiftehøjde:              " (sb:tal *sb-skifte* 2) " mm"))
  (princ (strcat "\n  Stenhøjde:                " (sb:tal *sb-stenh* 0) " mm"))
  (princ (strcat "\n  Buedybde:                 " (sb:tal *sb-dybde* 0) " mm"
                 (cond ((equal *sb-dybde* 108.0 0.01) "  (halvsten)")
                       ((equal *sb-dybde* 228.0 0.01) "  (helsten)")
                       (T ""))))
  (princ (strcat "\n  Stentype (valgt):         " *sb-type*
                 (if (= *sb-type* "Skiftevis")
                   (strcat "  (fuge mellem halve " (sb:tal *sb-delfuge* 0) " mm)") "")))
  (princ (strcat "\n  Fuge langs toppen:        "
                 (if (> *sb-topfuge* 0.0) (strcat (sb:tal *sb-topfuge* 1) " mm") "ingen")))
  (princ (strcat "\n  Fugekrav:                 " (sb:tal *sb-minfuge* 0) " - "
                 (sb:tal *sb-maxfuge* 0) " mm"))

  (princ "\n MÅL")
  (princ (strcat "\n  Spændvidde:               " (sb:tal (* 2.0 a) 0) " mm"))
  (princ (strcat "\n  Pilhøjde (underkant):     " (sb:tal b 0) " mm  (overkant - 1 sten)"))
  (princ (strcat "\n  Pilhøjde / spændvidde:    " (sb:tal (/ b (* 2.0 a)) 3)
                 "  (1 : " (sb:tal (/ (* 2.0 a) b) 1) ")"))
  (princ (strcat "\n  Overkant over vederlag:   " (itoa k) " skifter = "
                 (sb:tal (* k *sb-skifte*) 0) " mm"))
  (princ (strcat "\n  Vederlag flyttet:         " (if (> (- yV yV0) 0) "+" "")
                 (sb:tal (- yV yV0) 0) " mm i forhold til klik (lagt på langfuge)"))

  (princ "\n GEOMETRI")
  (princ (strcat "\n  Opsnøring:                " (itoa (length geo)) " cirkler  ("
                 (if (= (length geo) 5)
                   "pilhøjde under 1/4 af spændvidden)"
                   "pilhøjde mindst 1/4 af spændvidden)")))
  (princ "\n  Overgange:                fælles tangent - ingen knæk")
  (princ "\n  Cirkel        Radius   Vinkel   Centrum fra vederlagets midte")
  (setq navne (if (= (length geo) 5)
                '("A venstre" "C venstre" "B top    " "C højre  " "A højre  ")
                '("A venstre" "B top    " "A højre  "))
        nr 0)
  (foreach g geo
    (princ (strcat "\n  " (nth nr navne) "  "
                   (kb:pad (strcat (sb:tal (nth 2 g) 0) " mm") 9)
                   (kb:pad (strcat (sb:tal (* 180.0 (/ (- (nth 3 g) (nth 4 g)) pi)) 1) " gr") 9)
                   "x " (sb:tal (car g) 0) ",  y " (sb:tal (cadr g) 0)
                   (if (< (cadr g) -0.5) "  (under vederlag)" "")))
    (setq nr (1+ nr)))

  (princ "\n STEN OG FUGER")
  (princ (strcat "\n  Antal sten:               " (itoa n)
                 "  (slutsten = nr. " (itoa (1+ mid)) ")"))
  (if skiftevis
    (princ (strcat "\n  Stentype (tegnet):        Skiftevis - " (itoa antdelt) " delte, "
                   (itoa (- n antdelt)) " hele"))
    (princ (strcat "\n  Stentype (tegnet):        Hele sten"
                   (if (= *sb-type* "Skiftevis") "  (buedybden er for lille til 2 halve)" ""))))
  (princ (strcat "\n  Buelængde på oversiden:   " (sb:tal le 0) " mm"))
  (princ (strcat "\n  Fuge i toppen (ens):      " (sb:tal j 1) " mm  (max "
                 (sb:tal *sb-maxfuge* 0) ")  OK"))
  (if (< fbmin 1e8)
    (princ (strcat "\n  Fuge i bunden:            " (sb:tal fbmin 1) " - " (sb:tal fbmax 1)
                   " mm  (min " (sb:tal *sb-minfuge* 0) ")  OK")))
  (princ (strcat "\n  Kileskårne sten:          " (itoa antkile) " af " (itoa n)
                 "  (resten er hele " (sb:tal *sb-stenh* 0) " mm sten)"))
  (princ (strcat "\n  Smalleste sten i bund:    " (sb:tal minb 0) " mm"))

  (princ "\n TEGNING")
  (if trimres
    (progn
      (princ (strcat "\n  Trimmet omkring buen:     " (itoa (car trimres)) " trimmet, "
                     (itoa (cadr trimres)) " slettet"))
      (princ (strcat "\n  Finish:                   " (itoa (caddr trimres)) " dobbelte sten, "
                     (itoa (cadddr trimres)) " småstumper fjernet")))
    (princ "\n  Trim:                     ikke valgt (brug BUETRIM senere)"))
  (princ (strcat "\n  Gruppe:                   "
                 (if (vl-catch-all-error-p gnavn) (strcat "kunne ikke oprettes: " (vl-catch-all-error-message gnavn)) gnavn)))
  (princ "\n=======================================================")
  (princ)
)

;; ============================================================
;; KLØVERBLADSBUE (3 lige store cirkler)
;; ============================================================
;; Øverst en halvcirkelbue og nederst i hver side en kvartcirkelbue.
;; Konstruktionen (fra "Stik og buer") tegnes oppefra:
;;   Overkant bue på en langfuge.
;;   Uk-buehøjde = overkant - buehøjde (buedybden, fx ½ sten).
;;   1. centrum (B) = 1/4 spændvidde under uk-buehøjde, på LOD.
;;   Centrene for cirkelslag (A) = 1/4 spændvidde længere nede,
;;   1/4 spændvidde inde fra hver fals. Her starter buen.
;; Alle 3 cirkler har radius R = spændvidde / 4. Spændvidden er fra
;; fals til fals. Under buens start fortsætter falsen lodret ned.
;; Sidebuerne slutter i toppen af deres cirkel, hvor topbuen starter
;; (knækket).
;; I knækket sidder en låsesten, som låser sidebue og topbue sammen.
;; Låsestenens hjørne sidder i hakket i oversiden. Dens sider følger
;; strålen fra A og strålen fra B gennem hakket og er begge lige så
;; lange som buedybden - dvs. en specialsten 108 x 108 x 228 (eller 2
;; sten limet sammen), der tilhugges efter begge buer i bunden.
;; Sidebuer og topbue inddeles hver for sig på oversiden med hele sten
;; og ens fuger (7-20 mm). Topbuen har ulige antal sten (slutsten i
;; midten). Fugerne peger mod centrum for deres egen bue.
;; Lokale mål: (0, 0) = midt mellem centrene A (buens start), y op.

;; Hakket i oversiden (venstre side) og låsestenens sider.
;; Returnerer (Q vinkelA vinkelB side-A side-B):
;;  Q = hvor sidebuens og topbuens overside skærer hinanden
;;  vinkelA/B = Q's vinkel set fra centrum A (-R, 0) og B (0, R)
;;  side-A/B = vinklerne for låsestenens sider = strålerne gennem Q
;;  (st bruges ikke længere, men står der for at holde kaldene ens)
(defun kl:knaek (r D st / ro s q thl thu)
  (setq ro  (+ r D)
        s   (sqrt (/ (- (* ro ro) (* 0.5 r r)) 2.0))
        q   (list (- (* -0.5 r) s) (+ (* 0.5 r) s))
        thl (angle (list (- r) 0.0) q)
        thu (angle (list 0.0 r) q))
  (list q thl thu thl thu)
)

;; Inddeling på oversiden af én bue med buelængden le: hele sten + ens
;; fuge. Samme regel som kurvehanksbuen (kb:inddel). ulige = T: ulige
;; antal sten (topbuen med slutsten). Returnerer (antal fuge) eller nil.
(defun kl:inddel (le ri ulige / maal n0 n tst bedstn bedstj trin)
  (setq maal (max (- *sb-skifte* *sb-stenh*)
                  (- (* (+ *sb-stenh* *sb-minfuge*) (/ (+ ri *sb-dybde*) ri)) *sb-stenh*))
        n0   (fix (/ (- le maal) (+ *sb-stenh* maal)))
        n    (max 1 (- n0 8))
        trin (if ulige 2 1))
  (if (and ulige (= (rem n 2) 0)) (setq n (1+ n)))
  (while (<= n (+ n0 8))
    (setq tst (/ (- le (* n *sb-stenh*)) (1+ n)))
    (if (and (>= tst *sb-minfuge*) (<= tst *sb-maxfuge*)
             (or (not bedstn)
                 (if (>= bedstj (- maal 1e-6))
                   (and (>= tst (- maal 1e-6)) (< tst bedstj))
                   (> tst bedstj))))
      (setq bedstn n bedstj tst))
    (setq n (+ n trin)))
  (if bedstn (list bedstn bedstj))
)

;; Fugernes bredde i bunden for én bue (liste af fuger fl, fuge j i toppen).
;; fast0/fastn = T: fugen i den ende er en vederlagsfuge og beholder
;; bredden fra toppen. De andre fuger er kileformede (min. 7 mm i bunden)
;; som i kurvehanksbuen. Returnerer
;; (bundbredder fuge-bund-min fuge-bund-max antal-kileskårne smalleste-sten)
(defun kl:fuger (fl j fast0 fastn / bp dl nj i w wl fbmin fbmax antkile minb q bredde)
  (setq bp (mapcar '(lambda (jp) (kb:hj jp 0.0 1 0.0)) fl)
        dl (mapcar '(lambda (p1 p2) (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))
                   bp (cdr bp))
        nj (length fl)
        i  0  fbmin 1e9  fbmax 0.0)
  (while (< i nj)
    (if (or (and (= i 0) fast0) (and (= i (1- nj)) fastn))
      (setq w j)
      (setq w     (min j (max *sb-minfuge*
                              (if (> i 0) (- (nth (1- i) dl) *sb-stenh*) 0.0)
                              (if (< i (1- nj)) (- (nth i dl) *sb-stenh*) 0.0)))
            fbmin (min fbmin w)
            fbmax (max fbmax w)))
    (setq wl (cons w wl) i (1+ i)))
  (setq wl (reverse wl))
  (if (> fbmin 1e8) (setq fbmin j fbmax j))
  (setq i 0 antkile 0 minb 1e9)
  (while (< i (1- nj))
    (setq q      (kb:hjoerner (nth i fl) (nth (1+ i) fl) 0.0 0.0 j (nth i wl) (nth (1+ i) wl))
          bredde (distance (list (car (nth 0 q)) (cadr (nth 0 q)))
                           (list (car (nth 3 q)) (cadr (nth 3 q))))
          minb   (min minb bredde))
    (if (< bredde (- *sb-stenh* 0.5)) (setq antkile (1+ antkile)))
    (setq i (1+ i)))
  (list wl fbmin fbmax antkile minb)
)

;; Bulge for en bue om centrum c fra p til q (den korte vej)
(defun kl:bul (c p q / d)
  (setq d (sb:naer (- (angle c q) (angle c p)) 0.0))
  (sb:tan (/ d 4.0))
)
(defun kl:spejl (p) (list (- (car p)) (cadr p)))

;; Hele buen for radius r med m sten i hver sidebue (fuge jl) og n sten
;; i topbuen (fuge ju). Bruger *sb-dybde* og *sb-stenh*.
;; Returnerer liste:
;;  0 r  1 m  2 jl  3 n  4 ju  5 Q (hakket, venstre)
;;  6 sten fra venstre mod højre: (sten ja jb j wa wb) eller (laas punkter)
;;    punkter = ((punkt centrum-til-næste-punkt eller nil) ...)
;;  7 fuge-bund-min  8 fuge-bund-max  9 antal-kileskårne  10 smalleste
;;  11 geo: de 3 buer som i kurvehanksbuen (cx cy r start slut)
(defun kl:opbyg (r m jl n ju / D st ro kn q phiu cl cu i fll flu ful fuu wll wlu
                   flr wlr pa pd lv lh sten)
  (setq D  *sb-dybde*  st *sb-stenh*  ro (+ r D)
        kn (kl:knaek r D st)
        q  (car kn)
        phiu (nth 4 kn)
        cl (list (- r) 0.0)
        cu (list 0.0 r)
        i  0)
  ;; fuger i venstre sidebue: fra vederlaget op mod låsestenen
  (while (<= i m)
    (setq fll (cons (list (- r) 0.0 r (- pi (/ (+ (* i (+ st jl)) (/ jl 2.0)) ro))) fll)
          i   (1+ i)))
  (setq fll (reverse fll) i 0)
  ;; fuger i topbuen: fra venstre låsesten mod højre
  (while (<= i n)
    (setq flu (cons (list 0.0 r r (- phiu (/ (+ (* i (+ st ju)) (/ ju 2.0)) ro))) flu)
          i   (1+ i)))
  (setq flu (reverse flu)
        ful (kl:fuger fll jl T nil)
        fuu (kl:fuger flu ju nil nil)
        wll (car ful)
        wlu (car fuu)
        ;; højre sidebue = spejlbillede af venstre
        flr (mapcar '(lambda (jp) (list r 0.0 r (- pi (nth 3 jp)))) (reverse fll))
        wlr (reverse wll))
  ;; venstre låsesten: sidebuens sidste fuge (stråle fra A) op til
  ;; hakket Q, topbuens første fuge (stråle fra B) ned til topbuens
  ;; underside, topbuens underside ned til knækket og sidebuens
  ;; underside tilbage.
  (setq pa (kb:hj (last fll) 0.0 -1 (last wll))
        pd (kb:hj (car flu) 0.0 1 (car wlu))
        lv (list (list (list (car pa) (cadr pa)) nil)
                 (list q nil)
                 (list (list (car pd) (cadr pd)) cu)
                 (list (list (- r) r) cl))
        lh (mapcar '(lambda (x) (list (kl:spejl (car x)) (if (cadr x) (kl:spejl (cadr x))))) lv))
  ;; stenene fra venstre mod højre
  (setq i 0)
  (while (< i m)
    (setq sten (cons (list 'sten (nth i fll) (nth (1+ i) fll) jl (nth i wll) (nth (1+ i) wll)) sten)
          i    (1+ i)))
  (setq sten (cons (list 'laas lv) sten) i 0)
  (while (< i n)
    (setq sten (cons (list 'sten (nth i flu) (nth (1+ i) flu) ju (nth i wlu) (nth (1+ i) wlu)) sten)
          i    (1+ i)))
  (setq sten (cons (list 'laas lh) sten) i 0)
  (while (< i m)
    (setq sten (cons (list 'sten (nth i flr) (nth (1+ i) flr) jl (nth i wlr) (nth (1+ i) wlr)) sten)
          i    (1+ i)))
  (list r m jl n ju q (reverse sten)
        (min (nth 1 ful) (nth 1 fuu))
        (max (nth 2 ful) (nth 2 fuu))
        (+ (* 2 (nth 3 ful)) (nth 3 fuu))
        (min (nth 4 ful) (nth 4 fuu))
        (list (list (- r) 0.0 r pi (/ pi 2.0))
              (list 0.0 r r pi 0.0)
              (list r 0.0 r (/ pi 2.0) 0.0)))
)

;; Beregn buen for halv spændvidde a. Returnerer kl:opbyg-listen eller nil.
(defun kl:beregn (a / r kn ll lu indl indu)
  (setq r (/ a 2.0))
  (if (> r (* 2.0 *sb-stenh*))
    (progn
      (setq kn (kl:knaek r *sb-dybde* *sb-stenh*)
            ll (* (- pi (nth 3 kn)) (+ r *sb-dybde*))                ; sidebue, oversiden
            lu (* (- (* 2.0 (nth 4 kn)) pi) (+ r *sb-dybde*)))       ; topbue, oversiden
      (if (and (> ll 0.0) (> lu 0.0)
               (setq indl (kl:inddel ll r nil))
               (setq indu (kl:inddel lu r T)))
        (kl:opbyg r (car indl) (cadr indl) (car indu) (cadr indu)))))
)

(defun kl:antal-sten (g) (+ (* 2 (nth 1 g)) (nth 3 g) 2))

;; Tegn stenene (lokalt i m). Returnerer (enames antal-delte).
(defun kl:tegn-sten (m g skiftevis hs / ents ne del mid i x lv)
  (setq mid (/ (1- (nth 3 g)) 2) del 0 i 0 ne 0)
  (foreach x (nth 6 g)
    (if (eq (car x) 'laas)
      (setq lv   (cadr x)
            ents (cons (sb:pl (mapcar '(lambda (p) (sb:lok m (car (car p)) (cadr (car p)))) lv)
                              (kl:laas-bul lv) T)
                       ents)
            ne   (1+ ne)
            i    0)
      (progn
        ;; Skiftevis: låsestenen er hel, så stenene ved siden af den er
        ;; delte. I topbuen er slutstenen delt (som i kurvehanksbuen).
        (if (and skiftevis
                 (if (= ne 1)
                   (= (rem (abs (- i mid)) 2) 0)
                   (= (rem (if (= ne 0) (- (nth 1 g) i) (1+ i)) 2) 1)))
          (setq ents (cons (kb:sten m (nth 1 x) (nth 2 x) 0.0 hs (nth 3 x) (nth 4 x) (nth 5 x)) ents)
                ents (cons (kb:sten m (nth 1 x) (nth 2 x) (+ hs *sb-delfuge*) *sb-dybde*
                                    (nth 3 x) (nth 4 x) (nth 5 x)) ents)
                del  (1+ del))
          (setq ents (cons (kb:sten m (nth 1 x) (nth 2 x) 0.0 *sb-dybde* (nth 3 x) (nth 4 x) (nth 5 x))
                           ents)))
        (setq i (1+ i)))))
  (list ents del)
)

;; Bulges for låsestenens polylinje
(defun kl:laas-bul (lv / i n res p q c)
  (setq n (length lv) i 0)
  (while (< i n)
    (setq p   (car (nth i lv))
          q   (car (nth (rem (1+ i) n) lv))
          c   (cadr (nth i lv))
          res (cons (if c (kl:bul c p q) 0.0) res)
          i   (1+ i)))
  (reverse res)
)

;; ------------------------------------------------------------
;; KLOVERBUE: løsninger og valg i en dialog med forhåndsvisning
;; ------------------------------------------------------------

;; Mulige buer ud fra de klikkede fals (a0 = halv spændvidde, ys =
;; vederlaget rettet til langfuge). Spændvidden er altid som klikket.
;; Konstruktionen går oppefra: overkant på en langfuge (k skifter over
;; vederlaget) -> buens start (centrene A) = overkant - buedybde -
;; halv spændvidde. Buen må ikke starte under vederlaget. Under buens
;; start fortsætter falsen lodret ned til vederlaget.
;; Kandidat = (k buestart-y stående-højde geometri)
(defun kl:kandidater (a0 ys / g k ya lst)
  (if (setq g (kl:beregn a0))
    (progn
      (setq k 1)
      (while (<= (setq ya (- (+ ys (* k *sb-skifte*)) a0 *sb-dybde*)) (+ ys (* 5.0 *sb-skifte*)))
        (if (>= ya (- ys 0.5))
          (setq lst (cons (list k ya (max 0.0 (- ya ys)) g) lst)))
        (setq k (1+ k)))
      (reverse lst)))
)

;; Ligger buens start på en langfuge?
(defun kl:paa-langfuge (x / r)
  (setq r (rem (nth 2 x) *sb-skifte*))
  (or (< r 1.0) (> r (- *sb-skifte* 1.0)))
)

(defun kl:fugetop (g) (strcat (sb:tal (nth 2 g) 1) "/" (sb:tal (nth 4 g) 1)))
(defun kl:fugebund (g)
  (if (equal (nth 7 g) (nth 8 g) 0.05)
    (sb:tal (nth 7 g) 1)
    (strcat (sb:tal (nth 7 g) 1) "-" (sb:tal (nth 8 g) 1)))
)

(defun kl:overskrift ()
  (strcat (kb:pad "Overkant" 20) (kb:pad "Buen starter" 22) (kb:pad "Langfuge" 10)
          (kb:pad "Radius" 8) (kb:pad "Sten" 6) (kb:pad "Fuge top" 12)
          (kb:pad "Fuge bund" 11) "Kilesk.")
)
(defun kl:linje (x bedst / g)
  (setq g (nth 3 x))
  (strcat (kb:pad (strcat (itoa (car x)) " sk. (" (sb:tal (* (car x) *sb-skifte*) 0) " mm)") 20)
          (kb:pad (strcat (sb:tal (nth 2 x) 0) " mm over vederl.") 22)
          (kb:pad (if (kl:paa-langfuge x) "ja" "nej") 10)
          (kb:pad (sb:tal (car g) 0) 8)
          (kb:pad (itoa (kl:antal-sten g)) 6)
          (kb:pad (kl:fugetop g) 12)
          (kb:pad (kl:fugebund g) 11)
          (kb:pad (itoa (nth 9 g)) 6)
          (if bedst "<- forslag" ""))
)

(defun kl:dcl-fil (/ fn f)
  (setq fn (vl-filename-mktemp "kloverbue" nil ".dcl")
        f  (open fn "w"))
  (foreach l
    '("kl_valg : dialog {"
      "  label = \"Kloeverbladsbue - vaelg overkant\";"
      "  : boxed_column { label = \"Aabning og indstillinger\";"
      "    : text { key = \"hoved\"; width = 110; }"
      "    : text { key = \"hoved2\"; width = 110; }"
      "  }"
      "  : boxed_column { label = \"Mulige overkanter - vaelg en linje\";"
      "    : list_box { key = \"liste\"; height = 8; width = 110; fixed_width_font = true; }"
      "  }"
      "  : row {"
      "    : boxed_column { label = \"Forhaandsvisning\";"
      "      : image { key = \"billede\"; width = 62; height = 19; color = 255; }"
      "      : text { key = \"legende\"; width = 62; }"
      "    }"
      "    : boxed_column { label = \"Valgt bue\";"
      "      : text { key = \"v1\"; width = 46; }"
      "      : text { key = \"v2\"; width = 46; }"
      "      : text { key = \"v3\"; width = 46; }"
      "      : text { key = \"v4\"; width = 46; }"
      "      : text { key = \"v5\"; width = 46; }"
      "      : text { key = \"v6\"; width = 46; }"
      "      : text { key = \"v7\"; width = 46; }"
      "      : text { key = \"v8\"; width = 46; }"
      "      spacer_1;"
      "      : button { key = \"indst\"; label = \"Indstillinger og stentype...\"; }"
      "    }"
      "  }"
      "  spacer_1;"
      "  : row { alignment = centered; fixed_width = true;"
      "    : button { key = \"accept\"; label = \"Tegn buen\"; is_default = true; width = 18; }"
      "    : button { key = \"cancel\"; label = \"Annuller\"; is_cancel = true; width = 14; }"
      "  }"
      "}")
    (write-line l f))
  (close f)
  fn
)

;; Billede af kløverbladsbuen. Lokalt: (0, 0) = buens start i midten.
;; Hvid mur med grå langfuger, åbningen i dialogens grå, teglrøde hele
;; sten, gule kileskårne, blå låsesten (skiftevis: delt som når buen
;; tegnes, se kl:tegn-sten), + = centre. Grøn linje = overkant.
(defun kl:billede (x / bw bh D tf g r a st ok yl sk oy hx y0 kr ro kn thl thu hs delt m mid
                     ind ud pt lin y q farve ne i del sp c)
  (setq bw   (dimx_tile "billede")
        bh   (dimy_tile "billede")
        D    *sb-dybde*
        tf   *sb-topfuge*
        g    (nth 3 x)
        st   (nth 2 x)                                     ; stående fals under buen
        r    (car g)
        a    (* 2.0 r)
        ok   (+ a D)                                       ; overkant
        yl   (+ st (* 0.12 (+ a D)) 10.0)
        sk   (min (/ (- bw 16.0) (* 2.0 (+ a D 40.0)))
                  (/ (- bh 12.0) (+ a D yl 30.0)))
        oy   (+ (/ bh 2.0) (* sk (/ (- (+ a D 30.0) yl) 2.0)))   ; buen midt i billedet
        hx   (/ bw 2.0 sk)
        y0   (/ (- oy bh) sk)
        kr   (/ 4.0 sk)                                    ; centrenes kryds: 4 pixels
        ro   (+ r D tf)
        kn   (kl:knaek r (+ D tf) st)                      ; hakket i ydersiden (venstre)
        thl  (nth 1 kn)
        thu  (nth 2 kn)
        hs   (/ (- D *sb-delfuge*) 2.0)
        delt (and (= *sb-type* "Skiftevis") (>= hs 90.0))
        m    (nth 1 g)
        mid  (/ (1- (nth 3 g)) 2)
        ;; undersiden fra venstre mod højre og ydersiden (med fugen langs
        ;; toppen) fra højre mod venstre med hakkene ved låsestenene
        ind  (append (bill:bue (list (- r) 0.0) r pi (/ pi 2.0) 8)
                     (bill:bue (list 0.0 r) r pi 0.0 16)
                     (bill:bue (list r 0.0) r (/ pi 2.0) 0.0 8))
        ud   (append (bill:bue (list r 0.0) ro 0.0 (- pi thl) 8)
                     (bill:bue (list 0.0 r) ro (- pi thu) thu 16)
                     (bill:bue (list (- r) 0.0) ro thl pi 8)))
  (defun pt (p) (list (+ (/ bw 2.0) (* (car p) sk)) (- oy (* (cadr p) sk))))
  (defun lin (p q farve) (bill:streg (pt p) (pt q) bw bh farve))
  (start_image "billede")
  (fill_image 0 0 bw bh 255)
  ;; murens langfuger - vederlaget (y = -st) ligger på en langfuge
  (setq y (- (* (1+ (fix (/ (+ (/ oy sk) st) *sb-skifte*))) *sb-skifte*) st))
  (while (> y y0)
    (lin (list (- hx) y) (list hx y) 254)
    (setq y (- y *sb-skifte*)))
  ;; åbningen: falsene og buens underside
  (bill:fyld (mapcar 'pt (append (list (list (- a) y0)) ind (list (list a y0)))) bw bh -15)
  ;; overkant
  (lin (list (- hx) ok) (list hx ok) 92)
  ;; buen og fugen langs toppen står fri af murstenene
  (bill:fyld (mapcar 'pt (append ind ud)) bw bh 255)
  ;; falsene og vederlaget
  (lin (list (- a) 0.0) (list (- a) y0) 252)
  (lin (list a 0.0) (list a y0) 252)
  (lin (list (- hx) (- st)) (list (- a) (- st)) 252)
  (lin (list a (- st)) (list hx (- st)) 252)
  ;; konstruktionslinjer: buens start, centrene A op til knækket og
  ;; topbuens vandrette linje
  (lin (list (- a) 0.0) (list a 0.0) 253)
  (lin (list (- r) 0.0) (list (- r) r) 253)
  (lin (list r 0.0) (list r r) 253)
  (lin (list (- r) r) (list r r) 253)
  ;; stenene fra venstre mod højre
  (setq ne 0 i 0)
  (foreach sp (nth 6 g)
    (if (eq (car sp) 'laas)
      (progn
        (bill:sten (mapcar 'pt (kl:laas-pl (cadr sp))) bw bh 150 154)
        (setq ne (1+ ne) i 0))
      (progn
        (setq q     (kb:hjoerner (nth 1 sp) (nth 2 sp) 0.0 D (nth 3 sp) (nth 4 sp) (nth 5 sp))
              farve (if (< (distance (list (car (nth 0 q)) (cadr (nth 0 q)))
                                     (list (car (nth 3 q)) (cadr (nth 3 q))))
                           (- *sb-stenh* 0.5))
                      40 22))
        (foreach del (if (and delt
                              (if (= ne 1)
                                (= (rem (abs (- i mid)) 2) 0)
                                (= (rem (if (= ne 0) (- m i) (1+ i)) 2) 1)))
                       (list (list 0.0 hs) (list (+ hs *sb-delfuge*) D))
                       (list (list 0.0 D)))
          (bill:sten (mapcar 'pt (kb:hjoerner (nth 1 sp) (nth 2 sp) (car del) (cadr del)
                                              (nth 3 sp) (nth 4 sp) (nth 5 sp)))
                     bw bh farve (if (= farve 22) 24 42)))
        (setq i (1+ i)))))
  ;; centrene
  (foreach c (list (list (- r) 0.0) (list 0.0 r) (list r 0.0))
    (lin (list (- (car c) kr) (cadr c)) (list (+ (car c) kr) (cadr c)) 250)
    (lin (list (car c) (- (cadr c) kr)) (list (car c) (+ (cadr c) kr)) 250))
  (end_image)
)

;; Låsestenens omrids som punkter: hjørnerne og buerne mellem dem
(defun kl:laas-pl (lv / i n p q c v res f)
  (setq n (length lv) i 0)
  (while (< i n)
    (setq p   (car (nth i lv))
          q   (car (nth (rem (1+ i) n) lv))
          c   (cadr (nth i lv))
          res (cons p res))
    (if c
      (progn
        (setq v (sb:naer (- (angle c q) (angle c p)) 0.0))
        (foreach f '(0.2 0.4 0.6 0.8)
          (setq res (cons (polar c (+ (angle c p) (* f v)) (distance c p)) res)))))
    (setq i (1+ i)))
  (reverse res)
)

(defun kl:vis-valgt (nr / x g)
  (setq nr (atoi nr))
  (if (> nr 0)
    (progn
      (setq x (nth (1- nr) *kl-kand*) g (nth 3 x))
      (set_tile "v1" (strcat "Spændvidde: " (sb:tal (* 2.0 *kl-a0*) 0) " mm (fals til fals)"))
      (set_tile "v2" (strcat "R = 1/4 spændvidde = " (sb:tal (car g) 0) " mm (3 cirkler)"))
      (set_tile "v3" (strcat "Overkant: " (itoa (car x)) " skifter over vederlag = "
                             (sb:tal (* (car x) *sb-skifte*) 0) " mm"))
      (set_tile "v4" (strcat "Buen starter " (sb:tal (nth 2 x) 0) " mm over vederlag - "
                             (if (kl:paa-langfuge x) "på langfuge" "inde i et skifte")))
      (set_tile "v5" (strcat "Sten: " (itoa (nth 1 g)) " + L + " (itoa (nth 3 g)) " + L + "
                             (itoa (nth 1 g)) " = " (itoa (kl:antal-sten g))
                             "  (L = låsesten)"))
      (set_tile "v6" (strcat "Fuge i toppen: " (sb:tal (nth 2 g) 1) " (sider) / "
                             (sb:tal (nth 4 g) 1) " (top) mm  (max " (sb:tal *sb-maxfuge* 0) ")"))
      (set_tile "v7" (strcat "Fuge i bunden: " (kl:fugebund g) " mm  (min "
                             (sb:tal *sb-minfuge* 0) ")"))
      (set_tile "v8" (strcat "Kileskårne: " (itoa (nth 9 g)) " af " (itoa (- (kl:antal-sten g) 2))
                             "   smalleste i bund: " (sb:tal (nth 10 g) 0) " mm"))
      (kl:billede x))
    (progn
      (set_tile "liste" "1")
      (kl:vis-valgt "1")))
)

;; Vælg løsning. Returnerer kandidaten, 'indst eller nil (annuller).
;; Forslaget er den laveste overkant (buen starter tættest på vederlaget).
(defun kl:vaelg (kand a0 / fn id svar x i)
  (setq *sb-vis* (list "KLOVERBLADSBUE" a0)               ; buen vises i indstillingerne
        fn (kl:dcl-fil)
        id (load_dialog fn))
  (if (or (< id 0) (not (new_dialog "kl_valg" id)))
    ;; ingen dialog mulig - vis tabellen i kommandolinjen og brug forslaget
    (progn
      (if (>= id 0) (unload_dialog id))
      (vl-file-delete fn)
      (princ (strcat "\n  " (kl:overskrift)))
      (setq i 0)
      (foreach x kand
        (princ (strcat "\n  " (kl:linje x (= i 0))))
        (setq i (1+ i)))
      (initget "Ja Nej")
      (if (/= (getkword "\nTegn forslaget? [Ja/Nej] <Ja>: ") "Nej")
        (car kand)))
    (progn
      (setq *kl-kand* kand *kl-a0* a0 *kl-valg* nil)
      (set_tile "hoved"
        (strcat "Spændvidde " (sb:tal (* 2.0 a0) 0) " mm   Buedybde " (sb:tal *sb-dybde* 0)
                " mm   Sten: " *sb-type* "   Skifte " (sb:tal *sb-skifte* 2)
                " mm   Fuge " (sb:tal *sb-minfuge* 0) "-" (sb:tal *sb-maxfuge* 0) " mm"))
      (set_tile "hoved2"
        (strcat "Oppefra: overkant på langfuge - buehøjde = uk-buehøjde - 1/4 spænd = centrum B"
                " - 1/4 spænd = centre A (buens start). Under starten er falsen lodret."))
      (set_tile "legende" "Rød = hel sten   Gul = kileskåret   Blå = låsesten   + = centre   Grøn = overkant")
      (start_list "liste")
      (add_list (kl:overskrift))
      (setq i 0)
      (foreach x kand
        (add_list (kl:linje x (= i 0)))
        (setq i (1+ i)))
      (end_list)
      (set_tile "liste" "1")
      (kl:vis-valgt "1")
      (action_tile "liste"  "(kl:vis-valgt $value) (if (= $reason 4) (progn (setq *kl-valg* $value) (done_dialog 1)))")
      (action_tile "accept" "(setq *kl-valg* (get_tile \"liste\")) (done_dialog 1)")
      (action_tile "indst"  "(done_dialog 3)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq svar (start_dialog))
      (unload_dialog id)
      (vl-file-delete fn)
      (cond
        ((= svar 1)
         (setq x (atoi *kl-valg*))
         (nth (if (< x 1) 0 (1- x)) kand))
        ((= svar 3) 'indst)
        (T nil))))
)

(defun c:KLOVERBUE (/ *error* doc p1 p2 yv ys a0 xm z kand x a ya st g r D m ydre secs
                      omrids topf ents ts skiftevis hs trimres gnavn)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )

  (sb:brug "KLOVERBLADSBUE")
  (princ (strcat "\n--- KLØVERBLADSBUE ---  Buedybde: " (sb:tal *sb-dybde* 0) " mm"
                 "   Skifte: " (sb:tal *sb-skifte* 1) " mm"
                 "   Fuge: " (sb:tal *sb-minfuge* 0) "-" (sb:tal *sb-maxfuge* 0) " mm"))
  (if (not (and (setq p1 (sb:punkt nil "\nVenstre fals ved vederlaget (spændvidden måles fals til fals) eller [Indstillinger/Stentype]: "))
                (setq p2 (sb:punkt p1 "\nHøjre fals ved vederlaget eller [Indstillinger/Stentype]: "))))
    (exit))
  (setq a0 (/ (abs (- (car p2) (car p1))) 2.0)
        xm (/ (+ (car p1) (car p2)) 2.0)
        z  (caddr p1)
        yv (/ (+ (cadr p1) (cadr p2)) 2.0))

  ;; Løsninger i en dialog. Indstillinger regner forfra.
  (setq x 'indst)
  (while (eq x 'indst)
    ;; vederlaget rettes til nærmeste langfuge i muren
    (if (setq ys (kb:langfuge p1 p2 yv))
      (princ (strcat "\nVederlag rettet til langfuge i muren: "
                     (if (> (- ys yv) 0) "+" "") (sb:tal (- ys yv) 0) " mm"))
      (progn
        (setq ys yv)
        (princ "\nFandt ingen mursten ved vederlaget - bruger dit klik som langfuge.")))
    (setq kand (kl:kandidater a0 ys))
    (if (not kand)
      (progn
        (alert (strcat "Ingen kløverbladsbue passer til spændvidden " (sb:tal (* 2.0 a0) 0)
                       " mm med buedybde " (sb:tal *sb-dybde* 0) " mm."
                       "\n\nPrøv en anden spændvidde eller ret indstillingerne."))
        (exit)))
    (setq x (kl:vaelg kand a0))
    (if (eq x 'indst) (sb:indst-dialog)))
  (if (not x) (exit))

  (setq a  a0
        ya (nth 1 x)
        st (nth 2 x)
        g  (nth 3 x)
        r  (car g)
        D  *sb-dybde*
        m  (list xm ya z))

  ;; --- Tegn ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; Buens 3 stykker fra højre mod venstre (centrum ri ro start slut), UCS.
  ;; ydre = stenens dybde + fugen langs toppen.
  (setq ydre (+ D *sb-topfuge*)
        secs (list (list (sb:lok m r 0.0)     r (+ r ydre) 0.0 (/ pi 2.0))
                   (list (sb:lok m 0.0 r)     r (+ r ydre) 0.0 pi)
                   (list (sb:lok m (- r) 0.0) r (+ r ydre) (/ pi 2.0) pi)))

  ;; Omrids (underside + overside med hakket ved låsestenene)
  (setq omrids (sb:zone-pl (mapcar '(lambda (s) (list (nth 0 s) r (+ r D) (nth 3 s) (nth 4 s))) secs))
        ents   (list omrids))

  ;; Sten og låsesten
  (setq skiftevis (sb:skiftevis-p)
        hs        (/ (- D *sb-delfuge*) 2.0)
        ts        (kl:tegn-sten m g skiftevis hs)
        ents      (append (car ts) ents))

  ;; Fuge langs toppen af buen (fra buens start til buens start)
  (if (> *sb-topfuge* 0.0)
    (setq topf (sb:zone-pl (mapcar '(lambda (s) (list (nth 0 s) (- (nth 2 s) *sb-topfuge*)
                                                      (nth 2 s) (nth 3 s) (nth 4 s)))
                                   secs))
          ents (cons topf ents)))

  ;; Trim murstenene bag buen
  (initget "Ja Nej")
  (if (/= (getkword "\nTrim murstenene bag buen? [Ja/Nej] <Ja>: ") "Nej")
    (progn
      (setq trimres (sb:trim doc secs ents))
      ;; Efter trim: fjern topfuge og omrids, så fugerne står åbne.
      ;; Kun linjerne ved buens start i siderne bliver.
      (setq ents (vl-remove omrids ents))
      (entdel omrids)
      (if topf (progn (setq ents (vl-remove topf ents)) (entdel topf)))
      (setq ents (cons (sb:linje (sb:lok m (- a) 0.0) (sb:lok m (- (+ a D)) 0.0)) ents)
            ents (cons (sb:linje (sb:lok m a 0.0) (sb:lok m (+ a D) 0.0)) ents))
    )
  )

  (setq gnavn (vl-catch-all-apply 'sb:gruppe (list doc ents "KLOVERBUE")))
  ;; gem buens mål i tegningen, så BUEMAAL og BUESKABELON kan finde dem
  ;; vals: mx my mz r D topf skifter m jl n ju stenh skifte stående
  (vl-catch-all-apply 'sb:data-gem
    (list gnavn "KLOVER" (append (trans m 1 0)
                                 (list r D *sb-topfuge* (/ (+ a D) *sb-skifte*)
                                       (nth 1 g) (nth 2 g) (nth 3 g) (nth 4 g) *sb-stenh*
                                       *sb-skifte* st))))
  (vla-EndUndoMark doc)

  ;; --- Resultat ---
  (princ "\n==================== KLØVERBLADSBUE ====================")
  (princ "\n INDSTILLINGER")
  (princ (strcat "\n  Skiftehøjde:              " (sb:tal *sb-skifte* 2) " mm"))
  (princ (strcat "\n  Stenhøjde:                " (sb:tal *sb-stenh* 0) " mm"))
  (princ (strcat "\n  Buehøjde (buedybde):      " (sb:tal D 0) " mm"
                 (cond ((equal D 108.0 0.01) "  (½ sten)")
                       ((equal D 228.0 0.01) "  (1 sten)")
                       (T ""))))
  (princ (strcat "\n  Fuge langs toppen:        "
                 (if (> *sb-topfuge* 0.0) (strcat (sb:tal *sb-topfuge* 1) " mm") "ingen")))
  (princ (strcat "\n  Fugekrav:                 " (sb:tal *sb-minfuge* 0) " - "
                 (sb:tal *sb-maxfuge* 0) " mm"))

  (princ "\n KONSTRUKTION (oppefra)")
  (princ (strcat "\n  Spændvidde (fals-fals):   " (sb:tal (* 2.0 a) 0) " mm  -  1/4 = R = " (sb:tal r 0) " mm"))
  (princ (strcat "\n  Overkant bue:             " (itoa (car x)) " skifter over vederlaget = "
                 (sb:tal (* (car x) *sb-skifte*) 0) " mm  (på langfuge)"))
  (princ (strcat "\n  Uk-buehøjde:              overkant - " (sb:tal D 0) " mm"))
  (princ (strcat "\n  1. centrum (B):           " (sb:tal r 0) " mm under uk-buehøjde, på LOD"))
  (princ (strcat "\n  Centre for cirkelslag (A): " (sb:tal r 0) " mm under B, " (sb:tal r 0)
                 " mm inde fra hver fals"))
  (princ (strcat "\n  Buen starter:             " (sb:tal st 0) " mm over vederlaget - "
                 (if (kl:paa-langfuge x) "på langfuge" "inde i et skifte (falsens sten tilpasses)")))

  (princ "\n STEN OG FUGER")
  (princ (strcat "\n  Antal sten:               " (itoa (kl:antal-sten g)) "  = "
                 (itoa (nth 1 g)) " + låsesten + " (itoa (nth 3 g)) " + låsesten + " (itoa (nth 1 g))))
  (princ (strcat "\n  Topbue:                   " (itoa (nth 3 g)) " sten, slutsten = nr. "
                 (itoa (1+ (/ (1- (nth 3 g)) 2)))))
  (princ (strcat "\n  Låsesten:                 2 - hjørnet i hakket, siderne " (sb:tal D 0) " mm"
                 " langs strålerne fra A og B (specialsten " (sb:tal D 0) " x " (sb:tal D 0)
                 " x 228 eller 2 sten limet sammen, tilhugges)"))
  (if skiftevis
    (princ (strcat "\n  Stentype (tegnet):        Skiftevis - " (itoa (cadr ts)) " delte"))
    (princ (strcat "\n  Stentype (tegnet):        Hele sten"
                   (if (= *sb-type* "Skiftevis") "  (buedybden er for lille til 2 halve)" ""))))
  (princ (strcat "\n  Fuge i toppen (ens):      " (sb:tal (nth 2 g) 1) " mm i sidebuerne, "
                 (sb:tal (nth 4 g) 1) " mm i topbuen  (max " (sb:tal *sb-maxfuge* 0) ")  OK"))
  (princ (strcat "\n  Fuge i bunden:            " (kl:fugebund g) " mm  (min "
                 (sb:tal *sb-minfuge* 0) ")  OK"))
  (princ (strcat "\n  Kileskårne sten:          " (itoa (nth 9 g)) " af " (itoa (- (kl:antal-sten g) 2))
                 "  (smalleste i bund " (sb:tal (nth 10 g) 0) " mm)"))

  (princ "\n TEGNING")
  (if trimres
    (progn
      (princ (strcat "\n  Trimmet omkring buen:     " (itoa (car trimres)) " trimmet, "
                     (itoa (cadr trimres)) " slettet"))
      (princ (strcat "\n  Finish:                   " (itoa (caddr trimres)) " dobbelte sten, "
                     (itoa (cadddr trimres)) " småstumper fjernet")))
    (princ "\n  Trim:                     ikke valgt (brug BUETRIM senere)"))
  (princ (strcat "\n  Gruppe:                   "
                 (if (vl-catch-all-error-p gnavn) (strcat "kunne ikke oprettes: " (vl-catch-all-error-message gnavn)) gnavn)))
  (princ "\n  Bueskive i 1:1:           BUESKABELON (BSK)")
  (princ "\n=========================================================")
  (princ)
)

;; ============================================================
;; TRIM AF MURSTEN BAG BUEN
;; ============================================================

;; Lav region fra et objekt. Returnerer første region eller nil.
(defun sb:region (spc obj / res)
  (setq res (vl-catch-all-apply 'vla-AddRegion
              (list spc (vlax-make-variant
                          (vlax-safearray-fill (vlax-make-safearray vlax-vbObject '(0 . 0))
                                               (list obj))))))
  (if (not (vl-catch-all-error-p res))
    (car (vlax-safearray->list (vlax-variant-value res))))
)

;; Eksplodér region til linjer og buer (også del-regioner)
(defun sb:explode (reg / lst res)
  (setq res (vl-catch-all-apply 'vla-Explode (list reg)))
  (if (not (vl-catch-all-error-p res))
    (foreach o (vlax-safearray->list (vlax-variant-value res))
      (if (= (vla-get-ObjectName o) "AcDbRegion")
        (progn (setq lst (append lst (sb:explode o))) (vla-Delete o))
        (setq lst (cons (vlax-vla-object->ename o) lst)))))
  lst
)

;; Alle objekter oprettet efter ename 'fra'
(defun sb:nye-siden (fra / e lst)
  (setq e (if fra (entnext fra) (entnext)))
  (while e (setq lst (cons e lst) e (entnext e)))
  lst
)

;; Areal af et objekts afgrænsningsboks
(defun sb:boksareal (obj / mn mx res)
  (setq res (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx)))
  (if (vl-catch-all-error-p res)
    1e99
    (progn
      (setq mn (vlax-safearray->list mn) mx (vlax-safearray->list mx))
      (* (- (car mx) (car mn)) (- (cadr mx) (cadr mn)))))
)

;; Eksplodér en blok (også blokke inde i blokke) til midlertidige
;; objekter. Returnerer liste af vla-objekter.
(defun sb:eksploder-blok (obj / res lst)
  (setq res (vl-catch-all-apply 'vla-Explode (list obj)))
  (if (not (vl-catch-all-error-p res))
    (foreach o (vlax-safearray->list (vlax-variant-value res))
      (if (= (vla-get-ObjectName o) "AcDbBlockReference")
        (progn (setq lst (append lst (sb:eksploder-blok o))) (vla-Delete o))
        (setq lst (cons o lst)))))
  lst
)

;; Ligger punkt p (WCS) inde i trim-zonen?
(defun sb:i-zone (p c ri ro a2 a1 f / q d bis dot s1 s2)
  (setq q   (trans p 0 1)
        d   (distance (sb:2d c) (sb:2d q))
        bis (/ (+ a1 a2) 2.0)
        dot (+ (* (cos bis) (- (car q) (car c))) (* (sin bis) (- (cadr q) (cadr c))))
        s2  (- (* (cos a2) (- (cadr q) (cadr c))) (* (sin a2) (- (car q) (car c))))
        s1  (- (* (cos a1) (- (cadr q) (cadr c))) (* (sin a1) (- (car q) (car c)))))
  (and (> dot 0.0)
       (>= d (- ri 1e-6)) (<= d (+ ro 1e-6))
       (>= s2 (- f)) (<= s1 f))
)

;; ------------------------------------------------------------
;; Trim-zonen beskrives som en liste af buestykker ("sektioner"):
;;   (centrum indre-radius ydre-radius startvinkel slutvinkel)
;; ordnet fra højre mod venstre. En segmentbue har 1 stykke,
;; en kurvehanksbue har 3 (højre side, midte, venstre side).
;; ------------------------------------------------------------

;; Ligger punkt p (WCS) i et af zonens stykker?
(defun sb:i-zonen (p secs)
  (vl-some '(lambda (s) (sb:i-zone p (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) (nth 4 s) 0.0))
           secs)
)

;; Skæringer mellem linjen p1-p2 og alle zonens kanter
(defun sb:snit-zonen (p1 p2 secs / ts)
  (foreach s secs
    (setq ts (append ts (sb:snit-linje p1 p2 (nth 0 s) (nth 1 s) (nth 2 s)
                                       (nth 3 s) (nth 4 s) 0.0))))
  ts
)

;; Vinkel v flyttet med hele omgange, så den ligger tættest på ref
(defun sb:naer (v ref)
  (while (> (- v ref) pi) (setq v (- v (* 2.0 pi))))
  (while (< (- v ref) (- pi)) (setq v (+ v (* 2.0 pi))))
  v
)

;; Erstat element nr. i i en liste
(defun sb:ersat (lst i v / k)
  (setq k -1)
  (mapcar '(lambda (x) (if (= (setq k (1+ k)) i) v x)) lst)
)

;; Skæring mellem to cirkler (UCS). Returnerer det skæringspunkt der
;; ligger nærmest punktet hint, eller nil.
(defun sb:cirkelsnit (c1 r1 c2 r2 hint / d aa hh ux uy bx by p1 p2)
  (setq d (distance (sb:2d c1) (sb:2d c2)))
  (if (and (> d 1e-9) (<= d (+ r1 r2)) (>= d (abs (- r1 r2))))
    (progn
      (setq aa (/ (+ (- (* r1 r1) (* r2 r2)) (* d d)) (* 2.0 d))
            hh (sqrt (max 0.0 (- (* r1 r1) (* aa aa))))
            ux (/ (- (car c2) (car c1)) d)
            uy (/ (- (cadr c2) (cadr c1)) d)
            bx (+ (car c1) (* aa ux))
            by (+ (cadr c1) (* aa uy))
            p1 (list (- bx (* hh uy)) (+ by (* hh ux)) (caddr c1))
            p2 (list (+ bx (* hh uy)) (- by (* hh ux)) (caddr c1)))
      (if (< (distance (sb:2d p1) (sb:2d hint)) (distance (sb:2d p2) (sb:2d hint))) p1 p2)))
)

;; Hvor mødes buen på cirkel 1 (slutter i vinkel v1) og buen på
;; cirkel 2 (starter i vinkel v2)? nil hvis de allerede mødes.
;; Ellers (ny-v1 ny-v2) - vinklerne i det punkt hvor cirklerne skærer
;; hinanden (fx hakket mellem kløverbladsbuens sidebuer og topbue).
(defun sb:samling (c1 r1 v1 c2 r2 v2 / e1 b2 x)
  (setq e1 (polar c1 v1 r1)
        b2 (polar c2 v2 r2))
  (if (and (> (distance (sb:2d e1) (sb:2d b2)) 0.01)
           (setq x (sb:cirkelsnit c1 r1 c2 r2
                                 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) e1 b2))))
    (list (sb:naer (angle c1 x) v1) (sb:naer (angle c2 x) v2)))
)

;; Vinklerne for zonens buer: ((sektion ydre-start ydre-slut indre-start indre-slut) ...)
;; Hænger buerne sammen (segmentbue, kurvehanksbue), er vinklerne
;; sektionens egne. Ved en kløverbladsbue klippes buerne, hvor de
;; skærer hinanden.
(defun sb:zone-vinkler (secs / n oa2 oa1 ia2 ia1 k s1 s2 x)
  (setq n   (length secs)
        oa2 (mapcar '(lambda (s) (nth 3 s)) secs)
        oa1 (mapcar '(lambda (s) (nth 4 s)) secs)
        ia2 oa2
        ia1 oa1
        k   0)
  (while (< k (1- n))
    (setq s1 (nth k secs) s2 (nth (1+ k) secs))
    (if (setq x (sb:samling (nth 0 s1) (nth 2 s1) (nth k oa1) (nth 0 s2) (nth 2 s2) (nth (1+ k) oa2)))
      (setq oa1 (sb:ersat oa1 k (car x)) oa2 (sb:ersat oa2 (1+ k) (cadr x))))
    (if (setq x (sb:samling (nth 0 s1) (nth 1 s1) (nth k ia1) (nth 0 s2) (nth 1 s2) (nth (1+ k) ia2)))
      (setq ia1 (sb:ersat ia1 k (car x)) ia2 (sb:ersat ia2 (1+ k) (cadr x))))
    (setq k (1+ k)))
  (mapcar 'list secs oa2 oa1 ia2 ia1)
)

;; Lukket polylinje rundt om zonen (UCS). Returnerer ename.
;; Indre kant -> op ad højre side -> ydre buer mod venstre ->
;; ned ad venstre side -> indre buer tilbage mod højre.
(defun sb:zone-pl (secs / ang pts bul s x sidst)
  (setq ang (sb:zone-vinkler secs)
        x   (car ang)
        s   (car x)
        pts (list (polar (nth 0 s) (nth 3 x) (nth 1 s)))
        bul (list 0.0))
  (foreach x ang
    (setq s   (car x)
          pts (append pts (list (polar (nth 0 s) (nth 1 x) (nth 2 s))))
          bul (append bul (list (sb:tan (/ (- (nth 2 x) (nth 1 x)) 4.0))))))
  (setq sidst (last ang)
        pts (append pts (list (polar (nth 0 (car sidst)) (nth 2 sidst) (nth 2 (car sidst)))))
        bul (append bul (list 0.0)))
  (foreach x (reverse ang)
    (setq s   (car x)
          pts (append pts (list (polar (nth 0 s) (nth 4 x) (nth 1 s))))
          bul (append bul (list (- (sb:tan (/ (- (nth 4 x) (nth 3 x)) 4.0)))))))
  ;; polylinjen lukkes fra venstre sides inderste punkt tilbage til start
  (sb:pl pts bul T)
)

;; Hvor krydser linjen p1-p2 (WCS) zonens kanter? Regnes i 2D (UCS).
;; Returnerer parametre t mellem 0 og 1. Ekstra delepunkter skader
;; ikke - hvert stykke testes bagefter med sb:i-zone.
(defun sb:snit-linje (p1 p2 c ri ro a2 a1 f / q1 q2 dx dy wx wy aa bb cc disk ts u k den)
  (setq q1 (trans p1 0 1) q2 (trans p2 0 1)
        dx (- (car q2) (car q1))  dy (- (cadr q2) (cadr q1))
        wx (- (car q1) (car c))   wy (- (cadr q1) (cadr c))
        aa (+ (* dx dx) (* dy dy)))
  (if (> aa 1e-12)
    (progn
      ;; buens underside (ri) og overside af topfugen (ro)
      (foreach r (list ri ro)
        (setq bb   (* 2.0 (+ (* wx dx) (* wy dy)))
              cc   (- (+ (* wx wx) (* wy wy)) (* r r))
              disk (- (* bb bb) (* 4.0 aa cc)))
        (if (>= disk 0.0)
          (setq ts (cons (/ (- (- bb) (sqrt disk)) (* 2.0 aa)) ts)
                ts (cons (/ (+ (- bb) (sqrt disk)) (* 2.0 aa)) ts))))
      ;; vederlagslinjerne i siderne
      (foreach s (list (list a2 (- f)) (list a1 f))
        (setq u   (car s)
              k   (cadr s)
              den (- (* (cos u) dy) (* (sin u) dx)))
        (if (> (abs den) 1e-12)
          (setq ts (cons (/ (- k (- (* (cos u) wy) (* (sin u) wx))) den) ts))))))
  (vl-remove-if '(lambda (x) (or (<= x 0.0) (>= x 1.0))) ts)
)

;; Klip en LINE mod zonen. Returnerer 'trim, 'slet eller nil (uberørt).
(defun sb:klip-linje (e zobj secs / obj p1 p2 len res ts pt stykker
                        ta tb m forrige ny)
  (setq obj (vlax-ename->vla-object e)
        p1  (vlax-get obj 'StartPoint)
        p2  (vlax-get obj 'EndPoint)
        len (distance p1 p2))
  (if (> len 1e-6)
    (progn
      ;; skæringer med zonens kant (regnet i 2D, så Z-højde ikke betyder noget)
      (setq ts (vl-sort (append (list 0.0 1.0)
                                (sb:snit-zonen p1 p2 secs))
                        '<))
      ;; behold de stykker der ligger uden for zonen
      (while (cdr ts)
        (setq ta (car ts) tb (cadr ts) ts (cdr ts))
        (if (> (- tb ta) 1e-6)
          (progn
            (setq m (mapcar '(lambda (a b) (+ a (* (/ (+ ta tb) 2.0) (- b a)))) p1 p2))
            (if (not (sb:i-zonen m secs))
              (if (and stykker (equal (cadr (car stykker)) ta 1e-9))
                (setq stykker (cons (list (car (car stykker)) tb) (cdr stykker)))
                (setq stykker (cons (list ta tb) stykker)))))))
      (cond
        ((null stykker) (entdel e) 'slet)
        ((and (= (length stykker) 1)
              (equal (car stykker) '(0.0 1.0) 1e-9)) nil)
        (T
         (foreach st stykker
           (setq ny (if forrige (vla-Copy obj) obj) forrige T)
           (vla-put-StartPoint ny (vlax-3d-point
             (mapcar '(lambda (a b) (+ a (* (car st) (- b a)))) p1 p2)))
           (vla-put-EndPoint ny (vlax-3d-point
             (mapcar '(lambda (a b) (+ a (* (cadr st) (- b a)))) p1 p2))))
         'trim)
      )
    )
  )
)

;; Klip en ÅBEN polylinje: eksplodér til linjer, klip, og saml igen.
;; Virker også når polylinjen ligger HELT inde i buen (uden at krydse kanten).
(defun sb:klip-aaben (e zobj secs / obj res sidst dele r aendret ssj)
  (setq obj   (vlax-ename->vla-object e)
        sidst (entlast)
        res   (vl-catch-all-apply 'vla-Explode (list obj)))
  (if (not (vl-catch-all-error-p res))
    (progn
      (setq dele (mapcar 'vlax-vla-object->ename
                         (vlax-safearray->list (vlax-variant-value res))))
      (foreach d dele
        (setq r (cond
                  ((= (cdr (assoc 0 (entget d))) "LINE")
                   (sb:klip-linje d zobj secs))
                  ((= (cdr (assoc 0 (entget d))) "ARC")
                   (sb:klip-bue d zobj secs))))
        (if r (setq aendret T)))
      ;; alle dele fra eksplosionen + nye stykker fra klipningen
      (setq ssj (ssadd))
      (foreach d (sb:nye-siden sidst) (if (entget d) (ssadd d ssj)))
      (cond
        ;; ikke berørt - behold den originale polylinje
        ((not aendret)
         (foreach d (sb:nye-siden sidst) (if (entget d) (entdel d)))
         nil)
        ((= (sslength ssj) 0) (entdel e) 'slet)
        (T (entdel e)
           (if (> (sslength ssj) 1) (command "_.JOIN" ssj ""))
           'trim))
    )
  )
)

;; Egenskaber fra et objekt som DXF-par (lag, farve, linjetype osv.)
(defun sb:dxf-egenskaber (e / ed)
  (setq ed (entget e))
  (vl-remove nil
    (mapcar '(lambda (k) (assoc k ed)) '(8 62 6 370 48 420)))
)

;; Klip en ARC eller CIRCLE mod zonen. Returnerer 'trim, 'slet eller nil.
(defun sb:klip-bue (e zobj secs / ed typ cc r sa span res pt ts
                      stykker ta tb mv forste sidste eg)
  (setq ed  (entget e)
        typ (cdr (assoc 0 ed))
        cc  (cdr (assoc 10 ed))
        r   (cdr (assoc 40 ed))
        sa  (if (= typ "ARC") (cdr (assoc 50 ed)) 0.0)
        span (if (= typ "ARC")
               (rem (+ (- (cdr (assoc 51 ed)) sa) (* 4.0 pi)) (* 2.0 pi))
               (* 2.0 pi))
        ts  (list 0.0 span))
  (if (and (= typ "ARC") (< span 1e-9)) (setq span (* 2.0 pi) ts (list 0.0 span)))
  ;; skæringer -> vinkler målt fra startvinklen
  (setq res (vl-catch-all-apply 'vlax-invoke
              (list (vlax-ename->vla-object e) 'IntersectWith zobj 0)))
  (if (and res (not (vl-catch-all-error-p res)))
    (while res
      (setq pt  (list (car res) (cadr res))
            res (cdddr res)
            ts  (cons (rem (+ (- (angle cc pt) sa) (* 4.0 pi)) (* 2.0 pi)) ts))))
  (setq ts (vl-sort (vl-remove-if '(lambda (x) (> x (+ span 1e-9))) ts) '<))
  ;; stykker uden for zonen
  (while (cdr ts)
    (setq ta (car ts) tb (cadr ts) ts (cdr ts))
    (if (> (- tb ta) 1e-7)
      (progn
        (setq mv (+ sa (/ (+ ta tb) 2.0)))
        (if (not (sb:i-zonen (list (+ (car cc) (* r (cos mv))) (+ (cadr cc) (* r (sin mv))) 0.0)
                             secs))
          (if (and stykker (equal (cadr (car stykker)) ta 1e-9))
            (setq stykker (cons (list (car (car stykker)) tb) (cdr stykker)))
            (setq stykker (cons (list ta tb) stykker)))))))
  (setq stykker (reverse stykker))
  ;; cirkel: saml første og sidste stykke hvis de mødes ved 0
  (if (and (= typ "CIRCLE") (> (length stykker) 1)
           (equal (car (car stykker)) 0.0 1e-9)
           (equal (cadr (last stykker)) span 1e-9))
    (setq forste (car stykker)
          sidste (last stykker)
          stykker (append (list (list (car sidste) (+ span (cadr forste))))
                          (cdr (reverse (cdr (reverse stykker)))))))
  (cond
    ((null stykker) (entdel e) 'slet)
    ((and (= (length stykker) 1) (equal (car stykker) (list 0.0 span) 1e-9)) nil)
    (T
     (setq eg (sb:dxf-egenskaber e))
     (foreach st stykker
       (entmake (append (list '(0 . "ARC"))
                        eg
                        (list (assoc 10 ed) (assoc 40 ed)
                              (cons 50 (+ sa (car st)))
                              (cons 51 (+ sa (cadr st)))))))
     (entdel e)
     'trim)
  )
)

;; Klip en XLINE (hjælpelinje) eller RAY mod zonen.
(defun sb:klip-straale (e zobj secs / ed typ bp u res pt ts graenser
                          stykker ta tb m eg pa)
  (setq ed  (entget e)
        typ (cdr (assoc 0 ed))
        bp  (cdr (assoc 10 ed))
        u   (cdr (assoc 11 ed))
        res (vl-catch-all-apply 'vlax-invoke
              (list (vlax-ename->vla-object e) 'IntersectWith zobj 0)))
  (if (and res (not (vl-catch-all-error-p res)))
    (while res
      (setq pt  (list (car res) (cadr res) (caddr res))
            res (cdddr res)
            ts  (cons (apply '+ (mapcar '* (mapcar '- pt bp) u)) ts))))
  (if (not ts)
    nil
    (progn
      (setq ts (vl-sort ts '<))
      (if (= typ "RAY") (setq ts (vl-remove-if '(lambda (x) (< x 0.0)) ts)))
      ;; graenser: nil = uendelig
      (setq graenser (append (list (if (= typ "RAY") 0.0 nil)) ts (list nil)))
      (while (cdr graenser)
        (setq ta (car graenser) tb (cadr graenser) graenser (cdr graenser))
        (setq m (cond ((null ta) (- tb 1000.0))
                      ((null tb) (+ ta 1000.0))
                      (T (/ (+ ta tb) 2.0))))
        (if (and (or (null ta) (null tb) (> (- tb ta) 1e-6))
                 (not (sb:i-zonen (mapcar '(lambda (b d) (+ b (* m d))) bp u)
                                  secs)))
          (setq stykker (cons (list ta tb) stykker))))
      (setq eg (sb:dxf-egenskaber e))
      (foreach st stykker
        (setq ta (car st) tb (cadr st))
        (cond
          ((null ta)          ; fra -uendelig til tb: stråle bagud
           (setq pa (mapcar '(lambda (b d) (+ b (* tb d))) bp u))
           (entmake (append (list '(0 . "RAY") '(100 . "AcDbEntity"))
                            eg (list '(100 . "AcDbRay") (cons 10 pa) (cons 11 (mapcar '- u))))))
          ((null tb)          ; fra ta til +uendelig: stråle frem
           (setq pa (mapcar '(lambda (b d) (+ b (* ta d))) bp u))
           (entmake (append (list '(0 . "RAY") '(100 . "AcDbEntity"))
                            eg (list '(100 . "AcDbRay") (cons 10 pa) (cons 11 u)))))
          (T
           (entmake (append (list '(0 . "LINE")) eg
                            (list (cons 10 (mapcar '(lambda (b d) (+ b (* ta d))) bp u))
                                  (cons 11 (mapcar '(lambda (b d) (+ b (* tb d))) bp u))))))))
      (entdel e)
      (if stykker 'trim 'slet)
    )
  )
)

;; Finish: fjern småstumper og dobbelte sten efter trim
;; Returnerer (antal-dubletter antal-smaastykker)
(defun sb:finish (poly excl / ss i e ed typ obj ndub nsmaa noegler n)
  (setq ndub 0 nsmaa 0)
  ;; 1. småstumper (under 1 mm / 5 mm2) omkring buen
  (setq ss (ssget "_CP" poly '((0 . "LINE,ARC,LWPOLYLINE"))) i 0)
  (if ss
    (while (< i (sslength ss))
      (setq e (ssname ss i) i (1+ i))
      (if (and (entget e) (not (member e excl)))
        (progn
          (setq obj (vlax-ename->vla-object e)
                typ (cdr (assoc 0 (entget e))))
          (if (or (and (member typ '("LINE" "ARC"))
                       (< (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 1.0))
                  (and (= typ "LWPOLYLINE")
                       (= 1 (logand 1 (cdr (assoc 70 (entget e)))))
                       (< (vla-get-Area obj) 5.0))
                  (and (= typ "LWPOLYLINE")
                       (= 0 (logand 1 (cdr (assoc 70 (entget e)))))
                       (< (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)) 1.0)))
            (progn (entdel e) (setq nsmaa (1+ nsmaa))))))))
  ;; 2. dobbelte sten (samme punkter) blandt buens egne dele
  (foreach e excl
    (if (and (entget e) (= (cdr (assoc 0 (entget e))) "LWPOLYLINE"))
      (progn
        (setq n (vl-sort
                  (mapcar '(lambda (x) (strcat (rtos (cadr x) 2 1) "," (rtos (caddr x) 2 1)))
                          (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget e)))
                  '<))
        (if (member n noegler)
          (progn (entdel e) (setq ndub (1+ ndub)))
          (setq noegler (cons n noegler))))))
  (list ndub nsmaa)
)

;; Egenskaber (lag farve linjetype linjetykkelse) som stenen VISES med.
;; For blokke: lag 0 og ByBlock overtages fra blokken.
(defun sb:egenskaber (kurve blok / lag farve lt lw)
  (setq lag   (vla-get-Layer kurve)
        farve (vla-get-Color kurve)
        lt    (vla-get-Linetype kurve)
        lw    (vla-get-Lineweight kurve))
  (if blok
    (progn
      (if (= lag "0")                  (setq lag   (vla-get-Layer blok)))
      (if (= farve 0)                  (setq farve (vla-get-Color blok)))
      (if (= (strcase lt) "BYBLOCK")   (setq lt    (vla-get-Linetype blok)))
      (if (= lw -2)                    (setq lw    (vla-get-Lineweight blok)))))
  (list lag farve lt lw)
)

(defun sb:saet-egenskaber (obj eg)
  (vla-put-Layer obj (nth 0 eg))
  (vla-put-Color obj (nth 1 eg))
  (vl-catch-all-apply 'vla-put-Linetype (list obj (nth 2 eg)))
  (vl-catch-all-apply 'vla-put-Lineweight (list obj (nth 3 eg)))
)

;; Lav regioner af en mursten. Lukket polylinje bruges direkte,
;; en blok eksploderes midlertidigt. Sætter *sb-eg* til stenens
;; egenskaber. Returnerer liste af regioner eller nil.
;; Ukendte objekttyper gemmes i *sb-ukendt* til fejlfinding.
(defun sb:sten-regioner (spc e / obj typ kurver tmp reg)
  (setq obj (vlax-ename->vla-object e)
        typ (cdr (assoc 0 (entget e))))
  (cond
    ((= typ "LWPOLYLINE") (setq kurver (list obj)))
    ((= typ "INSERT")
     (setq tmp (sb:eksploder-blok obj))
     (foreach o tmp
       (if (member (vla-get-ObjectName o)
                   '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDbArc"
                     "AcDbCircle" "AcDbEllipse" "AcDbSpline"))
         (setq kurver (cons o kurver))
         (if (not (member (vla-get-ObjectName o) *sb-ukendt*))
           (setq *sb-ukendt* (cons (vla-get-ObjectName o) *sb-ukendt*))))))
  )
  (if kurver
    (setq *sb-eg* (sb:egenskaber (car kurver) (if (= typ "INSERT") obj))))
  (if kurver
    (progn
      (setq reg (vl-catch-all-apply 'vla-AddRegion
                  (list spc (vlax-make-variant
                              (vlax-safearray-fill
                                (vlax-make-safearray vlax-vbObject (cons 0 (1- (length kurver))))
                                kurver)))))
      (if (vl-catch-all-error-p reg)
        (setq reg nil)
        (setq reg (vlax-safearray->list (vlax-variant-value reg))))))
  ;; slet midlertidige objekter fra eksploderet blok
  (foreach o tmp (vl-catch-all-apply 'vla-Delete (list o)))
  reg
)

;; Trim alle mursten (lukkede polylinjer eller blokke) der overlapper
;; zonen. secs = zonens buestykker (se sb:zone-pl), i UCS.
;; excl = enames der ikke må røres (buens egne sten).
;; Returnerer (antal-trimmet antal-slettet dubletter småstumper)
(defun sb:trim (doc secs excl / spc zpl zreg poly ang s x i k ns ss e obj typ c ri ro a2 a1
                  regs r a0 a aendret nye ssj ekko ntrim nslet nstor sidst
                  nundersogt typer ssalle zobj nikke ikketyper fin)
  ;; Zonen går fra buens underside til overside af topfugen.
  ;; I siderne trimmes præcist til buens yderkant (vederlagslinjen),
  ;; hvor buens yderste fuger allerede ligger inden for omridset.
  (setq spc (if (= (getvar "cvport") 1) (vla-get-PaperSpace doc) (vla-get-ModelSpace doc))
        ntrim 0 nslet 0 nstor 0 nundersogt 0 nikke 0 ikketyper nil
        *sb-ukendt* nil)

  ;; Zone: bue + fuge langs toppen
  (setq zpl  (sb:zone-pl secs)
        zreg (sb:region spc (vlax-ename->vla-object zpl))
        zobj (vlax-ename->vla-object zpl)
        excl (cons zpl excl))
  (if (not zreg)
    (progn (entdel zpl) (princ "\nKunne ikke lave trim-zonen.") (list 0 0 0 0))
    (progn
      ;; Søgeområde: ydre buer fra højre mod venstre, så indre buer tilbage.
      ;; Lidt større end zonen (5 mm ude, 10 mm inde i åbningen), og
      ;; lidt forbi vederlagene i siderne. Buerne klippes hvor de mødes
      ;; (kløverbladsbuens hak), så polygonen ikke krydser sig selv.
      (setq poly nil
            ang  (sb:zone-vinkler
                   (mapcar '(lambda (s) (list (nth 0 s) (max 1.0 (- (nth 1 s) 10.0))
                                              (+ (nth 2 s) 5.0) (nth 3 s) (nth 4 s)))
                           secs))
            ns   (length ang)
            k    0)
      (foreach x ang
        (setq s (car x) c (nth 0 s) ro (nth 2 s) a2 (nth 1 x) a1 (nth 2 x))
        (if (= k 0) (setq a2 (- a2 0.02)))
        (if (= k (1- ns)) (setq a1 (+ a1 0.02)))
        (setq i (if (= k 0) 0 1))
        (while (<= i 16)
          (setq poly (append poly (list (polar c (+ a2 (* (- a1 a2) (/ i 16.0))) ro)))
                i (1+ i)))
        (setq k (1+ k)))
      (setq k 0)
      (foreach x (reverse ang)
        (setq s (car x) c (nth 0 s) ri (nth 1 s) a2 (nth 3 x) a1 (nth 4 x))
        (if (= k 0) (setq a1 (+ a1 0.02)))
        (if (= k (1- ns)) (setq a2 (- a2 0.02)))
        (setq i (if (= k 0) 16 15))
        (while (>= i 0)
          (setq poly (append poly (list (polar c (+ a2 (* (- a1 a2) (/ i 16.0))) ri)))
                i (1- i)))
        (setq k (1+ k)))

      (setq ss (ssget "_CP" poly '((0 . "LWPOLYLINE,INSERT,LINE,ARC,CIRCLE,XLINE,RAY,SPLINE,ELLIPSE,HATCH"))))

      (setq ekko (getvar "cmdecho"))
      (setvar "cmdecho" 0)
      (setq i 0)
      (if ss
        (while (< i (sslength ss))
          (setq e (ssname ss i) i (1+ i))
          (if (not (member e excl))
            (progn
              (setq obj (vlax-ename->vla-object e)
                    typ (cdr (assoc 0 (entget e))))
              (cond
                ;; Linjer (fx åbningens sider og overkant)
                ((= typ "LINE")
                 (setq nundersogt (1+ nundersogt)
                       r (sb:klip-linje e zobj secs))
                 (cond ((= r 'trim) (setq ntrim (1+ ntrim)))
                       ((= r 'slet) (setq nslet (1+ nslet)))))
                ;; Åbne polylinjer
                ((and (= typ "LWPOLYLINE") (= 0 (logand 1 (cdr (assoc 70 (entget e))))))
                 (setq nundersogt (1+ nundersogt)
                       r (sb:klip-aaben e zobj secs))
                 (cond ((= r 'trim) (setq ntrim (1+ ntrim)))
                       ((= r 'slet) (setq nslet (1+ nslet)))))
                ;; Buer og cirkler (fx hjælpecirkler fra opsnøring)
                ((member typ '("ARC" "CIRCLE"))
                 (setq nundersogt (1+ nundersogt)
                       r (sb:klip-bue e zobj secs))
                 (cond ((= r 'trim) (setq ntrim (1+ ntrim)))
                       ((= r 'slet) (setq nslet (1+ nslet)))))
                ;; Hjælpelinjer
                ((member typ '("XLINE" "RAY"))
                 (setq nundersogt (1+ nundersogt)
                       r (sb:klip-straale e zobj secs))
                 (cond ((= r 'trim) (setq ntrim (1+ ntrim)))
                       ((= r 'slet) (setq nslet (1+ nslet)))))
                ;; Kan ikke trimmes automatisk
                ((member typ '("SPLINE" "ELLIPSE" "HATCH"))
                 (setq nikke (1+ nikke))
                 (if (not (member typ ikketyper)) (setq ikketyper (cons typ ikketyper))))
                ;; Store blokke (fx et helt array) springes over
                ((and (= typ "INSERT") (> (sb:boksareal obj) 60000.0))
                 (setq nstor (1+ nstor)))
                ;; Lukkede polylinjer og blokke (mursten, åbningens omrids)
                (T
                  (setq nundersogt (1+ nundersogt)
                        regs  (sb:sten-regioner spc e)
                        aendret nil)
                  ;; Træk zonen fra hver region
                  (foreach r regs
                    (setq a0 (vla-get-Area r))
                    (sb:saet-egenskaber r *sb-eg*)
                    (vl-catch-all-apply 'vla-Boolean (list r 2 (vla-Copy zreg)))
                    (if (not (equal (vla-get-Area r) a0 0.5)) (setq aendret T)))
                  (if (not aendret)
                    ;; ikke berørt - behold originalen
                    (foreach r regs (vla-Delete r))
                    ;; berørt - erstat med trimmede polylinjer
                    (progn
                      (setq a 0.0)
                      (foreach r regs
                        (setq a (+ a (vla-get-Area r)))
                        (if (< (vla-get-Area r) 1.0)
                          (vla-Delete r)
                          (progn
                            (setq nye (sb:explode r))
                            (vla-Delete r)
                            (setq ssj (ssadd))
                            (foreach n nye (ssadd n ssj))
                            (setq sidst (entlast))
                            (if (> (sslength ssj) 1)
                              (command "_.JOIN" ssj ""))
                            ;; nye objekter får stenens egne egenskaber
                            (foreach n (sb:nye-siden sidst)
                              (sb:saet-egenskaber (vlax-ename->vla-object n) *sb-eg*))
                            (foreach n nye
                              (if (entget n)
                                (sb:saet-egenskaber (vlax-ename->vla-object n) *sb-eg*)))))
                      )
                      (entdel e)
                      (if (< a 1.0) (setq nslet (1+ nslet)) (setq ntrim (1+ ntrim)))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (setvar "cmdecho" ekko)
      (vla-Delete zreg)
      (entdel zpl)

      ;; Finish: småstumper og dobbelte sten
      (setq fin (sb:finish poly excl))
      (if (> nikke 0)
        (progn
          (princ (strcat "\nBemærk: " (itoa nikke) " objekt(er) går gennem buen men kan ikke trimmes automatisk:"))
          (foreach x ikketyper (princ (strcat " " x)))
          (princ " - brug TRIM på dem.")))

      ;; Fejlfinding: hvad ligger der i området?
      (if (= (+ ntrim nslet) 0)
        (progn
          (setq ssalle (ssget "_CP" poly) typer nil i 0)
          (if ssalle
            (while (< i (sslength ssalle))
              (setq e (ssname ssalle i) i (1+ i))
              (if (not (member e excl))
                (progn
                  (setq typ (cdr (assoc 0 (entget e))))
                  (if (assoc typ typer)
                    (setq typer (subst (cons typ (1+ (cdr (assoc typ typer)))) (assoc typ typer) typer))
                    (setq typer (cons (cons typ 1) typer)))))))
          (princ "\nIngen mursten blev trimmet.")
          (princ (strcat "\n  Undersøgt: " (itoa nundersogt)
                         "   For store (sprunget over): " (itoa nstor)))
          (princ "\n  Objekter fundet i området (ud over buen):")
          (if typer
            (foreach x typer (princ (strcat "  " (car x) " x " (itoa (cdr x)))))
            (princ "  ingen - er buen synlig på skærmen?"))
          (if *sb-ukendt*
            (progn
              (princ "\n  Blokkene indeholder:")
              (foreach x *sb-ukendt* (princ (strcat "  " x)))))
        )
      )
      (list ntrim nslet (car fin) (cadr fin))
    )
  )
)

;; Punkter (UCS) og bulges i en polylinje: (punkter bulges lukket)
(defun sb:pl-data (e / ed pts bul)
  (setq ed (entget e))
  (foreach x ed
    (cond ((= (car x) 10) (setq pts (cons (trans (list (cadr x) (caddr x) 0.0) 0 1) pts)))
          ((= (car x) 42) (setq bul (cons (cdr x) bul)))))
  (list (reverse pts) (reverse bul) (= 1 (logand 1 (cdr (assoc 70 ed)))))
)

;; Buerne i en polylinje: liste af (centrum radius startvinkel slutvinkel)
;; Vinklerne går mod uret fra start til slut (UCS).
(defun sb:pl-buer (e / d pts bul n i b p q k c th a lst)
  (setq d (sb:pl-data e) pts (car d) bul (cadr d) n (length pts) i 0)
  (while (< i (if (caddr d) n (1- n)))
    (setq b (nth i bul))
    (if (and b (/= b 0.0))
      (progn
        (setq p  (nth i pts)
              q  (nth (rem (1+ i) n) pts)
              k  (/ (- 1.0 (* b b)) (* 4.0 b))
              c  (list (- (/ (+ (car p) (car q)) 2.0) (* k (- (cadr q) (cadr p))))
                       (+ (/ (+ (cadr p) (cadr q)) 2.0) (* k (- (car q) (car p))))
                       (caddr p))
              th (abs (* 4.0 (atan b)))
              a  (if (> b 0.0) (angle c p) (angle c q))
              lst (cons (list c (distance c p) a (+ a th)) lst))))
    (setq i (1+ i)))
  (reverse lst)
)

;; Er polylinjen buens omrids eller topfuge? Den har to buer med
;; samme centrum og samme vinkel (inderside og yderside). Stenene har
;; ikke - deres to buer spænder over forskellige vinkler.
;; En kløverbladsbues omrids og topfuge har buer om 3 forskellige
;; centre (buerne er klippet i hakket, så vinklerne er ikke ens).
;; En sten har højst 2 centre (låsestenen).
(defun sb:er-kant (e / buer ok centre)
  (if (= 1 (logand 1 (cdr (assoc 70 (entget e)))))
    (progn
      (setq buer (sb:pl-buer e))
      (foreach b1 buer
        (foreach b2 buer
          (if (and (not (eq b1 b2))
                   (equal (car b1) (car b2) 0.5)
                   (not (equal (cadr b1) (cadr b2) 0.5))
                   (equal (- (nth 3 b1) (nth 2 b1)) (- (nth 3 b2) (nth 2 b2)) 1e-4))
            (setq ok T))))
      (foreach b buer
        (if (not (vl-some '(lambda (c) (equal c (car b) 0.5)) centre))
          (setq centre (cons (car b) centre))))
      (if (>= (length centre) 3) (setq ok T))))
  ok
)

;; Er zonen en kløverbladsbue? 3 stykker med samme radius.
(defun kl:er-klover (secs)
  (and (= (length secs) 3)
       (equal (nth 1 (car secs)) (nth 1 (cadr secs)) 0.5)
       (equal (nth 1 (car secs)) (nth 1 (caddr secs)) 0.5))
)

;; Er polylinjen en almindelig retvinklet mursten (vandrette/lodrette
;; sider uden buer)? Sådan en er ikke en del af buen.
(defun sb:retvinklet-p (e / d pts n i p q ok)
  (setq d (sb:pl-data e) pts (car d) n (length pts) i 0 ok T)
  (if (vl-some '(lambda (b) (/= b 0.0)) (cadr d)) (setq ok nil))
  (while (and ok (< i n))
    (setq p (nth i pts) q (nth (rem (1+ i) n) pts))
    (if (not (or (equal (car p) (car q) 0.01) (equal (cadr p) (cadr q) 0.01)))
      (setq ok nil))
    (setq i (1+ i)))
  ok
)

;; Kommando: trim mursten bag en segmentbue eller kurvehanksbue
;; der allerede er tegnet
(defun c:BUETRIM (/ *error* doc ss i e d kant grupper g fundet secs excl res
                    m ri pts bul n)
  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )
  (princ "\nVælg hele buen (alle sten, omrids og topfuge): ")
  (if (not (setq ss (ssget '((0 . "LWPOLYLINE"))))) (exit))

  ;; 1. Find omrids og topfuge
  (setq i 0)
  (while (< i (sslength ss))
    (setq e (ssname ss i) i (1+ i))
    (if (sb:er-kant e) (setq kant (cons e kant))))
  (if (not kant)
    (progn (princ "\nFandt ingen bue i det valgte (omridset mangler - er buen allerede trimmet?).")
           (exit)))

  ;; 2. Saml buerne efter centrum: (centrum ri ro start slut)
  ;;    Segmentbue = 1 centrum, kurvehanks- og kløverbladsbue = 3 (eller 5).
  ;;    Vinklerne er de yderste af buerne (kløverbladsbuens ydre buer er
  ;;    klippet i hakket, de indre er hele).
  (foreach e kant
    (foreach b (sb:pl-buer e)
      (setq fundet nil)
      (setq grupper
        (mapcar '(lambda (g / v0)
                   (if (and (not fundet) (equal (car g) (car b) 0.5))
                     (progn
                       (setq fundet T
                             v0     (sb:naer (nth 2 b) (nth 3 g)))
                       (list (car g) (min (cadr g) (cadr b)) (max (caddr g) (cadr b))
                             (min (nth 3 g) v0) (max (nth 4 g) (+ v0 (- (nth 3 b) (nth 2 b))))))
                     g))
                grupper))
      (if (not fundet)
        (setq grupper (cons (list (car b) (cadr b) (cadr b) (nth 2 b) (nth 3 b)) grupper)))))

  ;; 3. Ordn stykkerne fra højre mod venstre (x i midten af stykket)
  (setq secs (vl-sort grupper
               '(lambda (g1 g2)
                  (> (car (polar (car g1) (/ (+ (nth 3 g1) (nth 4 g1)) 2.0) (cadr g1)))
                     (car (polar (car g2) (/ (+ (nth 3 g2) (nth 4 g2)) 2.0) (cadr g2)))))))

  ;; 4. Beskyt buens egne sten: alle hjørner ligger i buen, og stenen
  ;;    er ikke en almindelig retvinklet mursten
  (setq i 0)
  (while (< i (sslength ss))
    (setq e (ssname ss i) i (1+ i))
    (if (and (not (member e kant))
             (= 1 (logand 1 (cdr (assoc 70 (entget e)))))
             (not (sb:retvinklet-p e))
             (vl-every '(lambda (p)
                          (vl-some '(lambda (s)
                                      (sb:i-zone (trans p 1 0) (nth 0 s) (- (nth 1 s) 1.0)
                                                 (+ (nth 2 s) 1.0) (nth 3 s) (nth 4 s) 1.0))
                                   secs))
                       (car (sb:pl-data e))))
      (setq excl (cons e excl))))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq res (sb:trim doc secs excl))

  ;; 5. Fjern topfuge og omrids; omridsets lige stykker bliver
  ;;    til de 2 vederlagslinjer
  (foreach e kant
    (if (entget e)
      (progn
        ;; omridset er den der når ind til undersiden
        (if (vl-some '(lambda (b)
                        (vl-some '(lambda (s) (and (equal (car s) (car b) 0.5)
                                                   (equal (nth 1 s) (cadr b) 0.5)))
                                 secs))
                     (sb:pl-buer e))
          (progn
            (setq d (sb:pl-data e) pts (car d) bul (cadr d) n (length pts) i 0)
            (while (< i n)
              (if (= (nth i bul) 0.0)
                (sb:linje (nth i pts) (nth (rem (1+ i) n) pts)))
              (setq i (1+ i)))))
        (entdel e))))
  (vla-EndUndoMark doc)
  (princ (strcat "\n" (cond ((= (length secs) 1) "Segmentbue")
                            ((kl:er-klover secs) "Kløverbladsbue")
                            (T "Kurvehanksbue"))
                 " fundet (" (itoa (length secs)) " centr" (if (= (length secs) 1) "um" "e") ", "
                 (itoa (length excl)) " sten i buen)."))
  (princ (strcat "\n" (itoa (car res)) " objekter trimmet, " (itoa (cadr res)) " slettet."
                 "\nFinish: " (itoa (caddr res)) " dobbelte sten, "
                 (itoa (cadddr res)) " småstumper fjernet."))
  (princ)
)

;; ============================================================
;; BUENS DATA I TEGNINGEN (bruges af BUEMAAL)
;; Gemmes som en XRECORD i ordbogen BUER_DATA under gruppens navn.
;; typ = "SEG" eller "KURVE", vals = liste af tal (WCS).
;; ============================================================

;; Gruppenavnet fra sb:gruppe ("KURVEBUE2 (48 objekter)" -> "KURVEBUE2")
(defun sb:gruppenavn (g / i)
  (if (= (type g) 'STR)
    (if (setq i (vl-string-search " " g)) (substr g 1 i) g))
)

(defun sb:data-gem (gnavn typ vals / navn d x)
  (if (setq navn (sb:gruppenavn gnavn))
    (progn
      (if (not (setq d (cdr (assoc -1 (dictsearch (namedobjdict) "BUER_DATA")))))
        (progn
          (setq d (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
          (dictadd (namedobjdict) "BUER_DATA" d)))
      (if (dictsearch d navn) (dictremove d navn))
      (setq x (entmakex (append (list '(0 . "XRECORD") '(100 . "AcDbXrecord") (cons 1 typ))
                                (mapcar '(lambda (v) (cons 40 (float v))) vals))))
      (if x (dictadd d navn x))))
)

;; Returnerer (typ tal ...) eller nil
(defun sb:data-hent (navn / d x)
  (if (and (setq d (dictsearch (namedobjdict) "BUER_DATA"))
           (setq x (dictsearch (cdr (assoc -1 d)) navn)))
    (cons (cdr (assoc 1 x))
          (mapcar 'cdr (vl-remove-if-not '(lambda (p) (= (car p) 40)) x))))
)

;; Navnet på den gruppe et objekt ligger i (SEGBUE.. / KURVEBUE..) eller nil
(defun sb:objekt-gruppe (e / ejere g navn nr)
  ;; gruppen står som "reaktor" (330) på objektet
  (foreach p (entget e)
    (if (= (car p) 330) (setq ejere (cons (cdr p) ejere))))
  (setq g (entget (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP")))))
  (while g
    (if (= (car (car g)) 3) (setq nr (cdr (car g))))
    (if (and nr (= (car (car g)) 350) (member (cdr (car g)) ejere)
             (wcmatch (strcase nr) "SEGBUE*,KURVEBUE*,KLOVERBUE*"))
      (setq navn nr))
    (setq g (cdr g)))
  navn
)

;; ============================================================
;; BUEMAAL - mål og opsnøring på en bue tegnet med SEGBUE/KURVEBUE
;; Tegnes på det NUVÆRENDE lag med den nuværende målestil.
;; ============================================================

;; Hjælpere (bruger spc, th og *bm-obj* fra c:BUEMAAL)
(defun bm:gem (res)
  (if (and res (not (vl-catch-all-error-p res)))
    (setq *bm-obj* (cons (vlax-vla-object->ename res) *bm-obj*)))
  res
)
(defun bm:3d (p) (vlax-3d-point (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))))
;; lineært mål mellem p1 og p2, målelinjen gennem tp, rot = 0 vandret / pi/2 lodret
(defun bm:maal (p1 p2 tp rot)
  (bm:gem (vl-catch-all-apply 'vla-AddDimRotated
            (list spc (bm:3d p1) (bm:3d p2) (bm:3d tp) rot)))
)
(defun bm:radius (c pt)
  (bm:gem (vl-catch-all-apply 'vla-AddDimRadial
            (list spc (bm:3d c) (bm:3d pt) (* 2.0 th))))
)
(defun bm:vinkel (c pa pb tp)
  (bm:gem (vl-catch-all-apply 'vla-AddDimAngular
            (list spc (bm:3d c) (bm:3d pa) (bm:3d pb) (bm:3d tp))))
)
(defun bm:linje (a b)
  (bm:gem (vl-catch-all-apply 'vla-AddLine (list spc (bm:3d a) (bm:3d b))))
)
(defun bm:tekst (p s)
  (bm:gem (vl-catch-all-apply 'vla-AddText (list spc s (bm:3d p) th)))
)
;; centrum: kryds + navn
(defun bm:centrum (c navn)
  (bm:linje (polar c 0.0 th) (polar c pi th))
  (bm:linje (polar c (/ pi 2.0) th) (polar c (* 1.5 pi) th))
  (bm:tekst (list (+ (car c) (* 1.2 th)) (+ (cadr c) (* 0.5 th)) (caddr c)) navn)
)

;; --- Segmentbue ---
;; vals: cx cy cz ri re a2 a1 topf p1x p1y p1z p2x p2y p2z h k n s D
(defun bm:seg (v / c ri re a2 a1 p1 p2 h k vm top ti hj)
  (setq c  (list (nth 0 v) (nth 1 v) (nth 2 v))
        ri (nth 3 v) re (nth 4 v) a2 (nth 5 v) a1 (nth 6 v)
        p1 (list (nth 8 v) (nth 9 v) (nth 10 v))
        p2 (list (nth 11 v) (nth 12 v) (nth 13 v))
        h  (nth 14 v) k (fix (+ 0.5 (nth 15 v)))
        vm (list (/ (+ (car p1) (car p2)) 2.0) (cadr p1) (caddr p1))   ; vederlagets midte
        ti (polar c (/ pi 2.0) ri)                                    ; top underside
        top (polar c (/ pi 2.0) re)                                   ; top overside
        hj (polar c a2 re))                                           ; højre øverste hjørne
  ;; spændvidde og stik
  (bm:maal p1 p2 (list (car vm) (- (cadr vm) (* 4.0 th))) 0.0)
  (bm:maal vm ti (list (- (car vm) (* 0.25 (distance p1 p2))) (cadr vm)) (/ pi 2.0))
  ;; højder til højre: hjørne -> overkant og vederlag -> overkant
  (bm:maal hj top (list (+ (car hj) (* 3.0 th)) (cadr hj)) (/ pi 2.0))
  (bm:maal p2 top (list (+ (car hj) (* 7.0 th)) (cadr p2)) (/ pi 2.0))
  ;; radius, vinkel, centrum og stråler (opsnøring)
  (bm:radius c ti)
  (bm:vinkel c (polar c a1 (* 0.5 ri)) (polar c a2 (* 0.5 ri)) (polar c (/ pi 2.0) (* 0.35 ri)))
  (bm:centrum c "M")
  (bm:linje c (polar c a1 re))
  (bm:linje c (polar c a2 re))
  ;; centrummets placering under vederlaget
  (bm:maal vm c (list (- (car vm) (* 3.0 th)) (cadr c)) (/ pi 2.0))
  (bm:tekst (list (+ (car hj) (* 1.0 th)) (+ (cadr hj) (* 0.5 th)) (caddr hj))
            (strcat "Hjørne " (itoa k) " skifter under OK"))
)

;; --- Kurvehanksbue ---
;; vals: mx my mz a b D topf k j n antal-centre, derefter pr. bue: cx cy r a0 a1 (lokalt)
(defun bm:kurve (v / m a b D nc geo i g L navne halv c)
  (setq m  (list (nth 0 v) (nth 1 v) (nth 2 v))
        a  (nth 3 v) b (nth 4 v) D (nth 5 v)
        nc (fix (+ 0.5 (nth 10 v)))
        i  0)
  (while (< i nc)
    (setq geo (cons (list (nth (+ 11 (* i 5)) v) (nth (+ 12 (* i 5)) v) (nth (+ 13 (* i 5)) v)
                          (nth (+ 14 (* i 5)) v) (nth (+ 15 (* i 5)) v)) geo)
          i (1+ i)))
  (setq geo (reverse geo))
  (defun L (x y) (list (+ (car m) x) (+ (cadr m) y) (caddr m)))
  (setq navne (if (= nc 5) '("A" "C" "B" "C" "A") '("A" "B" "A"))
        halv  (/ nc 2))
  ;; spændvidde, pilhøjde og højde til overkant
  (bm:maal (L (- a) 0.0) (L a 0.0) (L 0.0 (* -4.0 th)) 0.0)
  (bm:maal (L 0.0 0.0) (L 0.0 b) (L (* -0.5 a) (* 0.5 b)) (/ pi 2.0))
  (bm:maal (L (+ a D) 0.0) (L 0.0 (+ b D)) (L (+ a D (* 4.0 th)) 0.0) (/ pi 2.0))
  ;; hver bue: centrum, stråler, radius og vinkel
  (setq i 0)
  (foreach g geo
    (setq c (L (car g) (cadr g)))
    (bm:linje c (polar c (nth 3 g) (+ (nth 2 g) D)))
    (bm:linje c (polar c (nth 4 g) (+ (nth 2 g) D)))
    (if (<= i halv)                                   ; venstre halvdel + top
      (progn
        (bm:centrum c (nth i navne))
        (bm:radius c (polar c (/ (+ (nth 3 g) (nth 4 g)) 2.0) (nth 2 g)))
        (bm:vinkel c (polar c (nth 3 g) (* 0.5 (nth 2 g))) (polar c (nth 4 g) (* 0.5 (nth 2 g)))
                   (polar c (/ (+ (nth 3 g) (nth 4 g)) 2.0) (* 0.35 (nth 2 g))))
        ;; centrummets placering: lodret fra vederlag og vandret fra midten
        (if (> (abs (cadr g)) 0.5)
          (bm:maal (L (car g) 0.0) c (L (- (car g) (* 3.0 th)) 0.0) (/ pi 2.0)))
        (if (> (abs (car g)) 0.5)
          (bm:maal (L 0.0 (cadr g)) c (L 0.0 (- (cadr g) (* 3.0 th))) 0.0)))
      (bm:centrum c (nth i navne)))
    (setq i (1+ i)))
  (bm:tekst (L (- a) (* -8.0 th))
            (strcat (itoa nc) "-centret kurvehanksbue  pil " (sb:tal b 0) " / spænd "
                    (sb:tal (* 2.0 a) 0) " = 1:" (sb:tal (/ (* 2.0 a) b) 1)))
)

;; --- Kløverbladsbue ---
;; vals: mx my mz r D topf skifter m jl n ju stenh [skifte stående]
(defun bm:klover (v / m r D q st ok xm o L)
  (setq m  (list (nth 0 v) (nth 1 v) (nth 2 v))       ; buens start på LOD
        r  (nth 3 v) D (nth 4 v)
        st (if (> (length v) 13) (nth 13 v) 0.0)      ; stående fals under buen
        ok (+ (* 2.0 r) D)
        q  (car (kl:knaek r D (nth 11 v))))
  (defun L (x y) (list (+ (car m) x) (+ (cadr m) y) (caddr m)))
  ;; vandret: 4 x 1/4 på buens start + spændvidden fals til fals
  (setq xm (list (* -2.0 r) (- r) 0.0 r (* 2.0 r)))
  (while (cdr xm)
    (setq o (bm:maal (L (car xm) 0.0) (L (cadr xm) 0.0) (L 0.0 (- (+ st (* 4.0 th)))) 0.0))
    (if (and o (not (vl-catch-all-error-p o)))
      (vl-catch-all-apply 'vla-put-TextOverride (list o "¼ = <>")))
    (setq xm (cdr xm)))
  (bm:maal (L (* -2.0 r) (- st)) (L (* 2.0 r) (- st)) (L 0.0 (- (+ st (* 8.0 th)))) 0.0)
  ;; lodret kæde til højre: vederlag - buens start - B - uk-buehøjde - overkant
  (setq xm (+ (* 2.0 r) D (* 4.0 th)))
  (if (> st 0.5)
    (bm:maal (L (+ (* 2.0 r) D) (- st)) (L (+ (* 2.0 r) D) 0.0) (L xm 0.0) (/ pi 2.0)))
  (setq o (bm:maal (L (+ (* 2.0 r) D) 0.0) (L r r) (L xm 0.0) (/ pi 2.0)))
  (if (and o (not (vl-catch-all-error-p o)))
    (vl-catch-all-apply 'vla-put-TextOverride (list o "¼ = <>")))
  (setq o (bm:maal (L r r) (L 0.0 (* 2.0 r)) (L xm 0.0) (/ pi 2.0)))
  (if (and o (not (vl-catch-all-error-p o)))
    (vl-catch-all-apply 'vla-put-TextOverride (list o "¼ = <>")))
  (setq o (bm:maal (L 0.0 (* 2.0 r)) (L 0.0 ok) (L xm 0.0) (/ pi 2.0)))
  (if (and o (not (vl-catch-all-error-p o)))
    (vl-catch-all-apply 'vla-put-TextOverride (list o "Buehøjde <>")))
  ;; samlet: vederlag til overkant
  (bm:maal (L (+ (* 2.0 r) D) (- st)) (L 0.0 ok) (L (+ xm (* 4.0 th)) 0.0) (/ pi 2.0))
  ;; centre og konstruktionslinjer (buens start, centrene A op til knækket,
  ;; topbuens vandrette linje) og LOD
  (bm:centrum (L (- r) 0.0) "A")
  (bm:centrum (L 0.0 r) "B")
  (bm:centrum (L r 0.0) "A")
  (bm:linje (L (* -2.0 r) 0.0) (L (* 2.0 r) 0.0))
  (bm:linje (L (- r) 0.0) (L (- r) r))
  (bm:linje (L r 0.0) (L r r))
  (bm:linje (L (- r) r) (L r r))
  (bm:linje (L 0.0 (- st)) (L 0.0 (+ ok (* 2.0 th))))
  (bm:radius (L 0.0 r) (polar (L 0.0 r) (* 0.25 pi) r))
  ;; låsestenen (venstre) - kort streg ud fra hakket + tekst
  (bm:linje (L (car q) (cadr q)) (L (- (car q) (* 3.0 th)) (+ (cadr q) (* 3.0 th))))
  (bm:tekst (L (- (car q) (* 10.0 th)) (+ (cadr q) (* 3.4 th))) "Låsesten")
  (bm:tekst (L (* -2.0 r) (- (+ st (* 12.0 th))))
            (strcat "Kløverbladsbue: 3 lige store cirkler R = ¼ spændvidde = " (sb:tal r 0)))
)

(defun c:BUEMAAL (/ *error* doc spc th e navn data gnavn)
  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )
  (if (not (setq e (car (entsel "\nVælg en sten i buen (SEGBUE, KURVEBUE eller KLOVERBUE): ")))) (exit))
  (cond
    ((not (setq navn (sb:objekt-gruppe e)))
     (princ "\nObjektet ligger ikke i en bue-gruppe (SEGBUE../KURVEBUE../KLOVERBUE..)."))
    ((not (setq data (sb:data-hent navn)))
     (princ (strcat "\nBuen " navn " er tegnet med en ældre version af BUER.lsp og har ingen gemte mål."
                    "\nTegn buen igen, så kan BUEMAAL bruges.")))
    (T
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
           spc (if (= (getvar "cvport") 1) (vla-get-PaperSpace doc) (vla-get-ModelSpace doc))
           th  (* (getvar "DIMTXT") (if (= 0.0 (getvar "DIMSCALE")) 1.0 (getvar "DIMSCALE")))
           *bm-obj* nil)
     (vla-StartUndoMark doc)
     (cond ((= (car data) "SEG")    (bm:seg (cdr data)))
           ((= (car data) "KLOVER") (bm:klover (cdr data)))
           (T                       (bm:kurve (cdr data))))
     (if *bm-obj*
       (setq gnavn (vl-catch-all-apply 'sb:gruppe (list doc *bm-obj* "BUEMAAL"))))
     (vla-EndUndoMark doc)
     (princ (strcat "\nMål tegnet for " navn " på lag " (getvar "CLAYER")
                    (if (and gnavn (not (vl-catch-all-error-p gnavn)))
                      (strcat " - gruppe " (sb:gruppenavn gnavn)) "")
                    ". Ét U fjerner dem igen."))))
  (princ)
)

;; ============================================================
;; BUESKABELON - bueskive (træskabelon) i 1:1 til udførelsen
;; Tegnes på det NUVÆRENDE lag, et sted du klikker. Kun det der skal
;; bruges for at lave skiven:
;;  - skivens omrids (buens underside + bund) og LOD
;;  - centrene med stråler ud til hvor buen skifter cirkel
;;  - ved hver sten en loddestreg fra bunden lodret op til stenens
;;    venstre hjørne (fugen) + en kort streg ind på skiven i
;;    forlængelse af stenens side, og stenens nummer
;;  - en tabel: for hvert nummer afstanden langs bunden fra venstre
;;    ende (0), afstanden fra forrige streg og højden. Så kan skiven
;;    laves med tommestok og vinkel fra bundlinjen.
;;  - målene: spændvidde, pilhøjde/stik, radier og centrenes placering
;;  - en opskrift "Sådan laves skiven" med alle tal
;; ============================================================

;; Teksthøjde der passer til en skive i 1:1 (th fra c:BUESKABELON)
(defun bf:th (spaend)
  (setq th (min th (max 10.0 (* 0.02 spaend))))
)

;; Mål med egen tekst (<> = det målte tal)
(defun bf:maal (p1 p2 tp rot txt / o)
  (setq o (bm:maal p1 p2 tp rot))
  (if (and txt o (not (vl-catch-all-error-p o)))
    (vl-catch-all-apply 'vla-put-TextOverride (list o txt)))
  o
)

;; Tekst med egen højde (lokalt punkt i WCS)
(defun bf:txt (p tx h)
  (bm:gem (vl-catch-all-apply 'vla-AddText (list spc tx (bm:3d p) h)))
)

;; Stenens VENSTRE hjørne i bunden (lokalt) og retningen ind på skiven
;; langs stenens side: (x y vinkel). Stenen lægges til HØJRE for
;; hjørnet, fugen ligger til VENSTRE (stenene går fra venstre mod højre).
(defun bf:hjoerne (ja jb j wa wb / q)
  (setq q (kb:hjoerner ja jb 0.0 *sb-dybde* j wa wb))
  (list (car (nth 0 q)) (cadr (nth 0 q))
        (angle (list (car (nth 1 q)) (cadr (nth 1 q)))
               (list (car (nth 0 q)) (cadr (nth 0 q)))))
)

;; Loddestreger ved fugerne + tabel.
;; pk = stenenes venstre hjørner (x y vinkel) fra venstre mod højre,
;; lokalt (0,0 = midten af skivens bund). a = halv bundlængde.
;; tx, ty = tabellens øverste venstre hjørne (lokalt).
;; For hver sten: loddestreg fra bunden op til hjørnet, kort streg ind
;; på skiven langs stenens side og stenens nummer for enden af den.
;; Returnerer en tekstlinje til opskriften.
(defun bf:lodfuger (pk a tx ty / tp tl nr x y p e c tn forrige rh maxr bx y1 rk)
  (setq tp (* 0.7 th) tl (* 0.8 th) nr 0)
  (foreach pt pk
    (setq nr (1+ nr) x (car pt) y (cadr pt)
          p  (L x y)
          e  (polar p (caddr pt) tl)
          c  (polar e (caddr pt) (* 0.8 tp))
          tn (itoa nr))
    (bm:linje (L x 0.0) p)                              ; loddestreg op til fugen
    (bm:linje p e)                                      ; streg ind på skiven langs stenens side
    (bf:txt (list (- (car c) (* 0.45 tp (strlen tn))) (- (cadr c) (* 0.5 tp)) (caddr c)) tn tp))
  ;; 0 ved bundens venstre ende (herfra måles)
  (bf:txt (L (- (- a) (* 0.35 tp)) (* -1.5 tp)) "0" tp)
  ;; tabel - højst 25 rækker pr. kolonne
  (setq rh (* 1.5 tp) maxr 25 nr 0 rk 0 forrige (- a) bx tx)
  (bf:txt (L tx ty) "LODDESTREGER VED FUGERNE" th)
  (bf:txt (L tx (- ty (* 1.3 th))) "Mål langs bunden fra venstre ende (0). Sæt vinklen på bunden og afsæt højden." tp)
  (setq y1 (- ty (* 1.3 th) (* 2.0 rh)))
  (foreach pt pk
    (if (= 0 (rem rk maxr))
      (progn
        (if (> rk 0) (setq bx (+ bx (* 15.0 th))))
        (bf:txt (L bx y1) "Nr." tp)
        (bf:txt (L (+ bx (* 2.0 th)) y1) "Fra 0" tp)
        (bf:txt (L (+ bx (* 6.0 th)) y1) "Mellem" tp)
        (bf:txt (L (+ bx (* 10.0 th)) y1) "Højde" tp)))
    (setq nr (1+ nr)
          y  (- y1 (* rh (1+ (rem rk maxr)))))
    (bf:txt (L bx y) (itoa nr) tp)
    (bf:txt (L (+ bx (* 2.0 th)) y) (strcat (sb:tal (+ (car pt) a) 0) " mm") tp)
    (bf:txt (L (+ bx (* 6.0 th)) y) (strcat (sb:tal (- (car pt) forrige) 0) " mm") tp)
    (bf:txt (L (+ bx (* 10.0 th)) y) (strcat (sb:tal (cadr pt) 0) " mm") tp)
    (setq forrige (car pt) rk (1+ rk)))
  (strcat "   Loddestreger: én ved hver sten, lodret fra bunden op til stenens venstre hjørne (fugen)."
          " Mål langs bunden fra venstre ende (0) - se tabellen - sæt en vinkel på bunden og afsæt højden.")
)

;; --- Kurvehanksbue ---
;; vals: mx my mz a b D topf k j n antal-centre, pr. bue: cx cy r a0 a1, stenh
(defun bf:kurve (v / a b D nc geo i n j stenh pts bul navne halv gldybde glstenh
                   fu tl ymin c g ang lodlinje pk)
  (setq a  (nth 3 v) b (nth 4 v) D (nth 5 v)
        j  (nth 8 v) n (fix (+ 0.5 (nth 9 v)))
        nc (fix (+ 0.5 (nth 10 v)))
        stenh (if (> (length v) (+ 11 (* 5 nc))) (nth (+ 11 (* 5 nc)) v) *sb-stenh*)
        i  0)
  (while (< i nc)
    (setq geo (cons (list (nth (+ 11 (* i 5)) v) (nth (+ 12 (* i 5)) v) (nth (+ 13 (* i 5)) v)
                          (nth (+ 14 (* i 5)) v) (nth (+ 15 (* i 5)) v)) geo)
          i (1+ i)))
  (setq geo  (reverse geo)
        ymin (apply 'min (cons 0.0 (mapcar 'cadr geo))))
  (bf:th (* 2.0 a))
  (setq tl      (* 0.8 th)
        gldybde *sb-dybde*
        glstenh *sb-stenh*
        *sb-dybde* D                                  ; kb:fuger bruger buens egne mål
        *sb-stenh* stenh)
  ;; skivens omrids: undersiden af buen + lige bund
  (foreach g geo
    (setq pts (cons (list (+ (car g) (* (nth 2 g) (cos (nth 3 g))))
                          (+ (cadr g) (* (nth 2 g) (sin (nth 3 g))))) pts)
          bul (cons (sb:tan (/ (- (nth 4 g) (nth 3 g)) 4.0)) bul)))
  (setq pts (reverse (cons (list a 0.0) pts))
        bul (reverse (cons 0.0 bul)))
  (setq *bm-obj* (cons (sb:pl (mapcar '(lambda (p) (trans (L (car p) (cadr p)) 0 1)) pts) bul T)
                       *bm-obj*))
  ;; LOD
  (bm:linje (L 0.0 (- ymin (* 1.5 th))) (L 0.0 (+ b (* 2.5 th))))
  (bm:tekst (L (* 0.4 th) (+ b (* 1.5 th))) "LOD")
  ;; stenenes venstre hjørner i bunden (fugerne)
  (setq fu (kb:fuger geo n j a) i 0 pk nil)
  (while (< i n)
    (setq pk (cons (bf:hjoerne (nth i (car fu)) (nth (1+ i) (car fu)) j
                               (nth i (cadr fu)) (nth (1+ i) (cadr fu)))
                   pk)
          i  (1+ i)))
  (setq pk (reverse pk))
  (bm:tekst (L (* -0.35 th) (+ b tl (* 0.3 th))) "S")
  ;; centre og stråler ud til overgangene (ikke langs bunden)
  (setq navne (if (= nc 5) '("A" "C" "B" "C" "A") '("A" "B" "A"))
        halv  (/ nc 2) i 0)
  (foreach g geo
    (setq c (L (car g) (cadr g)))
    (bm:centrum c (nth i navne))
    (foreach ang (list (nth 3 g) (nth 4 g))
      (if (not (and (equal (cadr g) 0.0 0.5)
                    (or (equal ang 0.0 1e-6) (equal ang pi 1e-6))))
        (bm:linje c (polar c ang (nth 2 g)))))
    ;; mål for venstre halvdel og toppen: radius og centrets placering
    (if (<= i halv)
      (progn
        (bm:radius c (polar c (/ (+ (nth 3 g) (nth 4 g)) 2.0) (nth 2 g)))
        (if (> (abs (car g)) 0.5)                     ; vandret fra LOD
          (bm:maal (L 0.0 (cadr g)) c
                   (L 0.0 (if (< (abs (cadr g)) 0.5) (* -6.0 th) (- (cadr g) (* 2.5 th)))) 0.0))
        (if (> (abs (cadr g)) 0.5)                    ; lodret fra bunden
          (bm:maal (L (car g) 0.0) c (L (- (car g) (* 2.5 th)) 0.0) (/ pi 2.0)))))
    (setq i (1+ i)))
  ;; spændvidde og pilhøjde
  (bm:maal (L (- a) 0.0) (L a 0.0) (L 0.0 (* -3.0 th)) 0.0)
  (bf:maal (L 0.0 0.0) (L 0.0 b) (L (- (+ a (* 3.0 th))) 0.0) (/ pi 2.0) "Pil <>")
  ;; loddestreger
  (setq lodlinje (bf:lodfuger pk a (+ a (* 4.0 th)) b))
  (setq *sb-dybde* gldybde *sb-stenh* glstenh)
  (list (strcat (itoa nc) "-centret kurvehanksbue")
        (strcat "Spænd " (sb:tal (* 2.0 a) 0) "  Pil " (sb:tal b 0) "  " (itoa n) " sten")
        (+ b (* 3.0 th))
        (- a)
        (append
          (list (strcat "1. Tegn bunden " (sb:tal (* 2.0 a) 0) " mm lang (spændvidden) og LOD midt på."
                        " Toppen af skiven ligger " (sb:tal b 0) " mm over bunden (pilhøjden).")
                "2. Sæt centrene af (samme mål til begge sider af LOD):")
          (bf:kurve-centre geo navne halv)
          (list (strcat "3. Slå buerne med en snor/lægte fra hvert centrum, fra vederlaget og ind:"
                        (if (= nc 5)
                          " A til strålen fra C gennem A, C til strålen fra B gennem C, og topbuen fra B."
                          " A til strålen fra B gennem A, og topbuen fra B mellem de to stråler."))
                "   Strålerne er tegnet - buerne går glat over i hinanden (ingen knæk)."
                lodlinje
                (strcat "4. Sæt stenene af på kanten: " (itoa n) " sten, slutsten (S) i midten. Stregerne inde på skiven = forlængelsen af stenens VENSTRE side: stenen lægges til HØJRE for sin streg, fugen ligger til VENSTRE for den.")
                (strcat "   "
                        " Stenene er " (sb:tal (nth 5 fu) 0) "-" (sb:tal stenh 0)
                        " mm i bunden, fugen i bunden " (kb:bundtekst (list 0 0 0 0 0 (nth 2 fu) (nth 3 fu)))
                        " mm, fugen i toppen " (sb:tal j 1) " mm.")
                "5. Skær skiven ud, og sæt den på kiler i åbningen, så den kan sænkes, når buen skal afforskalles.")))
)

;; Centrenes placering som tekst (venstre halvdel + toppen)
(defun bf:kurve-centre (geo navne halv / i txt g)
  (setq i 0)
  (foreach g geo
    (if (<= i halv)
      (setq txt (cons (strcat "   " (nth i navne) ": "
                              (if (< (abs (car g)) 0.5) "på LOD" (strcat (sb:tal (abs (car g)) 0) " mm fra LOD"))
                              ", "
                              (cond ((< (abs (cadr g)) 0.5) "på bunden")
                                    ((< (cadr g) 0.0) (strcat (sb:tal (- (cadr g)) 0) " mm under bunden"))
                                    (T (strcat (sb:tal (cadr g) 0) " mm over bunden")))
                              "  -  radius " (sb:tal (nth 2 g) 0) " mm, "
                              (sb:tal (* 180.0 (/ (- (nth 3 g) (nth 4 g)) pi)) 1) " grader")
                      txt)))
    (setq i (1+ i)))
  (reverse txt)
)

;; --- Segmentbue ---
;; vals: cx cy cz ri re a2 a1 topf p1x p1y p1z p2x p2y p2z h k n s D stenh
(defun bf:seg (v / ri a2 a1 h n s stenh half beta tot gam i ang c tl da lodlinje pk)
  (setq ri (nth 3 v) a2 (nth 5 v) a1 (nth 6 v)
        h  (nth 14 v) n (fix (+ 0.5 (nth 16 v))) s (nth 17 v)
        stenh (if (> (length v) 19) (nth 19 v) *sb-stenh*)
        half (/ stenh 2.0)
        beta (* 2.0 (atan half (sqrt (- (* ri ri) (* half half)))))
        tot  (- a1 a2)
        gam  (/ (- tot (* n beta)) (1+ n))
        da   (sb:asin (/ half ri)))
  (bf:th s)
  (setq tl (* 0.8 th)
        c  (L 0.0 (- h ri)))                          ; centrum M
  ;; skivens omrids: undersiden af buen + lige bund
  (setq *bm-obj* (cons (sb:pl (list (trans (L (/ s -2.0) 0.0) 0 1) (trans (L (/ s 2.0) 0.0) 0 1))
                              (list (- (sb:tan (/ tot 4.0))) 0.0) T)
                       *bm-obj*))
  ;; LOD
  (bm:linje (L 0.0 (- (min 0.0 (- h ri)) (* 1.5 th))) (L 0.0 (+ h (* 2.5 th))))
  (bm:tekst (L (* 0.4 th) (+ h (* 1.5 th))) "LOD")
  ;; stenenes venstre hjørner i bunden (lokalt) - stenens sider er
  ;; parallelle med stenens midterlinje, så stregen går ind langs den
  (setq i 0 pk nil)
  (while (< i n)
    (setq ang (+ a2 (* gam (1+ i)) (* beta (+ i 0.5)))           ; stenens midte
          pk  (cons (append (polar (list 0.0 (- h ri)) (+ ang da) ri) (list (+ ang pi))) pk)
          i   (1+ i)))
  (setq pk (reverse pk))
  (bm:tekst (L (* -0.35 th) (+ h tl (* 0.3 th))) "S")
  ;; centrum M med stråler ud til vederlagene
  (bm:centrum c "M")
  (bm:linje c (polar c a1 ri))
  (bm:linje c (polar c a2 ri))
  ;; mål: spændvidde, stik, radius og centrummets placering under bunden
  (bm:maal (L (/ s -2.0) 0.0) (L (/ s 2.0) 0.0) (L 0.0 (* -3.0 th)) 0.0)
  (bf:maal (L 0.0 0.0) (L 0.0 h) (L (- (+ (/ s 2.0) (* 3.0 th))) 0.0) (/ pi 2.0) "Stik <>")
  (bm:radius c (polar c (/ (+ a1 (/ pi 2.0)) 2.0) ri))
  (bm:maal (L 0.0 0.0) c (L (* -2.5 th) 0.0) (/ pi 2.0))
  ;; loddestreger
  (setq lodlinje (bf:lodfuger pk (/ s 2.0) (+ (/ s 2.0) (* 4.0 th)) h))
  (list "Segmentbue"
        (strcat "Spænd " (sb:tal s 0) "  Stik " (sb:tal h 0) "  R " (sb:tal ri 0) "  " (itoa n) " sten")
        (+ h (* 3.0 th))
        (/ s -2.0)
        (list (strcat "1. Tegn bunden " (sb:tal s 0) " mm lang (spændvidden) og LOD midt på."
                      " Toppen af skiven ligger " (sb:tal h 0) " mm over bunden (stikket).")
              (strcat "2. Centrum M ligger på LOD, " (sb:tal (- ri h) 0) " mm under bunden.")
              (strcat "3. Slå buen med en snor/lægte fra M med radius " (sb:tal ri 0) " mm"
                      " (buens vinkel " (sb:tal (* 180.0 (/ tot pi)) 1) " grader).")
              lodlinje
              (strcat "4. Sæt stenene af på kanten: " (itoa n) " sten, slutsten (S) i midten. Stregerne inde på skiven = forlængelsen af stenens VENSTRE side: stenen lægges til HØJRE for sin streg, fugen ligger til VENSTRE for den.")
              (strcat "   Fra venstre vederlag: fuge " (sb:tal (* ri gam) 1) " mm, sten " (sb:tal stenh 0)
                      " mm, fuge " (sb:tal (* ri gam) 1) " mm osv. (målt langs kanten).")
              "5. Skær skiven ud, og sæt den på kiler i åbningen, så den kan sænkes, når buen skal afforskalles."))
)

;; --- Kløverbladsbue ---
;; Som i "Stik og buer": skiven er undersiden af de 3 buer med lodrette
;; ben 1 skifte ned under buens start. Centrene A ligger på buens start
;; 1/4 spændvidde inde fra falsene, B på LOD 1/4 spændvidde over.
;; vals: mx my mz r D topf skifter m jl n ju stenh [skifte stående]
(defun bf:klover (v / r D mm jl n ju stenh sk ben gldybde glstenh g cl cr K ray tl pa pd
                    xm hh lodlinje pq pk pe)
  (setq r     (nth 3 v) D (nth 4 v)
        mm    (fix (+ 0.5 (nth 7 v))) jl (nth 8 v)
        n     (fix (+ 0.5 (nth 9 v))) ju (nth 10 v)
        stenh (nth 11 v)
        sk    (if (> (length v) 12) (nth 12 v) *sb-skifte*)
        ben   sk                                      ; skivens ben: 1 skifte under buens start
        gldybde *sb-dybde*
        glstenh *sb-stenh*
        *sb-dybde* D                                  ; stenene regnes med buens egne mål
        *sb-stenh* stenh
        g     (kl:opbyg r mm jl n ju)
        cl    (list (- r) 0.0)
        cr    (list r 0.0))
  (bf:th (* 4.0 r))
  (setq tl (* 0.8 th))
  ;; lokale punkter: (0, 0) = buens start på LOD (midt mellem centrene A)
  (defun K (x y) (L x (+ y ben)))
  (defun ray (p q) (bm:linje (K (car p) (cadr p)) (K (car q) (cadr q))))

  ;; skiven: ben - sidebue - topbue - sidebue - ben - bund
  (setq *bm-obj* (cons (sb:pl (mapcar '(lambda (p) (trans (K (car p) (cadr p)) 0 1))
                                      (list (list (* -2.0 r) (- ben)) (list (* -2.0 r) 0.0)
                                            (list (- r) r) (list r r)
                                            (list (* 2.0 r) 0.0) (list (* 2.0 r) (- ben))))
                              (list 0.0 (sb:tan (/ pi -8.0)) -1.0 (sb:tan (/ pi -8.0)) 0.0 0.0) T)
                       *bm-obj*))
  ;; konstruktion: buens start gennem centrene A, A op til knækkene,
  ;; vandret linje gennem B mellem knækkene, og LOD
  (ray (list (* -2.0 r) 0.0) (list (* 2.0 r) 0.0))
  (ray cl (list (- r) r))
  (ray cr (list r r))
  (ray (list (- r) r) (list r r))
  (bm:linje (K 0.0 (- (+ ben (* 1.5 th)))) (K 0.0 (+ (* 2.0 r) (* 2.5 th))))
  (bm:tekst (K (* 0.4 th) (+ (* 2.0 r) (* 1.5 th))) "LOD")
  (bm:centrum (K (- r) 0.0) "A")
  (bm:centrum (K 0.0 r) "B")
  (bm:centrum (K r 0.0) "A")

  ;; stenenes venstre hjørner i bunden (også låsestenene), lokalt med benene
  (foreach sp (nth 6 g)
    (if (eq (car sp) 'laas)
      (progn
        ;; låsestenens venstre kant: på sidebuen (venstre knæk) eller på
        ;; topbuen (højre knæk). Siden går fra hjørnet ud til hakket.
        (setq pa (car (nth 0 (cadr sp)))
              pd (car (nth 2 (cadr sp)))
              pq (car (nth 1 (cadr sp)))
              pe (if (< (car pa) (car pd)) pa pd)
              pk (cons (list (car pe) (+ (cadr pe) ben) (angle pq pe)) pk)))
      (setq pk (cons (bf:hjoerne (kl:hojt (nth 1 sp) ben) (kl:hojt (nth 2 sp) ben)
                                 (nth 3 sp) (nth 4 sp) (nth 5 sp))
                     pk))))
  (setq pk (reverse pk))
  (bm:tekst (K (* -0.35 th) (+ (* 2.0 r) tl (* 0.3 th))) "S")
  (bm:tekst (K (- (+ r (* 1.2 th))) (+ r (* 0.3 th))) "L")
  (bm:tekst (K (+ r (* 0.4 th)) (+ r (* 0.3 th))) "L")

  ;; mål: 4 x 1/4 spændvidde (placerer centrene A) + spændvidden,
  ;; lodret til venstre: skivens ben, 1/4 (op til B og knækket), 1/4 (op til toppen)
  (setq xm (list (* -2.0 r) (- r) 0.0 r (* 2.0 r)))
  (while (cdr xm)
    (bf:maal (K (car xm) 0.0) (K (cadr xm) 0.0) (K 0.0 (- (+ ben (* 3.0 th)))) 0.0 "¼ = <>")
    (setq xm (cdr xm)))
  (bm:maal (K (* -2.0 r) (- ben)) (K (* 2.0 r) (- ben)) (K 0.0 (- (+ ben (* 6.0 th)))) 0.0)
  (setq hh (- (+ (* 2.0 r) (* 3.0 th))))
  (bf:maal (K (* -2.0 r) (- ben)) (K (* -2.0 r) 0.0) (K hh 0.0) (/ pi 2.0) "Ben <>")
  (bf:maal (K (* -2.0 r) 0.0) (K (- r) r) (K hh 0.0) (/ pi 2.0) "¼ = <>")
  (bf:maal (K (- r) r) (K 0.0 (* 2.0 r)) (K hh 0.0) (/ pi 2.0) "¼ = <>")
  ;; loddestreger (højden måles fra skivens bund, dvs. med benene)
  (setq lodlinje (bf:lodfuger pk (* 2.0 r) (+ (* 2.0 r) (* 4.0 th)) (+ ben (* 2.0 r))))

  (setq *sb-dybde* gldybde *sb-stenh* glstenh)
  (list "Kløverbladsbue"
        (strcat "Spænd " (sb:tal (* 4.0 r) 0) "  R = ¼ spænd = " (sb:tal r 0) "  "
                (itoa (kl:antal-sten g)) " sten (L = låsesten)")
        (+ ben (* 2.0 r) (* 3.0 th))
        (* -2.0 r)
        (list (strcat "1. Tegn bunden " (sb:tal (* 4.0 r) 0) " mm lang (spændvidden fals til fals) og LOD midt på."
                      " Del den i 4 lige dele: ¼ = R = " (sb:tal r 0) " mm.")
              (strcat "2. Tegn benene lodret op " (sb:tal ben 0) " mm (1 skifte). Her starter buen:"
                      " tegn en vandret linje - centrene A ligger på den, " (sb:tal r 0)
                      " mm inde fra hver side.")
              (strcat "3. Centrum B ligger på LOD, " (sb:tal r 0) " mm over buens start."
                      " Knækkene ligger lodret over A i samme højde som B.")
              (strcat "4. Slå sidebuerne fra A med radius " (sb:tal r 0) " mm fra buens start op til knækket,"
                      " og topbuen fra B med radius " (sb:tal r 0) " mm mellem de to knæk.")
              (strcat "   Toppen af skiven ligger " (sb:tal (+ ben (* 2.0 r)) 0) " mm over bunden.")
              lodlinje
              (strcat "5. Sæt stenene af på kanten: " (itoa mm) " sten i hver sidebue, " (itoa n)
                      " i topbuen (slutsten S) og en låsesten (L) i hvert knæk.")
              "   Stregerne inde på skiven = forlængelsen af stenens VENSTRE side: stenen lægges til HØJRE for sin streg, fugen ligger til VENSTRE for den."
              (strcat "   Stenene er " (sb:tal (nth 10 g) 0) "-" (sb:tal stenh 0) " mm i bunden, fugen i bunden "
                      (kl:fugebund g) " mm, fugen i toppen " (sb:tal jl 1) " mm (sider) / "
                      (sb:tal ju 1) " mm (top).")
              (strcat "6. Låsestenen: hjørnet i hakket, siderne " (sb:tal D 0) " mm langs strålen fra A og strålen fra B"
                      " (specialsten " (sb:tal D 0) " x " (sb:tal D 0) " x 228), tilhugges efter begge buer.")
              "7. Skær skiven ud, og sæt den på kiler i åbningen. Mur op fra begge sider, så skiven ikke forskubbes."))
)

;; Fuge (cx cy r vinkel) flyttet op med dy (kløverbladsbuens ben)
(defun kl:hojt (jp dy)
  (list (car jp) (+ (cadr jp) dy) (nth 2 jp) (nth 3 jp))
)

(defun c:BUESKABELON (/ *error* doc spc th e navn data bp info gnavn L y linjer o)
  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nFejl: " msg)))
    (princ)
  )
  (if (not (setq e (car (entsel "\nVælg en sten i buen (SEGBUE, KURVEBUE eller KLOVERBUE): ")))) (exit))
  (cond
    ((not (setq navn (sb:objekt-gruppe e)))
     (princ "\nObjektet ligger ikke i en bue-gruppe (SEGBUE../KURVEBUE../KLOVERBUE..)."))
    ((not (setq data (sb:data-hent navn)))
     (princ (strcat "\nBuen " navn " er tegnet med en ældre version af BUER.lsp."
                    "\nTegn buen igen, så kan BUESKABELON bruges.")))
    (T
     (if (not (setq bp (getpoint "\nKlik hvor MIDTEN af skivens bund skal ligge (et frit sted, skiven tegnes i 1:1): ")))
       (exit))
     (setq bp  (trans bp 1 0)
           doc (vla-get-ActiveDocument (vlax-get-acad-object))
           spc (if (= (getvar "cvport") 1) (vla-get-PaperSpace doc) (vla-get-ModelSpace doc))
           th  (* (getvar "DIMTXT") (if (= 0.0 (getvar "DIMSCALE")) 1.0 (getvar "DIMSCALE")))
           *bm-obj* nil)
     ;; lokale punkter (0,0 = midten af skivens bund) -> WCS
     (defun L (x y) (list (+ (car bp) x) (+ (cadr bp) y) (caddr bp)))
     (vla-StartUndoMark doc)
     ;; info = (buetype mål-linje top venstre-kant opskrift)
     (setq info (cond ((= (car data) "SEG")    (bf:seg (cdr data)))
                      ((= (car data) "KLOVER") (bf:klover (cdr data)))
                      (T                       (bf:kurve (cdr data)))))
     ;; overskrift + opskrift over skiven
     (setq linjer (append (list (strcat "BUESKIVE 1:1 - " navn)
                                (strcat (car info) ".  " (cadr info))
                                ""
                                "SÅDAN LAVES SKIVEN:")
                          (nth 4 info))
           y      (+ (nth 2 info) (* 1.5 th) (* 1.6 th (length linjer))))
     (foreach t1 linjer
       (if (/= t1 "") (bm:tekst (L (nth 3 info) y) t1))
       (setq y (- y (* 1.6 th))))
     ;; målene får samme (mindre) tekst som resten af skiven
     (foreach e *bm-obj*
       (setq o (vlax-ename->vla-object e))
       (if (wcmatch (vla-get-ObjectName o) "*Dimension*")
         (progn
           (vl-catch-all-apply 'vla-put-TextHeight (list o th))
           (vl-catch-all-apply 'vla-put-ArrowheadSize (list o (* 0.6 th))))))
     (if *bm-obj*
       (setq gnavn (vl-catch-all-apply 'sb:gruppe (list doc *bm-obj* "BUESKABELON"))))
     (vla-EndUndoMark doc)
     (princ (strcat "\nBueskive tegnet i 1:1 for " navn " på lag " (getvar "CLAYER")
                    (if (and gnavn (not (vl-catch-all-error-p gnavn)))
                      (strcat " - gruppe " (sb:gruppenavn gnavn)) "")
                    ". Ét U fjerner den igen."))))
  (princ)
)

;; ============================================================
;; MURRUND - rundt murværk set oppefra (hel cirkel eller bue), ét skifte
;; Kort: MRU. Kan også startes fra menuen BUER.
;;
;; En dialog (som ved buerne), hvor man vælger:
;;  - Kopper (108 x 54 set oppefra, lang side ind mod centrum - blød runding)
;;    eller Løbere (228 x 108 set oppefra, lang side langs muren) + stenens mål
;;  - radius til ydersiden (snoren) eller indersiden - skriv eller klik
;;  - hel cirkel eller bue (vinkel)
;;  - mindste fuge inderst og største fuge yderst (standard 7 og 20 mm,
;;    samme regel som ved buer og stik)
;; Listen viser mulighederne (antal sten, fuger, om stenene skal hugges
;; kileformede) med et forslag. Billederne viser hele muren og et
;; forstørret udsnit af fugerne, så man kan se forskel.
;;
;; Stenene lægges med de yderste hjørner på cirklen (der hvor skabelonen
;; ligger an) og indersiden tangent til den indre cirkel. Stenene har fuld
;; bredde yderst. Bliver fugen yderst for bred med hele sten, hugges
;; stenene kileformede inderst, så fugen inderst bliver den mindste fuge.
;; Stenene tegnes i laget Mursten og samles i en gruppe (MURRUND1 ...).
;; Målene sættes på med FM fra MAAL.lsp (indlæses automatisk).
;; ============================================================

;; Valgene huskes, mens AutoCAD er åben
(if (not *mr-typ*)    (setq *mr-typ*    "kopper"))   ; "kopper" / "løbere"
;; Stenens mål set oppefra (lang kort) - kopper og løbere har hver deres
(setq *mr-standardmaal* '(("kopper" 108.0 54.0) ("løbere" 228.0 108.0)))
(if (not *mr-stenmaal*) (setq *mr-stenmaal* *mr-standardmaal*))
(if (not *mr-radius*) (setq *mr-radius* 1500.0))
(if (not *mr-side*)   (setq *mr-side*   "Y"))        ; Y = radius til ydersiden, I = indersiden
(if (not *mr-form*)   (setq *mr-form*   "hel"))      ; "hel" / "bue"
(if (not *mr-vinkel*) (setq *mr-vinkel* 90.0))       ; buens vinkel i grader
;; Fugerne deles med FM i MAAL.lsp (den skriver OBS, hvis de ikke holder)
(if (not *rund-fugemin*)  (setq *rund-fugemin*  7.0))    ; mindste fuge inderst
(if (not *rund-fugemaks*) (setq *rund-fugemaks* 20.0))   ; største fuge yderst
(setq *mr-fugenormal* 12.0)                             ; almindelig studsfuge (forslag yderst)

;; ---------- Små hjælpere ----------
;; Punktet brøkdelen f fra p mod q
(defun mr:mellem (p q f)
  (list (+ (car p) (* f (- (car q) (car p)))) (+ (cadr p) (* f (- (cadr q) (cadr p)))))
)
(defun mr:hel () (= *mr-form* "hel"))
;; Vinklen muren går over (radianer)
(defun mr:vv () (if (mr:hel) (* 2.0 pi) (* pi (/ *mr-vinkel* 180.0))))
;; Stenens mål (lang kort) for en stentype ("kopper" / "løbere")
(defun mr:stenmaal (typ) (cdr (assoc typ *mr-stenmaal*)))
(defun mr:gem-stenmaal (typ lang kort)
  (setq *mr-stenmaal* (cons (list typ lang kort)
                            (vl-remove-if '(lambda (x) (= (car x) typ)) *mr-stenmaal*)))
)

(defun mr:murlag ()
  (if (not (tblsearch "LAYER" "Mursten"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") '(2 . "Mursten")
                   '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
)

;; ---------- Beregning ----------
;; Fugen mellem to sten i afstanden rad fra centrum.
;; w = stenens bredde langs muren, tv = vinklen fra sten til sten.
(defun mr:fuge (rad w tv) (- (* 2.0 rad (sin (/ tv 2.0))) (* w (cos (/ tv 2.0)))))

;; Vinklen fra sten til sten med n sten (bue: yderste hjørner følger buens ender)
(defun mr:deling (n hel vv psi)
  (cond (hel (/ (* 2.0 pi) n))
        ((> n 1) (/ (- vv (* 2.0 psi)) (1- n)))
        (T 0.0))
)

;; Flest mulige sten, så fugen i afstanden rad er mindst den mindste fuge
(defun mr:antal (rad w hel vv psi / t0 n)
  (setq t0 (* 2.0 (atan (* 0.5 w) rad))          ; ved denne vinkel er fugen 0
        n  (if hel
             (fix (/ (* 2.0 pi) t0))
             (1+ (fix (/ (max 0.0 (- vv (* 2.0 psi))) t0)))))
  (while (and (> n (if hel 3 2))
              (< (mr:fuge rad w (mr:deling n hel vv psi)) *rund-fugemin*))
    (setq n (1- n)))
  n
)

;; Murens mål ud fra valgene: (w d ri a rr psi), nil hvis radius er for lille.
;; w = stenens bredde langs muren (yderst), d = murens tykkelse,
;; ri = radius til indersiden, a = til midt på ydersiden,
;; rr = til de yderste hjørner (snoren), psi = halv stenbredde yderst som vinkel.
(defun mr:geo (/ sm w d a ri)
  (setq sm (mr:stenmaal *mr-typ*))
  (if (= *mr-typ* "kopper") (setq w (cadr sm) d (car sm)) (setq w (car sm) d (cadr sm)))
  (if (= *mr-side* "I")
    (setq ri *mr-radius* a (+ *mr-radius* d))
    (setq a  (sqrt (max 0.0 (- (* *mr-radius* *mr-radius*) (* 0.25 w w))))
          ri (- a d)))
  (if (> ri (* 0.5 w))
    (list w d ri a (sqrt (+ (* a a) (* 0.25 w w))) (atan (* 0.5 w) a)))
)

;; Løsning med n sten: (n deling fuge-inderst fuge-yderst bredde-inderst ok)
;; Bliver fugen inderst under mindste fuge, hugges stenene kileformede inderst,
;; så fugen inderst bliver mindste fuge. ok = fugen yderst holder reglerne.
(defun mr:loesning (n g / w ri a tv fo fi wi)
  (setq w  (nth 0 g) ri (nth 2 g) a (nth 3 g)
        tv (mr:deling n (mr:hel) (mr:vv) (nth 5 g))
        fo (mr:fuge a w tv)
        fi (mr:fuge ri w tv)
        wi w)
  (if (< fi *rund-fugemin*)
    (setq wi (/ (- (* 2.0 ri (sin (/ tv 2.0))) *rund-fugemin*) (cos (/ tv 2.0)))
          fi *rund-fugemin*))
  (list n tv fi fo wi (and (>= fo (- *rund-fugemin* 0.05)) (<= fo (+ *rund-fugemaks* 0.05))))
)

;; Skal stenene i løsningen x hugges?
(defun mr:kile-p (x g) (< (nth 4 x) (- (nth 0 g) 0.5)))

;; Er fugen f1 yderst bedre end f2? Helst mindst en almindelig fuge og så smal som muligt.
(defun mr:bedre (f1 f2)
  (if (>= f2 (- *mr-fugenormal* 1e-6))
    (and (>= f1 (- *mr-fugenormal* 1e-6)) (< f1 f2))
    (> f1 f2))
)

;; Mulige løsninger, flest sten først: de 3 med flest hele sten, og huggede
;; sten der holder reglerne (højst hugget ned til halv bredde, højst 7 stk.).
;; Kan hele sten holde reglerne, vises kun huggede sten med en almindelig fuge
;; yderst - ellers er der ingen grund til at hugge.
;; Returnerer (løsninger nummer-på-forslaget) eller nil.
(defun mr:kandidater (g / w ri a psi hel vv nmin nhel n x alle helok lst kileok i hi ki mindst ms s)
  (setq w    (nth 0 g) ri (nth 2 g) a (nth 3 g) psi (nth 5 g)
        hel  (mr:hel) vv (mr:vv)
        nmin (if hel 3 2)
        nhel (mr:antal ri w hel vv psi)           ; flest hele sten (mindste fuge inderst)
        n    (mr:antal a w hel vv psi))           ; flest sten i alt (mindste fuge yderst)
  (while (>= n (max nmin (- nhel 2)))
    (setq x (mr:loesning n g))
    (if (or (not (mr:kile-p x g))
            (and (nth 5 x) (>= (nth 4 x) (* 0.5 w))))
      (setq alle (cons x alle)))
    (setq n (1- n)))
  (setq alle  (reverse alle)
        helok (vl-some '(lambda (x) (and (nth 5 x) (not (mr:kile-p x g)))) alle))
  (foreach x alle
    (if (or (not (mr:kile-p x g)) (not helok) (>= (nth 3 x) (- *mr-fugenormal* 1e-6)))
      (setq lst (cons x lst))))
  (setq lst (reverse lst))
  ;; bedste huggede: fugen yderst tættest på en almindelig fuge
  (foreach x lst
    (if (and (nth 5 x) (mr:kile-p x g)
             (or (not kileok) (mr:bedre (nth 3 x) (nth 3 kileok))))
      (setq kileok x)))
  (if kileok
    (setq lst (vl-remove-if '(lambda (x) (and (mr:kile-p x g) (> (abs (- (car x) (car kileok))) 3)))
                            lst)))
  ;; forslag: hele sten der holder reglerne (flest sten), ellers den bedste
  ;; huggede, ellers den der bryder reglerne mindst
  (setq i 0)
  (foreach x lst
    (cond
      ((and (nth 5 x) (not (mr:kile-p x g))) (if (not hi) (setq hi i)))
      ((eq x kileok) (setq ki i)))
    (setq s (+ (max 0.0 (- (nth 3 x) *rund-fugemaks*)) (max 0.0 (- *rund-fugemin* (nth 3 x)))))
    (if (or (not mindst) (< s ms)) (setq mindst i ms s))
    (setq i (1+ i)))
  (if lst (list lst (cond (hi) (ki) (mindst))))
)

;; Linjer i listen
(defun mr:overskrift ()
  (strcat (kb:pad "Sten" 6) (kb:pad "Stenene" 13) (kb:pad "Fuge inderst" 14)
          (kb:pad "Fuge yderst" 13) (kb:pad "Hugges til" 12) "Regler")
)
(defun mr:linje (x g forslag)
  (strcat (kb:pad (itoa (nth 0 x)) 6)
          (kb:pad (if (mr:kile-p x g) "kileformede" "hele") 13)
          (kb:pad (strcat (sb:tal (nth 2 x) 1) " mm") 14)
          (kb:pad (strcat (sb:tal (nth 3 x) 1) " mm") 13)
          (kb:pad (if (mr:kile-p x g) (strcat (sb:tal (nth 4 x) 0) " mm") "-") 12)
          (kb:pad (if (nth 5 x) "OK" "IKKE OK") 9)
          (if forslag "<- forslag" ""))
)

;; Løsningen x beskrevet i 4 linjer (dialogen og kommandolinjen)
(defun mr:tekster (x g / w d ri rr tv fi fo wi)
  (setq w  (nth 0 g) d (nth 1 g) ri (nth 2 g) rr (nth 4 g)
        tv (nth 1 x) fi (nth 2 x) fo (nth 3 x) wi (nth 4 x))
  (list
    (strcat (itoa (nth 0 x)) " " *mr-typ* " pr. skifte - "
            (if (mr:kile-p x g)
              (strcat "hugges kileformede til " (sb:tal wi 0) " mm inderst ("
                      (sb:tal (- w wi) 0) " mm af hver sten)")
              "hele sten, ingen skal hugges"))
    (strcat "Fuge inderst " (sb:tal fi 1) " mm (mindst " (sb:tal-kort *rund-fugemin*) ") "
            (if (>= fi (- *rund-fugemin* 0.05)) "OK" "IKKE OK")
            "     Fuge yderst " (sb:tal fo 1) " mm (højst " (sb:tal-kort *rund-fugemaks*) ") "
            (if (<= fo (+ *rund-fugemaks* 0.05)) "OK" "IKKE OK"))
    (strcat "Radius " (sb:tal rr 0) " mm til ydersiden (snoren) og " (sb:tal ri 0)
            " mm til indersiden - murtykkelse " (sb:tal d 0) " mm")
    (strcat (if (mr:hel)
              (strcat "Omkreds udv. 2 x π x " (sb:tal rr 0) " = " (sb:tal (* 2.0 pi rr) 0) " mm")
              (strcat "Buelængde udv. 2 x π x " (sb:tal rr 0) " x " (sb:tal-kort *mr-vinkel*)
                      " / 360 = " (sb:tal (* rr (mr:vv)) 0) " mm"))
            " - næste skifte drejes en halv sten (" (sb:tal (/ (* tv 90.0) pi) 1) " grader)"))
)

;; Stenens 4 hjørner om centrum c. Stenens akse peger i retningen phi.
;; Rækkefølge: inderst og yderst på den ene side, yderst og inderst på den anden.
(defun mr:hjoerner (c phi ri a w wi / b1 b2 v)
  (setq b1 (polar c phi ri)
        b2 (polar c phi a)
        v  (- phi (/ pi 2.0)))
  (list (polar b1 v (* 0.5 wi)) (polar b2 v (* 0.5 w))
        (polar b2 (+ v pi) (* 0.5 w)) (polar b1 (+ v pi) (* 0.5 wi)))
)

;; Mål med FM fra MAAL.lsp (indlæses, hvis den ikke er det)
(defun mr:maal (ss / fn)
  (if (not fm:koer)
    (if (setq fn (cond ((and *buer-mappe* (findfile (strcat *buer-mappe* "\\MAAL.lsp"))))
                       ((findfile "MAAL.lsp"))
                       ((findfile "C:/Murer_Toolpalettes_Autocad/MAAL.lsp"))))
      (vl-catch-all-apply 'load (list fn))))
  (if fm:koer
    (fm:koer ss nil)
    (princ "\nFandt ikke MAAL.lsp - indlæs den og kør FM på stenene for at få mål."))
)

;; ---------- Billeder i dialogen ----------
;; Streg fra p til q (mm) i det billede der er åbent, klippet ved kanten.
;; Bruger bw bh sk ox oy fra den funktion der tegner billedet.
(defun mr:lin (p q c / x1 y1 dx dy t0 t1 ok r)
  (setq x1 (+ ox (* (car p) sk))
        y1 (- oy (* (cadr p) sk))
        dx (- (+ ox (* (car q) sk)) x1)
        dy (- (- oy (* (cadr q) sk)) y1)
        t0 0.0 t1 1.0 ok T)
  (foreach pq (list (list (- dx) x1) (list dx (- bw 1.0 x1))
                    (list (- dy) y1) (list dy (- bh 1.0 y1)))
    (if ok
      (if (equal (car pq) 0.0 1e-9)
        (if (< (cadr pq) 0.0) (setq ok nil))
        (progn
          (setq r (/ (cadr pq) (car pq)))
          (if (< (car pq) 0.0)
            (if (> r t1) (setq ok nil) (setq t0 (max t0 r)))
            (if (< r t0) (setq ok nil) (setq t1 (min t1 r))))))))
  (if ok
    (vector_image (fix (+ x1 (* t0 dx) 0.5)) (fix (+ y1 (* t0 dy) 0.5))
                  (fix (+ x1 (* t1 dx) 0.5)) (fix (+ y1 (* t1 dy) 0.5)) c))
)

(defun mr:omrids (q c)
  (mr:lin (nth 0 q) (nth 1 q) c) (mr:lin (nth 1 q) (nth 2 q) c)
  (mr:lin (nth 2 q) (nth 3 q) c) (mr:lin (nth 3 q) (nth 0 q) c)
)

;; Fyldt sten: tætte streger fra indersiden ud til ydersiden
(defun mr:fyld (q c / m s)
  (setq m (+ 2 (fix (* 2.5 sk (max (distance (nth 0 q) (nth 3 q)) (distance (nth 1 q) (nth 2 q))))))
        s 0)
  (while (<= s m)
    (mr:lin (mr:mellem (nth 0 q) (nth 3 q) (/ (float s) m))
            (mr:mellem (nth 1 q) (nth 2 q) (/ (float s) m)) c)
    (setq s (1+ s)))
)

;; Tyk streg (3 pixels)
(defun mr:tyk (p q c / e)
  (setq e (/ 1.0 sk))
  (foreach dy (list (- e) 0.0 e)
    (mr:lin (list (car p) (+ (cadr p) dy)) (list (car q) (+ (cadr q) dy)) c))
)

;; Cirkelbue med radius rad om (0,0) fra vinkel v1 til v2
(defun mr:cirkelbue (rad v1 v2 c / k)
  (setq k 0)
  (repeat 48
    (mr:lin (polar '(0.0 0.0) (+ v1 (* k (/ (- v2 v1) 48.0))) rad)
            (polar '(0.0 0.0) (+ v1 (* (1+ k) (/ (- v2 v1) 48.0))) rad) c)
    (setq k (1+ k)))
)

(defun mr:tomme-billeder ()
  (foreach k '("billede" "udsnit")
    (start_image k)
    (fill_image 0 0 (dimx_tile k) (dimy_tile k) 250)
    (end_image))
)

;; Hele muren: stenene (hvide - orange hvis de skal hugges) og centrum (blåt +)
(defun mr:billede-hele (x / bw bh sk ox oy w ri a rr n tv wi vv v0 a0 pts xs ys k farve)
  (setq bw (dimx_tile "billede") bh (dimy_tile "billede")
        w  (nth 0 *mr-g*) ri (nth 2 *mr-g*) a (nth 3 *mr-g*) rr (nth 4 *mr-g*)
        n  (nth 0 x) tv (nth 1 x) wi (nth 4 x)
        vv (mr:vv)
        v0 (- (/ pi 2.0) (/ vv 2.0))                    ; buen vises symmetrisk om lodret
        a0 (if (mr:hel) (/ pi 2.0) (+ v0 (nth 5 *mr-g*)))
        farve (if (mr:kile-p x *mr-g*) 30 7)
        pts (if (or (mr:hel) (>= vv (/ pi 2.0))) (list '(0.0 0.0)))
        k  0)
  (repeat 37
    (setq pts (cons (polar '(0.0 0.0) (+ v0 (* k (/ vv 36.0))) rr)
                    (cons (polar '(0.0 0.0) (+ v0 (* k (/ vv 36.0))) ri) pts))
          k   (1+ k)))
  (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts)
        sk (min (/ (- bw 12.0) (max 1.0 (- (apply 'max xs) (apply 'min xs))))
                (/ (- bh 12.0) (max 1.0 (- (apply 'max ys) (apply 'min ys)))))
        ox (- (/ bw 2.0) (* sk (/ (+ (apply 'max xs) (apply 'min xs)) 2.0)))
        oy (+ (/ bh 2.0) (* sk (/ (+ (apply 'max ys) (apply 'min ys)) 2.0))))
  (start_image "billede")
  (fill_image 0 0 bw bh 250)
  (setq k (/ 6.0 sk))
  (mr:lin (list (- k) 0.0) (list k 0.0) 5)
  (mr:lin (list 0.0 (- k)) (list 0.0 k) 5)
  (setq k 0)
  (repeat n
    (mr:omrids (mr:hjoerner '(0.0 0.0) (+ a0 (* k tv)) ri a w wi) farve)
    (setq k (1+ k)))
  (end_image)
)

;; Udsnit: den midterste sten og halvdelen af nabostenene, forstørret, så man
;; kan se fugerne. Grøn/rød streg over fugen inderst og yderst.
(defun mr:billede-udsnit (x / bw bh sk ox oy w d ri a rr tv fi fo wi ymin ymax qs q1 q2 farve)
  (setq bw (dimx_tile "udsnit") bh (dimy_tile "udsnit")
        w  (nth 0 *mr-g*) d (nth 1 *mr-g*) ri (nth 2 *mr-g*) a (nth 3 *mr-g*) rr (nth 4 *mr-g*)
        tv (nth 1 x) fi (nth 2 x) fo (nth 3 x) wi (nth 4 x)
        ymin (- ri (* 0.15 d))
        ymax (+ rr (* 0.15 d))
        sk (min (/ (- bw 8.0) (* 2.0 (+ (* 0.95 w) (max fi fo))))
                (/ (- bh 8.0) (- ymax ymin)))
        ox (/ bw 2.0)
        oy (+ (/ bh 2.0) (* sk (/ (+ ymin ymax) 2.0)))
        farve (if (mr:kile-p x *mr-g*) 30 7))
  (start_image "udsnit")
  (fill_image 0 0 bw bh 250)
  ;; cirklerne inderst og yderst (snoren) i gråt
  (mr:cirkelbue ri (- (/ pi 2.0) (* 2.5 tv)) (+ (/ pi 2.0) (* 2.5 tv)) 8)
  (mr:cirkelbue rr (- (/ pi 2.0) (* 2.5 tv)) (+ (/ pi 2.0) (* 2.5 tv)) 8)
  ;; 3 sten med den midterste lodret. Huggede sten: den hele sten i gråt bagved.
  (foreach phi (list (- (/ pi 2.0) tv) (/ pi 2.0) (+ (/ pi 2.0) tv))
    (if (mr:kile-p x *mr-g*)
      (mr:omrids (mr:hjoerner '(0.0 0.0) phi ri a w w) 8))
    (setq q1 (mr:hjoerner '(0.0 0.0) phi ri a w wi)
          qs (cons q1 qs))
    (mr:fyld q1 farve))
  (setq qs (reverse qs))
  ;; fugerne: grøn = holder reglen, rød = holder ikke
  (foreach par (list (list (nth 0 qs) (nth 1 qs)) (list (nth 1 qs) (nth 2 qs)))
    (setq q1 (car par) q2 (cadr par))
    (mr:tyk (nth 3 q1) (nth 0 q2) (if (>= fi (- *rund-fugemin* 0.05)) 3 1))
    (mr:tyk (nth 2 q1) (nth 1 q2) (if (<= fo (+ *rund-fugemaks* 0.05)) 3 1)))
  (end_image)
)

;; ---------- Dialogen ----------
;; Ingen æ, ø og å i DCL-filen - de tekster sættes fra LISP.
(defun mr:dcl-fil ()
  (sb:dcl-skriv "murrund"
    '("mr_rund : dialog {"
      "  label = \"Rund mur (MURRUND)\";"
      "  : row {"
      "    : column {"
      "      : boxed_column { label = \"Sten\";"
      "        : popup_list { key = \"sten\"; width = 48; }"
      "        : row { : text { key = \"t_lang\"; width = 32; } : edit_box { key = \"lang\"; edit_width = 8; } }"
      "        : row { : text { key = \"t_kort\"; width = 32; } : edit_box { key = \"kort\"; edit_width = 8; } }"
      "      }"
      "      : boxed_column { label = \"Radius\";"
      "        : popup_list { key = \"side\"; width = 48; }"
      "        : row { : text { key = \"t_radius\"; width = 32; } : edit_box { key = \"radius\"; edit_width = 8; } }"
      "        : button { key = \"klik\"; label = \"Klik centrum og radius i tegningen <\"; }"
      "      }"
      "      : boxed_column { label = \"Form\";"
      "        : popup_list { key = \"form\"; width = 48; }"
      "        : row { : text { key = \"t_vinkel\"; width = 32; } : edit_box { key = \"vinkel\"; edit_width = 8; } }"
      "      }"
      "      : boxed_column { label = \"Fuger\";"
      "        : row { : text { key = \"t_fmin\"; width = 32; } : edit_box { key = \"fmin\"; edit_width = 8; } }"
      "        : row { : text { key = \"t_fmax\"; width = 32; } : edit_box { key = \"fmax\"; edit_width = 8; } }"
      "        : text { key = \"t_fuger\"; width = 48; }"
      "      }"
      "    }"
      "    : column {"
      "      : boxed_column { label = \"Muligheder (klik for at se dem)\";"
      "        : list_box { key = \"liste\"; height = 10; width = 82; fixed_width_font = true; }"
      "      }"
      "      : row {"
      "        : boxed_column { label = \"Hele muren\";"
      "          : image { key = \"billede\"; width = 38; height = 15; color = 250; }"
      "        }"
      "        : boxed_column { label = \"Udsnit af fugerne\";"
      "          : image { key = \"udsnit\"; width = 38; height = 15; color = 250; }"
      "        }"
      "      }"
      "      : text { key = \"legende\"; width = 82; }"
      "      : text { key = \"legende2\"; width = 82; }"
      "    }"
      "  }"
      "  : boxed_column { label = \"Valgt mur\";"
      "    : text { key = \"v1\"; width = 132; }"
      "    : text { key = \"v2\"; width = 132; }"
      "    : text { key = \"v3\"; width = 132; }"
      "    : text { key = \"v4\"; width = 132; }"
      "  }"
      "  : text { key = \"advarsel\"; width = 132; }"
      "  : row { alignment = centered; fixed_width = true;"
      "    : button { key = \"accept\"; label = \"Tegn muren\"; width = 18; }"
      "    : button { key = \"opdater\"; label = \"Opdater (Enter)\"; is_default = true; width = 18; }"
      "    : button { key = \"standard\"; label = \"Standard\"; width = 14; }"
      "    : button { key = \"cancel\"; label = \"Annuller\"; is_cancel = true; width = 14; }"
      "  }"
      "}"))
)

;; Vis stenens mål for stentypen i felterne
(defun mr:vis-stenmaal (typ / sm)
  (setq sm (mr:stenmaal typ))
  (set_tile "lang" (sb:tal-kort (car sm)))
  (set_tile "kort" (sb:tal-kort (cadr sm)))
)

;; Ny stentype valgt: vis dens egne mål og regn forfra
(defun mr:skift-sten ()
  (mr:vis-stenmaal (if (= (get_tile "sten") "1") "løbere" "kopper"))
  (mr:opdater)
)

;; Sæt felterne ud fra de gemte valg
(defun mr:felter-vis ()
  (set_tile "sten"   (if (= *mr-typ* "løbere") "1" "0"))
  (mr:vis-stenmaal *mr-typ*)
  (set_tile "side"   (if (= *mr-side* "I") "1" "0"))
  (set_tile "radius" (sb:tal-kort *mr-radius*))
  (set_tile "form"   (if (= *mr-form* "bue") "1" "0"))
  (set_tile "vinkel" (sb:tal-kort *mr-vinkel*))
  (set_tile "fmin"   (sb:tal-kort *rund-fugemin*))
  (set_tile "fmax"   (sb:tal-kort *rund-fugemaks*))
)

;; Læs felterne. Gyldige værdier gemmes (T). Ellers vises fejlen (nil).
(defun mr:laes (/ lang kort radius vinkel fmin fmax bue fejl)
  (setq lang   (sb:tal-ind (get_tile "lang"))
        kort   (sb:tal-ind (get_tile "kort"))
        radius (sb:tal-ind (get_tile "radius"))
        vinkel (sb:tal-ind (get_tile "vinkel"))
        fmin   (sb:tal-ind (get_tile "fmin"))
        fmax   (sb:tal-ind (get_tile "fmax"))
        bue    (= (get_tile "form") "1"))
  (mode_tile "vinkel" (if bue 0 1))
  (setq fejl
    (cond
      ((not (and lang (> lang 0.0)))
       "Stenens lange side skal være et tal over 0.")
      ((not (and kort (> kort 0.0) (<= kort lang)))
       "Stenens korte side skal være over 0 og ikke længere end den lange side.")
      ((not (and radius (> radius 0.0)))
       "Radius skal være et tal over 0.")
      ((and bue (not (and vinkel (> vinkel 0.0) (< vinkel 360.0))))
       "Buens vinkel skal være mellem 0 og 360 grader.")
      ((not (and fmin (> fmin 0.0)))
       "Mindste fuge inderst skal være et tal over 0.")
      ((not (and fmax (> fmax fmin)))
       "Største fuge yderst skal være større end mindste fuge inderst.")))
  (if fejl
    (progn
      (set_tile "advarsel" (strcat "FEJL: " fejl))
      (setq *mr-sig* nil)
      nil)
    (progn
      (setq *mr-typ*        (if (= (get_tile "sten") "1") "løbere" "kopper")
            *mr-radius*     radius
            *mr-side*       (if (= (get_tile "side") "1") "I" "Y")
            *mr-form*       (if bue "bue" "hel")
            *rund-fugemin*  fmin
            *rund-fugemaks* fmax)
      (mr:gem-stenmaal *mr-typ* lang kort)
      (if bue (setq *mr-vinkel* vinkel))
      (set_tile "advarsel" "")
      T))
)

;; Regn mulighederne ud igen, hvis et valg er ændret. T hvis der er muligheder.
(defun mr:opdater (/ sig res i)
  (if (mr:laes)
    (progn
      (setq sig (list *mr-typ* (mr:stenmaal *mr-typ*) *mr-radius* *mr-side* *mr-form*
                      *mr-vinkel* *rund-fugemin* *rund-fugemaks*))
      (if (not (equal sig *mr-sig*))
        (progn
          (setq *mr-sig*  sig
                *mr-g*    (mr:geo)
                res       (if *mr-g* (mr:kandidater *mr-g*))
                *mr-kand* (car res)
                *mr-forv* (cadr res))
          (start_list "liste")
          (if *mr-kand*
            (progn
              (add_list (mr:overskrift))
              (setq i 0)
              (foreach x *mr-kand*
                (add_list (mr:linje x *mr-g* (= i *mr-forv*)))
                (setq i (1+ i)))))
          (end_list)
          (if *mr-kand*
            (progn
              (set_tile "liste" (itoa (1+ *mr-forv*)))
              (mr:vis-valgt (itoa (1+ *mr-forv*))))
            (progn
              (mr:tomme-billeder)
              (foreach k '("v1" "v2" "v3" "v4") (set_tile k ""))))))
      (cond
        ((not *mr-kand*)
         (set_tile "advarsel"
           (if *mr-g*
             "Der er ikke plads til stenene med de fuger - gør radius større eller ændr fugerne."
             "Radius er for lille til murens tykkelse.")))
        ((not (nth 5 (nth *mr-forv* *mr-kand*)))
         (set_tile "advarsel"
           (strcat "Ingen af mulighederne holder fugereglerne - gør radius større"
                   (if (= *mr-typ* "løbere") ", vælg kopper" "") " eller ændr fugerne."))))
      (if *mr-kand* T)))
)

;; Vis linje nr i listen: tekster og billeder
(defun mr:vis-valgt (nr / x tk)
  (setq nr (atoi nr))
  (if (and (> nr 0) (setq x (nth (1- nr) *mr-kand*)))
    (progn
      (setq tk (mr:tekster x *mr-g*))
      (set_tile "v1" (nth 0 tk))
      (set_tile "v2" (nth 1 tk))
      (set_tile "v3" (nth 2 tk))
      (set_tile "v4" (nth 3 tk))
      (mr:billede-hele x)
      (mr:billede-udsnit x))
    ;; overskriften kan ikke vælges - gå til forslaget
    (if *mr-kand*
      (progn
        (set_tile "liste" (itoa (1+ *mr-forv*)))
        (mr:vis-valgt (itoa (1+ *mr-forv*))))))
)

;; "Tegn muren": brug den valgte linje (eller forslaget)
(defun mr:tegn-knap (/ nr)
  (if (mr:opdater)
    (progn
      (setq nr     (atoi (get_tile "liste"))
            *mr-x* (nth (if (> nr 0) (1- nr) *mr-forv*) *mr-kand*))
      (done_dialog 1)))
)

;; "Standard": kopper 108 x 54 (løbere 228 x 108) og fuger 7-20 mm
;; (radius og form beholdes)
(defun mr:standard ()
  (setq *mr-stenmaal* *mr-standardmaal*)
  (set_tile "sten" "0")
  (mr:vis-stenmaal "kopper")
  (set_tile "fmin" "7")
  (set_tile "fmax" "20")
  (mr:opdater)
)

;; Åbn dialogen. Returnerer 1 = tegn, 2 = klik centrum og radius, 0 = annuller,
;; nil = dialogen kan ikke vises.
(defun mr:dialog (/ fn id svar)
  (setq fn (mr:dcl-fil)
        id (load_dialog fn))
  (if (or (< id 0) (not (new_dialog "mr_rund" id)))
    (progn
      (if (>= id 0) (unload_dialog id))
      (vl-file-delete fn)
      nil)
    (progn
      (set_tile "t_lang"   "Lange side set oppefra (mm):")
      (set_tile "t_kort"   "Korte side set oppefra (mm):")
      (set_tile "t_radius" "Radius (mm):")
      (set_tile "t_vinkel" "Buens vinkel (grader, mod uret):")
      (set_tile "t_fmin"   "Mindste fuge inderst (mm):")
      (set_tile "t_fmax"   "Største fuge yderst (mm):")
      (set_tile "t_fuger"  "Fugerne er kileformede: smallest inderst, bredest yderst.")
      (set_tile "legende"
        "Hvid = hel sten    Orange = hugges kileformet (gråt = det der hugges af)    Blå + = centrum")
      (set_tile "legende2"
        "Udsnit: grøn streg over fugen = holder reglen, rød = holder ikke (inderst og yderst)")
      (start_list "sten")
      (add_list "Kopper - lang side ind mod centrum")
      (add_list "Løbere - lang side langs muren")
      (end_list)
      (start_list "side")
      (add_list "Radius til ydersiden (snoren/skabelonen)")
      (add_list "Radius til indersiden")
      (end_list)
      (start_list "form")
      (add_list "Hel cirkel")
      (add_list "Bue - en del af en cirkel")
      (end_list)
      (mr:felter-vis)
      (setq *mr-sig* nil)
      (mr:opdater)
      (foreach k '("lang" "kort" "side" "radius" "form" "vinkel" "fmin" "fmax")
        (action_tile k "(mr:opdater)"))
      (action_tile "sten"     "(mr:skift-sten)")
      (action_tile "liste"    "(mr:vis-valgt $value) (if (= $reason 4) (mr:tegn-knap))")
      (action_tile "opdater"  "(mr:opdater)")
      (action_tile "accept"   "(mr:tegn-knap)")
      (action_tile "klik"     "(if (mr:laes) (done_dialog 2))")
      (action_tile "standard" "(mr:standard)")
      (action_tile "cancel"   "(done_dialog 0)")
      (setq svar (start_dialog))
      (unload_dialog id)
      (vl-file-delete fn)
      svar))
)

;; Uden dialog: spørg i kommandolinjen og brug forslaget. Returnerer centrum (UCS).
(defun mr:kommandolinje (/ c v res k)
  (initget "Kopper Loebere")
  (setq *mr-typ* (if (= (getkword "\nSten [Kopper/Loebere] <Kopper>: ") "Loebere") "løbere" "kopper"))
  (if (not (setq c (getpoint "\nCentrum for cirklen: "))) (exit))
  (initget "Yderside Inderside")
  (setq *mr-side* (if (= (getkword "\nRadius til [Yderside/Inderside] <Yderside>: ") "Inderside") "I" "Y"))
  (initget 6)
  (if (setq v (getdist c (strcat "\nRadius i mm (eller klik) <" (sb:tal-kort *mr-radius*) ">: ")))
    (setq *mr-radius* v))
  (initget "Hel Bue")
  (setq *mr-form* (if (= (getkword "\nHel cirkel eller bue [Hel/Bue] <Hel>: ") "Bue") "bue" "hel"))
  (if (= *mr-form* "bue")
    (progn
      (initget 6)
      (if (setq v (getreal (strcat "\nBuens vinkel i grader <" (sb:tal-kort *mr-vinkel*) ">: ")))
        (setq *mr-vinkel* (min v 359.0)))))
  (setq *mr-g* (mr:geo)
        res    (if *mr-g* (mr:kandidater *mr-g*)))
  (if (car res)
    (progn
      (princ (strcat "\n  " (mr:overskrift)))
      (setq k 0)
      (foreach x (car res)
        (princ (strcat "\n  " (mr:linje x *mr-g* (= k (cadr res)))))
        (setq k (1+ k)))
      (setq *mr-x* (nth (cadr res) (car res)))))
  c
)

;; ---------- Kommandoen ----------
(defun c:MURRUND (/ *error* doc svar c p1 r g x a0 ents k gnavn tk ss)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
      (princ (strcat "\nFejl: " msg)))
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; ---------- dialogen (centrum og radius kan klikkes undervejs) ----------
  (setq *mr-x* nil)
  (while (= (setq svar (mr:dialog)) 2)
    (if (setq p1 (getpoint "\nCentrum for cirklen: "))
      (progn
        (initget 6)
        (if (setq r (getdist p1 (strcat "\nRadius til " (if (= *mr-side* "I") "indersiden" "ydersiden")
                                        " - klik eller skriv mm: ")))
          (setq c p1 *mr-radius* r)))))
  (cond
    ((= svar 0) (exit))                              ; Annuller
    ((null svar) (setq c (mr:kommandolinje))))       ; dialogen kan ikke vises

  (setq g *mr-g* x *mr-x*)
  (if (not (and g x))
    (progn
      (princ "\nDer er ikke plads til stenene - gør radius større eller ændr fugerne.")
      (exit)))

  ;; ---------- centrum og buens start ----------
  (if (not c)
    (if (not (setq c (getpoint "\nCentrum for cirklen: "))) (exit)))
  (setq c (trans c 1 0)
        c (list (car c) (cadr c)))
  (if (mr:hel)
    (setq a0 (/ pi 2.0))
    (progn
      (if (not (setq p1 (getpoint (trans c 0 1)
                                  "\nBuens start - klik retningen fra centrum (buen går mod uret): ")))
        (exit))
      (setq a0 (+ (angle c (trans p1 1 0)) (nth 5 g)))))     ; første stens akse

  ;; ---------- tegn stenene og saml dem i en gruppe ----------
  (vla-StartUndoMark doc)
  (mr:murlag)
  (setq k 0)
  (repeat (nth 0 x)
    (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "Mursten")
                           '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1))
                     (mapcar '(lambda (p) (cons 10 p))
                             (mr:hjoerner c (+ a0 (* k (nth 1 x))) (nth 2 g) (nth 3 g)
                                          (nth 0 g) (nth 4 x)))))
    (setq ents (cons (entlast) ents)
          k    (1+ k)))
  (setq ents  (reverse ents)
        gnavn (vl-catch-all-apply 'sb:gruppe (list doc ents "MURRUND"))
        tk    (mr:tekster x g))
  (princ (strcat "\nMURRUND: " (nth 0 tk) "."))
  (princ (strcat "\n" (nth 1 tk) "."))
  (princ (strcat "\n" (nth 2 tk) "."))
  (princ (strcat "\n" (nth 3 tk) "."))
  (princ (strcat "\nStenene ligger i laget Mursten - gruppe "
                 (if (vl-catch-all-error-p gnavn)
                   (strcat "kunne ikke oprettes: " (vl-catch-all-error-message gnavn))
                   gnavn)
                 "."))
  (if (not (nth 5 x))
    (princ (strcat "\nOBS: denne løsning holder IKKE fugereglerne (" (sb:tal-kort *rund-fugemin*)
                   "-" (sb:tal-kort *rund-fugemaks*) " mm).")))

  (initget "Ja Nej")
  (if (/= (getkword "\nSæt mål på? [Ja/Nej] <Ja>: ") "Nej")
    (progn
      (setq ss (ssadd))
      (foreach e ents (if (entget e) (ssadd e ss)))
      (mr:maal ss)))
  (vla-EndUndoMark doc)
  (princ)
)

;; ============================================================
;; STARTMENU (BUER), HJÆLP (BUEHJAELP) OG KORTE KOMMANDOER
;; ============================================================

;; Mappen BUER.lsp blev indlæst fra (hvis AutoCAD kan finde den)
(setq *buer-mappe*
  (cond ((findfile "BUER.lsp") (vl-filename-directory (findfile "BUER.lsp")))
        ((getenv "BuerMappe"))
        (T nil)))
(if *buer-mappe* (setenv "BuerMappe" *buer-mappe*))

;; Find vejledningen. Spørger efter filen første gang, hvis den ikke
;; ligger ved siden af BUER.lsp, og husker stedet bagefter.
(defun sb:vejledning-fil (/ fn)
  (setq fn (cond ((and *buer-mappe* (findfile (strcat *buer-mappe* "\\BUER_vejledning.txt"))))
                 ((findfile "BUER_vejledning.txt"))
                 ((and (getenv "BuerVejledning") (findfile (getenv "BuerVejledning"))))
                 (T (getfiled "Find BUER_vejledning.txt" "BUER_vejledning" "txt" 0))))
  (if fn (setenv "BuerVejledning" fn))
  fn
)

;; Læs en tekstfil som UTF-8 (AutoCAD 2021+), ellers almindeligt
(defun sb:laes-linjer (fn / f l lst)
  (setq f (vl-catch-all-apply 'open (list fn "r" "utf8")))
  (if (or (null f) (vl-catch-all-error-p f)) (setq f (open fn "r")))
  (if f
    (progn
      (while (setq l (read-line f))
        (setq lst (cons (vl-string-translate "\t" " " l) lst)))
      (close f)))
  (reverse lst)
)

(defun sb:dcl-skriv (navn linjer / fn f)
  (setq fn (vl-filename-mktemp navn nil ".dcl")
        f  (open fn "w"))
  (foreach l linjer (write-line l f))
  (close f)
  fn
)

;; --- BUEHJAELP: vejledningen i et vindue ---
(defun c:BUEHJAELP (/ fn linjer dfn id svar)
  (if (not (setq fn (sb:vejledning-fil)))
    (princ "\nFandt ikke BUER_vejledning.txt - læg den i samme mappe som BUER.lsp.")
    (progn
      (setq linjer (sb:laes-linjer fn)
            dfn (sb:dcl-skriv "buerhjaelp"
                  '("buer_hjaelp : dialog {"
                    "  label = \"BUER - vejledning\";"
                    "  : list_box { key = \"tekst\"; width = 78; height = 32; fixed_width_font = true; }"
                    "  : row { alignment = centered; fixed_width = true;"
                    "    : button { key = \"notepad\"; label = \"Aabn i Notesblok\"; width = 20; }"
                    "    : button { key = \"accept\"; label = \"Luk\"; is_default = true; is_cancel = true; width = 12; }"
                    "  }"
                    "}"))
            id  (load_dialog dfn))
      (if (and (>= id 0) (new_dialog "buer_hjaelp" id))
        (progn
          (start_list "tekst")
          (foreach l linjer (add_list l))
          (end_list)
          (action_tile "notepad" "(done_dialog 2)")
          (action_tile "accept"  "(done_dialog 1)")
          (setq svar (start_dialog))
          (unload_dialog id)
          (if (= svar 2) (startapp "notepad" fn)))
        (progn
          (if (>= id 0) (unload_dialog id))
          (startapp "notepad" fn)))
      (vl-file-delete dfn)))
  (princ)
)

;; ---------- BUER-menuen: tegninger på knapperne ----------
;; Tegningerne laves i et lille koordinatsystem (spændvidde 100, vederlag
;; i y = 0) og skaleres, så de passer i knappen. En bue beskrives som en
;; liste af cirkelbuer (cx cy r fra til) med vinklerne mod uret.

;; Den cirkelbue der indeholder vinklen v
(defun bmenu:stykke (geo v / res g)
  (foreach g geo
    (if (and (not res) (<= (- (nth 3 g) 1e-9) v (+ (nth 4 g) 1e-9)))
      (setq res g)))
  (if res res (last geo))
)

;; Punkt på buen i vinklen v, afstanden off ud fra undersiden
(defun bmenu:pkt (geo v off / g)
  (setq g (bmenu:stykke geo v))
  (polar (list (car g) (cadr g)) v (+ (caddr g) off))
)

;; Fugernes vinkler: n sten med lige store stykker på oversiden
;; (afstand D fra undersiden) mellem vinklerne v0 og v1
(defun bmenu:fuger (geo D v0 v1 n / stk tot k l s a b res g)
  (foreach g geo
    (setq a (max (nth 3 g) v0)
          b (min (nth 4 g) v1))
    (if (> b a) (setq stk (cons (list a b (* (- b a) (+ (caddr g) D)) (+ (caddr g) D)) stk))))
  (setq stk (reverse stk)
        tot (apply '+ (mapcar 'caddr stk))
        k   0)
  (while (<= k n)
    (setq l (* tot (/ (float k) n))
          s stk)
    (while (and (cdr s) (> l (caddr (car s))))
      (setq l (- l (caddr (car s)))
            s (cdr s)))
    (setq res (cons (min (cadr (car s)) (+ (car (car s)) (/ l (cadddr (car s))))) res)
          k   (1+ k)))
  (reverse res)
)

;; Punkt på en stens kant mellem fugerne v0 og v1: andel 0 ved v0 og 1
;; ved v1, afstanden off fra undersiden. Der er en halv fuge (g) i hver ende.
(defun bmenu:kant (geo v0 v1 andel off g / a0 a1)
  (setq a0 (+ v0 (/ g 2.0 (+ (caddr (bmenu:stykke geo v0)) off)))
        a1 (- v1 (/ g 2.0 (+ (caddr (bmenu:stykke geo v1)) off))))
  (bmenu:pkt geo (+ a0 (* andel (- a1 a0))) off)
)

;; Stenene mellem fugevinklerne fl som polygoner (lister af punkter)
(defun bmenu:sten (geo fl D g / res pl i)
  (while (cdr fl)
    (setq pl nil i 0)
    (repeat 5                                   ; undersiden frem
      (setq pl (cons (bmenu:kant geo (car fl) (cadr fl) (/ i 4.0) 0.0 g) pl)
            i  (1+ i)))
    (repeat 5                                   ; oversiden tilbage
      (setq i  (1- i)
            pl (cons (bmenu:kant geo (car fl) (cadr fl) (/ i 4.0) D g) pl)))
    (setq res (cons pl res)
          fl  (cdr fl)))
  (reverse res)
)

;; Hver bue giver (sten streger): stenene som polygoner og murens
;; streger som (punkt punkt). Alle tre buer har samme mur omkring, så de
;; tegnes lige store: åbningens sider og vederlaget ud til x = 77.
(defun bmenu:mur (ekstra)
  (append (list (list '(50.0 0.0) '(50.0 -20.0))
                (list '(-50.0 0.0) '(-50.0 -20.0))
                (list '(50.0 0.0) '(77.0 0.0))
                (list '(-50.0 0.0) '(-77.0 0.0)))
          ekstra)
)

(defun bmenu:segbue (/ D h r cy v0 geo sk)
  (setq D  20.0  h 18.0
        r  (/ (+ 2500.0 (* h h)) (* 2.0 h))
        cy (- h r)
        v0 (atan (- cy) 50.0)
        geo (list (list 0.0 cy r v0 (- pi v0)))
        sk (polar (list 0.0 cy) v0 (+ r D)))
  (list (bmenu:sten geo (bmenu:fuger geo D v0 (- pi v0) 15) D 1.6)
        (bmenu:mur (list (list '(50.0 0.0) sk)          ; skrå vederlag
                         (list '(-50.0 0.0) (kl:spejl sk)))))
)

(defun bmenu:kurvebue (/ D r1 r2 yc geo)
  (setq D  19.0
        r1 (/ 100.0 3.0)
        r2 (- 100.0 r1)
        yc (* (sin (/ pi 3.0)) (- r1 r2))
        geo (list (list (- 50.0 r1) 0.0 r1 0.0 (/ pi 3.0))
                  (list 0.0 yc r2 (/ pi 3.0) (/ (* 2.0 pi) 3.0))
                  (list (- r1 50.0) 0.0 r1 (/ (* 2.0 pi) 3.0) pi)))
  (list (bmenu:sten geo (bmenu:fuger geo D 0.0 pi 23) D 1.6)
        (bmenu:mur nil))
)

;; Låsestenen i højre hak: fra sidebuens sidste fuge op til hakket q,
;; ned ad topbuens første fuge og langs undersiden tilbage over knækket.
(defun bmenu:laas (r q vA vB / cA cB)
  (setq cA (list r 0.0)
        cB (list 0.0 r))
  (list (polar cA vA r) q (polar cB vB r)
        (polar cB (* vB 0.5) r)
        (list r r)
        (polar cA (/ (+ vA (/ pi 2.0)) 2.0) r))
)

;; Kløverbladsbue: 3 cirkler med R = spændvidde/4, låsesten i hakkene
;; (AutoLISP skelner ikke mellem store og små bogstaver - derfor "luft", ikke "d")
(defun bmenu:klover (/ D r ro s q vA vB luft geoA geoB geoC laas)
  (setq D  11.0  r 25.0  ro (+ r D)
        s  (sqrt (/ (- (* ro ro) (* 0.5 r r)) 2.0))
        q  (list (+ (* 0.5 r) s) (+ (* 0.5 r) s))
        vA (angle (list r 0.0) q)
        vB (angle (list 0.0 r) q)
        luft (/ 0.8 (+ r (/ D 2.0)))            ; ekstra fuge ved låsestenen
        geoA (list (list r 0.0 r 0.0 (/ pi 2.0)))
        geoB (list (list 0.0 r r 0.0 pi))
        geoC (list (list (- r) 0.0 r (/ pi 2.0) pi))
        laas (bmenu:laas r q vA vB))
  (list (append (bmenu:sten geoA (bmenu:fuger geoA D 0.0 (- vA luft) 5) D 1.6)
                (bmenu:sten geoB (bmenu:fuger geoB D (+ vB luft) (- pi vB luft) 11) D 1.6)
                (bmenu:sten geoC (bmenu:fuger geoC D (+ (- pi vA) luft) pi 5) D 1.6)
                (list laas (mapcar 'kl:spejl laas)))
        (bmenu:mur nil))
)

;; Rund mur set oppefra: en ring af sten og et kryds i centrum
(defun bmenu:murrund (/ geo)
  (setq geo (list (list 0.0 0.0 30.0 0.0 (* 2.0 pi))))
  (list (bmenu:sten geo (bmenu:fuger geo 16.0 0.0 (* 2.0 pi) 24) 16.0 1.6)
        (list (list '(-6.0 0.0) '(6.0 0.0))
              (list '(0.0 -6.0) '(0.0 6.0))))
)

;; Tegn sten (fyldte, teglrøde) og streger (grå) i billedknappen key.
;; Tegningen skaleres og centreres i knappen.
(defun bmenu:tegn (key sten streger / bw bh alle x0 x1 y0 y1 sk ox oy px s pl)
  (setq bw   (dimx_tile key)
        bh   (dimy_tile key)
        alle (apply 'append (append sten streger))
        x0   (apply 'min (mapcar 'car alle))
        x1   (apply 'max (mapcar 'car alle))
        y0   (apply 'min (mapcar 'cadr alle))
        y1   (apply 'max (mapcar 'cadr alle))
        sk   (min (/ (- bw 14.0) (- x1 x0)) (/ (- bh 14.0) (- y1 y0)))
        ox   (- (/ bw 2.0) (* sk (/ (+ x0 x1) 2.0)))
        oy   (+ (/ bh 2.0) (* sk (/ (+ y0 y1) 2.0))))
  (defun px (p) (list (+ ox (* sk (car p))) (- oy (* sk (cadr p)))))
  (start_image key)
  (fill_image 0 0 bw bh -15)                    ; dialogens baggrund
  (foreach s streger
    (bill:streg (px (car s)) (px (cadr s)) bw bh 8))
  (foreach pl sten
    (bill:fyld (mapcar 'px pl) bw bh 22))
  (end_image)
)

;; Tegn én knap: fn er funktionen der giver (sten streger)
(defun bmenu:ikon (key fn / x)
  (setq x (apply fn nil))
  (bmenu:tegn key (car x) (cadr x))
)

;; --- BUER: startmenu med knapper ---
;; Billedknapper med små tegninger af buerne (bmenu:-funktionerne ovenfor).
;; Tekster med æ, ø og å sættes fra LISP - i DCL-filen står de uden.
(defun c:BUER (/ dfn id svar x)
  (setq dfn (sb:dcl-skriv "buermenu"
              '("buer_menu : dialog {"
                "  label = \"Buer og rund mur\";"
                "  : boxed_row { label = \"Tegn\";"
                "    : column {"
                "      : image_button { key = \"i_segbue\"; width = 24; height = 7; color = dialog_background; }"
                "      : button { key = \"segbue\"; label = \"Segmentbue\"; }"
                "    }"
                "    : column {"
                "      : image_button { key = \"i_kurvebue\"; width = 24; height = 7; color = dialog_background; }"
                "      : button { key = \"kurvebue\"; label = \"Kurvehanksbue\"; }"
                "    }"
                "    : column {"
                "      : image_button { key = \"i_klover\"; width = 24; height = 7; color = dialog_background; }"
                "      : button { key = \"klover\"; label = \"Kloeverbladsbue\"; }"
                "    }"
                "    : column {"
                "      : image_button { key = \"i_murrund\"; width = 24; height = 7; color = dialog_background; }"
                "      : button { key = \"murrund\"; label = \"Rund mur\"; }"
                "    }"
                "  }"
                "  : boxed_row { label = \"Efter buen er tegnet\";"
                "    : button { key = \"buetrim\"; label = \"Trim murvaerket\"; }"
                "    : button { key = \"buemaal\"; label = \"Maal og opsnoering\"; }"
                "    : button { key = \"skabelon\"; label = \"Bueskive 1:1\"; }"
                "  }"
                "  : text { label = \"Genveje:   SB   KB   KLB   MRU      BT   BMA   BSK      BHJ\"; alignment = centered; }"
                "  : row { alignment = centered; fixed_width = true;"
                "    : button { key = \"hjaelp\"; label = \"Vejledning\"; width = 16; }"
                "    : button { key = \"cancel\"; label = \"Luk\"; is_cancel = true; width = 12; }"
                "  }"
                "}"))
        id  (load_dialog dfn))
  (if (and (>= id 0) (new_dialog "buer_menu" id))
    (progn
      (set_tile "klover"  "Kløverbladsbue")
      (set_tile "buetrim" "Trim murværket")
      (set_tile "buemaal" "Mål og opsnøring")
      ;; tegningerne på knapperne - går en tegning galt, er knappen bare tom
      (foreach x '(("i_segbue" bmenu:segbue) ("i_kurvebue" bmenu:kurvebue)
                   ("i_klover" bmenu:klover) ("i_murrund" bmenu:murrund))
        (vl-catch-all-apply 'bmenu:ikon x))
      (action_tile "segbue"     "(done_dialog 1)")
      (action_tile "i_segbue"   "(done_dialog 1)")
      (action_tile "kurvebue"   "(done_dialog 2)")
      (action_tile "i_kurvebue" "(done_dialog 2)")
      (action_tile "buetrim"    "(done_dialog 3)")
      (action_tile "buemaal"    "(done_dialog 4)")
      (action_tile "skabelon"   "(done_dialog 5)")
      (action_tile "hjaelp"     "(done_dialog 6)")
      (action_tile "klover"     "(done_dialog 7)")
      (action_tile "i_klover"   "(done_dialog 7)")
      (action_tile "murrund"    "(done_dialog 8)")
      (action_tile "i_murrund"  "(done_dialog 8)")
      (action_tile "cancel"     "(done_dialog 0)")
      (setq svar (start_dialog))
      (unload_dialog id)
      (vl-file-delete dfn)
      ;; kommandoen køres efter dialogen er lukket
      (cond ((= svar 1) (c:SEGBUE))
            ((= svar 2) (c:KURVEBUE))
            ((= svar 3) (c:BUETRIM))
            ((= svar 4) (c:BUEMAAL))
            ((= svar 5) (c:BUESKABELON))
            ((= svar 6) (c:BUEHJAELP))
            ((= svar 7) (c:KLOVERBUE))
            ((= svar 8) (c:MURRUND))))
    (progn
      (if (>= id 0) (unload_dialog id))
      (vl-file-delete dfn)
      (princ "\nKommandoer: SEGBUE (SB), KURVEBUE (KB), KLOVERBUE (KLB), MURRUND (MRU), BUETRIM (BT), BUEMAAL (BMA), BUESKABELON (BSK), BUEHJAELP (BHJ)")))
  (princ)
)

;; --- Korte kommandoer ---
(defun c:SB  () (c:SEGBUE))
(defun c:KB  () (c:KURVEBUE))
(defun c:KLB () (c:KLOVERBUE))
(defun c:MRU () (c:MURRUND))
(defun c:BT  () (c:BUETRIM))
(defun c:BMA () (c:BUEMAAL))
(defun c:BSK () (c:BUESKABELON))
(defun c:BHJ () (c:BUEHJAELP))

(princ "\nBUER indlæst - skriv BUER for menuen.  Kommandoer: SEGBUE (SB), KURVEBUE (KB), KLOVERBUE (KLB), MURRUND (MRU), BUETRIM (BT), BUEMAAL (BMA), BUESKABELON (BSK), BUEHJAELP (BHJ)")
(princ)
