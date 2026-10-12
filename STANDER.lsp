;;; ============================================================
;;; STANDER.lsp  -  Standerskifte / rulskifte mellem 2 punkter
;;; Kommando: STANDERSKIFTE  (kort: STSK)
;;;
;;; Brug:
;;;  1. Skriv STANDERSKIFTE (eller klik på knappen i paletten Mur)
;;;  2. Klik første og andet punkt (yderkanten af første og sidste sten)
;;;  3. Vælg sten: Stander 54 x 228 eller Rulskifte 54 x 108
;;;  4. Skriv antal rækker (1-10)
;;;
;;; Regler:
;;;  - Stenene står helt ude ved begge punkter, fuger kun imellem stenene
;;;  - Antal sten vælges, så fugen bliver tættest på 12 mm, og alle
;;;    fuger gøres lige store
;;;  - Rækkerne står oven på hinanden med 12 mm liggefuge og samme
;;;    inddeling, så de lodrette fuger går i lod
;;;  - Første række står i højde med første punkt
;;;  Nye sten tegnes i laget Mursten. Sidste valg huskes.
;;;
;;; NB: AutoLISP skelner ikke mellem store og små bogstaver -
;;;     brug aldrig fx både F og f som variabler.
;;; Én gang U fortryder hele kommandoen.
;;; ============================================================

(vl-load-com)

(setq *st-fuge*    12.0        ; ønsket fuge (lodret og vandret)
      *st-bredde*  54.0        ; stenens bredde set forfra
      *st-lagnavn* "Mursten"
      *st-typer*   '(("Stander" . 228.0) ("Rulskifte" . 108.0)))   ; stenens højde

(if (not *st-type*)    (setq *st-type* "Stander"))
(if (not *st-raekker*) (setq *st-raekker* 1))

(defun st:tal (x d) (vl-string-subst "," "." (rtos x 2 d)))

;; Nye sten tegnes i laget Mursten (farve og linjetype ByLayer).
;; Laget oprettes, hvis det ikke findes.
(defun st:lag ()
  (if (not (tblsearch "LAYER" *st-lagnavn*))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 *st-lagnavn*)
                   '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
  (list (cons 8 *st-lagnavn*))
)

;; Tegn en sten som lukket rektangel i højden z
(defun st:sten (x1 y1 x2 y2 z eg)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity"))
      eg
      (list '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) (cons 38 z)
            (list 10 x1 y1) (list 10 x2 y1)
            (list 10 x2 y2) (list 10 x1 y2))))
)

;; Fugen med n sten på afstanden afst (stenene helt ude i begge ender)
(defun st:fuge (afst n) (/ (- afst (* n *st-bredde*)) (- n 1.0)))

;; Inddeling: det antal sten (mindst 2), der giver en fuge tættest på
;; 12 mm. Returnerer (antal fuge) eller nil, hvis 2 sten ikke kan være der.
(defun st:inddel (afst / na bedst)
  ;; na = flest sten hvor fugen stadig er mindst 12 mm, na+1 = fuge under 12
  (setq na (max 2 (fix (/ (+ afst *st-fuge*) (+ *st-bredde* *st-fuge*)))))
  (foreach n (list na (1+ na))
    (if (and (>= (st:fuge afst n) 0.0)
             (or (not bedst)
                 (< (abs (- (st:fuge afst n) *st-fuge*))
                    (abs (- (cadr bedst) *st-fuge*)))))
      (setq bedst (list n (st:fuge afst n)))))
  bedst
)

;; ============================================================
;; Kommandoen
;; ============================================================
(defun c:STANDERSKIFTE (/ *error* doc p1 p2 xa y0 z afst ind n fuge h
                          svar r eg i k x y)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\nFejl: " msg)))
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; ---------- de 2 punkter ----------
  (if (not (setq p1 (getpoint "\nFørste punkt (yderkant af første sten): "))) (exit))
  (if (not (setq p2 (getpoint p1 "\nAndet punkt (yderkant af sidste sten): "))) (exit))
  (setq p1   (trans p1 1 0)
        p2   (trans p2 1 0)
        xa   (min (car p1) (car p2))
        y0   (cadr p1)
        z    (caddr p1)
        afst (abs (- (car p2) (car p1))))
  (if (not (setq ind (st:inddel afst)))
    (progn
      (princ (strcat "\nAfstanden er " (st:tal afst 0) " mm - der skal være plads til mindst 2 sten ("
                     (st:tal (* 2 *st-bredde*) 0) " mm). Intet tegnet."))
      (exit)))
  (setq n (car ind) fuge (cadr ind))

  ;; ---------- sten og antal rækker ----------
  (initget "Stander Rulskifte")
  (if (setq svar (getkword (strcat "\nSten [Stander/Rulskifte] <" *st-type* ">: ")))
    (setq *st-type* svar))
  (while (progn
           (initget 6)
           (setq r (getint (strcat "\nAntal rækker (1-10) <" (itoa *st-raekker*) ">: ")))
           (and r (> r 10) (princ "\nHøjst 10 rækker.")))
  )
  (if r (setq *st-raekker* r))
  (setq h (cdr (assoc *st-type* *st-typer*)))

  ;; ---------- tegn ----------
  (vla-StartUndoMark doc)
  (setq eg (st:lag) k 0)
  (repeat *st-raekker*
    (setq y (+ y0 (* k (+ h *st-fuge*))) i 0)
    (repeat n
      (setq x (+ xa (* i (+ *st-bredde* fuge))))
      (st:sten x y (+ x *st-bredde*) (+ y h) z eg)
      (setq i (1+ i)))
    (setq k (1+ k)))

  (if (> (abs (- (cadr p2) (cadr p1))) 1.0)
    (princ (strcat "\nPunkterne står ikke i samme højde - stenene står i højde med første punkt.")))
  (princ (strcat "\nSTANDERSKIFTE: afstand " (st:tal afst 0) " mm, "
                 (itoa n) " sten pr. række, fuge " (st:tal fuge 1) " mm."
                 "\n  " (itoa *st-raekker*) (if (= *st-raekker* 1) " række " " rækker ")
                 (strcase *st-type* T) " (" (st:tal *st-bredde* 0) " x " (st:tal h 0) ") = "
                 (itoa (* n *st-raekker*)) " sten i laget " *st-lagnavn*
                 ", højde i alt " (st:tal (- (* *st-raekker* (+ h *st-fuge*)) *st-fuge*) 0) " mm."))
  (vla-EndUndoMark doc)
  (princ)
)

(defun c:STSK () (c:STANDERSKIFTE))

(princ "\nSTANDER.lsp indlæst. Kommando: STANDERSKIFTE (kort: STSK) - standerskifte/rulskifte mellem 2 punkter.")
(princ)
