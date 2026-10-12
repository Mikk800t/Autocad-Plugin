;;; ===========================================================================
;;;  AutoReload.lsp  -  Indlaeser LISP-filer og vaerktoejspaletter og
;;;                     genindlaeser dem automatisk naar de aendres
;;; ---------------------------------------------------------------------------
;;;  Naar filen indlaeses (APPLOAD):
;;;    - Alle overvaagede LISP-filer indlaeses
;;;    - Alle vaerktoejspaletter (.xtp) i C:\Murer_Toolpalettes_Autocad
;;;      laegges ind i AutoCAD, som laeser dem direkte fra mappen
;;;
;;;  Overvaager:
;;;    1) Standardmappen C:\Murer_Toolpalettes_Autocad (se ar:*default-folders*)
;;;    2) Alle .lsp/.fas/.vlx filer i APPLOAD "Startup Suite"
;;;    3) Filer du selv tilfoejer med ARADD
;;;    4) Ekstra mapper du tilfoejer med ARFOLDER
;;;    5) Paletterne Buer og Mur (Buer.xtp/Mur.xtp + ikoner, se ar:*pal-auto*)
;;;
;;;  Naar en fil gemmes med en ny version (aendret dato/stoerrelse), bliver
;;;  den genindlaest naar du starter naeste kommando (fx BUER). Bagefter
;;;  laegges Buer og Mur ind igen, hvis deres .xtp eller ikoner er aendret.
;;;  NB: Ren AutoLISP har ingen timer, saa genindlaesningen sker ved
;;;  naeste kommando/klik paa en knap - ikke i selve sekundet filen gemmes.
;;;
;;;  Kommandoer:
;;;    ARSTATUS   - Vis overvaagede filer og paletter
;;;    ARADD      - Tilfoej en LISP-fil
;;;    ARFOLDER   - Tilfoej en mappe
;;;    ARREMOVE   - Fjern en fil/mappe fra listen
;;;    ARNOW      - Tjek og genindlaes nu
;;;    ARTOGGLE   - Slaa automatisk genindlaesning til/fra
;;;    ARALLE     - Genindlaes ALLE filer og paletter nu (knappen i paletten Mur)
;;;
;;;  Installation: APPLOAD -> vaelg AutoReload_2.lsp -> Load.
;;;  (Tilfoej den evt. under "Contents..." (Startup Suite), saa den koerer
;;;  i alle tegninger af sig selv.)
;;; ===========================================================================

(vl-load-com)

(setq ar:*tag* "AUTORELOAD")

;; Mapper der altid overvaages (tilfoej flere med ; adskilt i listen)
(setq ar:*default-folders* '("C:\\Murer_Toolpalettes_Autocad"))
(if (null ar:*enabled*) (setq ar:*enabled* T))

;; Paletter der holdes opdateret mens du tegner (navnet paa .xtp-filen)
(setq ar:*pal-auto* '("Buer" "Mur"))
;; Fast id for palettelisten (AcTpCatalog.atc) i projektmappen
(setq ar:*pal-kat-id* "{7479899D-88FE-4019-B48E-22675E6778CF}")

;;; ---------------------------------------------------------------- hjaelpere

(defun ar:split (s d / p r)
  (if s
    (progn
      (while (setq p (vl-string-search d s))
        (setq r (cons (substr s 1 p) r)
              s (substr s (+ p 1 (strlen d)))
        )
      )
      (setq r (cons s r))
    )
  )
  (vl-remove "" (reverse r))
)

(defun ar:join (l d / r)
  (setq r "")
  (foreach x l (setq r (if (= r "") x (strcat r d x))))
  r
)

(defun ar:uniq (l / r)
  (foreach x l
    (if (not (member (strcase x) (mapcar 'strcase r)))
      (setq r (cons x r))
    )
  )
  (reverse r)
)

(defun ar:lispfile-p (f)
  (wcmatch (strcase f) "*.LSP,*.FAS,*.VLX")
)

;; Fuld sti hvis filen findes, ellers den oprindelige tekst
;; (NB: (or ...) returnerer T i AutoLISP, ikke vaerdien)
(defun ar:full (f / p)
  (if (setq p (findfile f)) p f)
)

;; Denne fil maa aldrig genindlaese sig selv
(defun ar:self-p (f)
  (wcmatch (strcase (vl-filename-base f)) "AUTORELOAD*")
)

;; "Fingeraftryk" af en fil: tidsstempel + stoerrelse
(defun ar:stamp (f / tm)
  (if (and f (findfile f) (setq tm (vl-file-systime f)))
    (list tm (vl-file-size f))
  )
)

;;; ------------------------------------------------------------ konfiguration

(defun ar:read-config ()
  (setq ar:*files*   (ar:split (getenv "AutoReload_Files") ";")
        ar:*folders* (ar:split (getenv "AutoReload_Folders") ";")
        ar:*folders* (ar:uniq (append (vl-remove-if-not 'vl-file-directory-p
                                                        ar:*default-folders*)
                                      ar:*folders*))
  )
)

(defun ar:save-config ()
  (setenv "AutoReload_Files"   (ar:join ar:*files* ";"))
  (setenv "AutoReload_Folders" (ar:join ar:*folders* ";"))
)

;; Laes Startup Suite fra registreringsdatabasen
(defun ar:startup-files (/ prod key n i v r)
  (setq prod (if vlax-user-product-key
               (vlax-user-product-key)
               (vlax-product-key)
             )
        key  (strcat "HKEY_CURRENT_USER\\" prod
                     "\\Profiles\\" (getvar "CPROFILE")
                     "\\Dialogs\\Appload\\Startup"
             )
        n    (vl-registry-read key "NumStartup")
  )
  (if (= (type n) 'STR) (setq n (atoi n)))
  (if (= (type n) 'INT)
    (progn
      (setq i 1)
      (while (<= i n)
        (setq v (vl-registry-read key (strcat (itoa i) "Startup")))
        (if (and (= (type v) 'STR) (ar:lispfile-p v))
          (setq r (cons v r))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse r)
)

(defun ar:folder-files (dir / r)
  (setq dir (vl-string-right-trim "\\/" dir))
  (foreach pat '("*.lsp" "*.fas" "*.vlx")
    (foreach f (vl-directory-files dir pat 1)
      (setq r (cons (strcat dir "\\" f) r))
    )
  )
  (reverse r)
)

;; Samlet liste over filer der overvaages (fulde stier)
(defun ar:watched (/ l)
  (setq l (append ar:*files*
                  ar:*startup*
                  (apply 'append (mapcar 'ar:folder-files ar:*folders*))
          )
        l (mapcar '(lambda (f) (ar:full f)) l)
  )
  (vl-remove-if 'ar:self-p
    (vl-remove-if-not 'findfile (ar:uniq l))
  )
)

;;; ------------------------------------------------------------- indlaesning

(defun ar:load (f / r)
  (setq r (vl-catch-all-apply 'load (list f)))
  (if (vl-catch-all-error-p r)
    (princ (strcat "\nAutoReload: FEJL ved indlaesning af " f
                   " -> " (vl-catch-all-error-message r)))
    (princ (strcat "\nAutoReload: genindlaest "
                   (vl-filename-base f) (vl-filename-extension f)))
  )
)

(defun ar:set-stamp (f / key st old)
  (setq key (strcase f)
        st  (ar:stamp f)
        old (assoc key ar:*stamps*)
  )
  (if st
    (setq ar:*stamps*
      (if old
        (subst (cons key st) old ar:*stamps*)
        (cons (cons key st) ar:*stamps*)
      )
    )
  )
)

;; Find aendrede/nye filer og genindlaes dem
(defun ar:check-core (/ changed key st old)
  (foreach f (ar:watched)
    (setq key (strcase f)
          st  (ar:stamp f)
          old (assoc key ar:*stamps*)
    )
    (cond
      ((null st))                                   ; fil optaget/utilgaengelig
      ((null old)                                   ; ny fil -> kun registrer
       (setq ar:*stamps* (cons (cons key st) ar:*stamps*)))
      ((not (equal (cdr old) st))                   ; ny version
       (setq ar:*stamps* (subst (cons key st) old ar:*stamps*)
             changed     (cons f changed)))
    )
  )
  (foreach f (reverse changed) (ar:load f))
  ;; Bagefter: Buer og Mur skal ind igen, hvis .xtp eller ikonerne er aendret
  (foreach b (ar:pal-skal-laves nil)
    (if (not (member b ar:*pal-venter*))
      (setq ar:*pal-venter* (append ar:*pal-venter* (list b)))
    )
  )
)

;; force = tjek nu (uden 1-sek. graensen)
;; pal   = T hvis paletterne maa laegges ind nu (ikke midt i en kommando)
(defun ar:check (force pal / now err)
  (setq now (getvar "MILLISECS"))
  (if (and ar:*enabled* (not ar:*busy*))
    (progn
      (setq ar:*busy* T)
      (if (or force
              (null ar:*last*)
              (null now)
              (> (abs (- now ar:*last*)) 1000))     ; max 1 tjek pr. sekund
        (progn
          (setq ar:*last* now)
          (setq err (vl-catch-all-apply 'ar:check-core nil))
          (if (vl-catch-all-error-p err)
            (princ (strcat "\nAutoReload fejl: " (vl-catch-all-error-message err)))
          )
        )
      )
      (if (and pal (or ar:*pal-venter* ar:*pal-opsaet*))
        (ar:pal-koer)
      )
      (setq ar:*busy* nil)
    )
  )
  (princ)
)

;;; ------------------------------------------------------- vaerktoejspaletter
;;;  Hver .xtp-fil i projektmappen bliver til en palette, som AutoCAD laeser
;;;  direkte fra projektmappen - intet kopieres til AutoCAD's egne mapper:
;;;    <navn>\<navn>.atc   paletten, lavet ud fra <navn>.xtp
;;;    <navn>\Images\      ikonerne (bruges som de er)
;;;    AcTpCatalog.atc     listen over paletterne
;;;  Projektmappen kommer paa AutoCAD's liste over palettemapper
;;;  (Options -> Files -> Tool Palettes File Locations).

;; Projektmappen med .xtp-filerne
(defun ar:pal-root () (car ar:*default-folders*))

(defun ar:pal-xtp (b) (strcat (ar:pal-root) "\\" b ".xtp"))

;; Mappen med ikonerne (DependentFilesPath i .xtp - normalt samme navn)
(defun ar:pal-dep (b / x)
  (if (setq x (assoc (strcase b) ar:*pal-dep*)) (cdr x) b)
)

(defun ar:pal-atc (b) (strcat (ar:pal-root) "\\" (ar:pal-dep b) "\\" b ".atc"))

;; Navne (uden .xtp) paa alle palettefiler i projektmappen
(defun ar:pal-alle ()
  (mapcar 'vl-filename-base (vl-directory-files (ar:pal-root) "*.xtp" 1))
)

;; Tidspunkt som tekst der kan sammenlignes, fx "20260926132953948"
(defun ar:tid (f / tm)
  (if (and f (setq tm (vl-file-systime f)))
    (apply 'strcat
      (mapcar '(lambda (n w / s)
                 (setq s (itoa n))
                 (while (< (strlen s) w) (setq s (strcat "0" s)))
                 s
               )
              (list (nth 0 tm) (nth 1 tm) (nth 3 tm) (nth 4 tm)
                    (nth 5 tm) (nth 6 tm) (nth 7 tm))
              '(4 2 2 2 2 2 3)
      )
    )
  )
)

;; T hvis paletten skal laves (igen): .atc mangler, eller .xtp eller et
;; ikon er nyere end .atc. En .xtp der fejlede, proeves foerst igen naar
;; den bliver gemt paa ny.
(defun ar:pal-gammel-p (b / x atc ta img)
  (setq x   (ar:pal-xtp b)
        atc (ar:pal-atc b))
  (and (findfile x)
       (not (equal (ar:stamp x) (cdr (assoc (strcase b) ar:*pal-fejl*))))
       (or (not (findfile atc))
           (> (ar:tid x) (setq ta (ar:tid atc)))
           (progn
             (setq img (strcat (ar:pal-root) "\\" (ar:pal-dep b) "\\Images"))
             (vl-some '(lambda (f) (> (ar:tid (strcat img "\\" f)) ta))
                      (vl-directory-files img "*.png" 1))
           )
       )
  )
)

;; Paletter der skal laves nu: alle = T -> alle .xtp, ellers kun ar:*pal-auto*
(defun ar:pal-skal-laves (alle)
  (vl-remove-if-not 'ar:pal-gammel-p (if alle (ar:pal-alle) ar:*pal-auto*))
)

;; Paletterne maa kun skiftes, naar der ikke koerer en kommando
(defun ar:pal-nu-ok-p ()
  (zerop (logand (getvar "CMDACTIVE") 15))
)

;; --- XML via MSXML (samme bibliotek som AutoCAD selv bruger til paletterne)

(defun ar:xml-ny (/ d)
  (foreach p '("Msxml2.DOMDocument.6.0" "Msxml2.DOMDocument.3.0")
    (if (null d)
      (progn
        (setq d (vl-catch-all-apply 'vlax-create-object (list p)))
        (if (vl-catch-all-error-p d) (setq d nil))
      )
    )
  )
  (if d
    (progn
      (vlax-put-property d 'async :vlax-false)
      (vl-catch-all-apply 'vlax-invoke-method
        (list d 'setProperty "SelectionLanguage" "XPath"))
    )
  )
  d
)

;; Aabn en XML-fil (nil hvis den mangler eller ikke kan laeses)
(defun ar:xml-aabn (f / d)
  (if (and f (findfile f) (setq d (ar:xml-ny)))
    (if (equal (vlax-invoke-method d 'load f) :vlax-true)
      d
      (progn (vlax-release-object d) nil)
    )
  )
)

(defun ar:xml-luk (d)
  (if d (vl-catch-all-apply 'vlax-release-object (list d)))
)

(defun ar:xml-node (n xp)
  (vlax-invoke-method n 'selectSingleNode xp)
)

(defun ar:xml-tekst (n xp / x)
  (if (setq x (ar:xml-node n xp)) (vlax-get-property x 'text))
)

(defun ar:xml-noder (n xp / l x r)
  (setq l (vlax-invoke-method n 'selectNodes xp))
  (while (setq x (vlax-invoke-method l 'nextNode))
    (setq r (cons x r))
  )
  (reverse r)
)

;; Erstat alle forekomster af a med b i s
(defun ar:erstat (s a b / p i)
  (setq i 0)
  (while (setq p (vl-string-search a s i))
    (setq s (strcat (substr s 1 p) b (substr s (+ p 1 (strlen a))))
          i (+ p (strlen b))
    )
  )
  s
)

(defun ar:xml-esc (s)
  (foreach p '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;"))
    (setq s (ar:erstat s (car p) (cdr p)))
  )
  s
)

;; De to foerste tal i et versionsnummer: "25.1s (LMS Tech)" / "20.1.1" -> "25.1" / "20.1"
(defun ar:rev-hoved (v / i p)
  (setq i 1)
  (while (and (<= i (strlen v)) (wcmatch (substr v i 1) "#,`."))
    (setq i (1+ i))
  )
  (setq v (substr v 1 (1- i)))
  (if (and (setq p (vl-string-search "." v))
           (setq p (vl-string-search "." v (1+ p))))
    (substr v 1 p)
    v
  )
)

;; --- AutoCAD's liste over palettemapper

(defun ar:dir-key (d)
  (strcase (vl-string-right-trim "\\ "
             (vl-string-left-trim " " (vl-string-translate "/" "\\" d))))
)

(defun ar:samme-dir-p (a b) (= (ar:dir-key a) (ar:dir-key b)))

(defun ar:i-sti-p (dir sti)
  (vl-some '(lambda (x) (ar:samme-dir-p x dir)) (ar:split sti ";"))
)

;; Tom mappe som AutoCAD kortvarigt peges hen paa, mens filerne skrives
(defun ar:tom-dir () (strcat (getvar "TEMPPREFIX") "AutoReload_tom_palet"))

(defun ar:acad-files ()
  (vla-get-Files (vla-get-Preferences (vlax-get-acad-object)))
)
(defun ar:tp-sti () (vla-get-ToolPalettePath (ar:acad-files)))
(defun ar:tp-saet (s) (vla-put-ToolPalettePath (ar:acad-files) s))

;; Stien uden den tomme mappe. Er AutoCAD gaaet ned midt i et skift,
;; bruges den sti der blev gemt lige foer.
(defun ar:pal-grund-sti (sti / l red)
  (setq l (ar:split sti ";"))
  (if (and (vl-some '(lambda (x) (ar:samme-dir-p x (ar:tom-dir))) l)
           (setq red (getenv "AutoReload_PaletteSti"))
           (/= red ""))
    red
    (ar:join (vl-remove-if '(lambda (x) (ar:samme-dir-p x (ar:tom-dir))) l) ";")
  )
)

;; Stien med projektmappen til sidst (hvis den ikke allerede er med)
(defun ar:pal-sti-med (sti dir)
  (cond ((ar:i-sti-p dir sti) sti)
        ((= sti "") dir)
        (T (strcat sti ";" dir))
  )
)

;; AutoCAD's egne palettelister (AcTpCatalog.atc i de andre palettemapper)
(defun ar:pal-andre-kataloger (sti / r f)
  (foreach dir (ar:split sti ";")
    (if (and (not (ar:samme-dir-p dir (ar:pal-root)))
             (not (ar:samme-dir-p dir (ar:tom-dir)))
             (setq f (findfile (strcat (vl-string-right-trim "\\/" dir)
                                       "\\AcTpCatalog.atc"))))
      (setq r (cons f r))
    )
  )
  (reverse r)
)

;; ((NAVN . id) ...) for paletterne i listerne
(defun ar:pal-kendte (katfiler / d r nm id)
  (foreach f katfiler
    (if (setq d (ar:xml-aabn f))
      (progn
        (foreach p (ar:xml-noder d "/Catalog/Palettes/Palette")
          (setq nm (ar:xml-tekst p "Properties/ItemName")
                id (ar:xml-tekst p "ItemID/@idValue"))
          (if (and nm id (/= nm ""))
            (setq r (cons (cons (strcase nm) id) r))
          )
        )
        (ar:xml-luk d)
      )
    )
  )
  (reverse r)
)

;; Fjern paletter med de samme navne fra listerne, saa der ikke kommer
;; dobbelte paletter (fx Buer og Mur importeret i haanden). Selve filerne
;; bliver liggende. Returnerer antal fjernede.
(defun ar:pal-ryd (katfiler navne / d n i nm)
  (setq n     0
        navne (mapcar 'strcase navne))
  (foreach f katfiler
    (if (setq d (ar:xml-aabn f))
      (progn
        (setq i 0)
        (foreach p (ar:xml-noder d "/Catalog/Palettes/Palette")
          (setq nm (ar:xml-tekst p "Properties/ItemName"))
          (if (and nm (member (strcase nm) navne))
            (progn
              (vlax-invoke-method (vlax-get-property p 'parentNode) 'removeChild p)
              (setq i (1+ i))
            )
          )
        )
        (if (> i 0) (vlax-invoke-method d 'save f))
        (ar:xml-luk d)
        (setq n (+ n i))
      )
    )
  )
  n
)

;; Store bogstaver paa palettenavnene i projektmappens palette-liste
(defun ar:pal-vore-navne (/ d r)
  (if (setq d (ar:xml-aabn (strcat (ar:pal-root) "\\AcTpCatalog.atc")))
    (progn
      (foreach x (ar:xml-noder d "/Catalog/Palettes/Palette/Properties/ItemName")
        (setq r (cons (strcase (vlax-get-property x 'text)) r))
      )
      (ar:xml-luk d)
    )
  )
  r
)

;; T hvis AutoCAD endnu ikke laeser paletterne rigtigt fra projektmappen:
;; mappen mangler paa listen, palette-listen mangler, eller der ligger
;; stadig paletter med samme navne i AutoCAD's egne lister.
(defun ar:pal-opsaet-p (/ sti vore)
  (setq sti (ar:tp-sti))
  (cond
    ((ar:i-sti-p (ar:tom-dir) sti) T)
    ((not (ar:i-sti-p (ar:pal-root) sti)) T)
    ((null (setq vore (ar:pal-vore-navne))) T)
    ((vl-some '(lambda (k) (member (car k) vore))
              (ar:pal-kendte (ar:pal-andre-kataloger sti)))
     T)
  )
)

;; --- selve paletterne

;; Lav <mappe>\<navn>.atc ud fra <navn>.xtp. Paletten beholder sit id (og
;; dermed sin plads og ikonstoerrelse i palettevinduet): id fra sidste gang,
;; ellers id fra en palette med samme navn i AutoCAD's liste, ellers id'et
;; i .xtp. Returnerer nil hvis det gik godt, ellers en fejltekst.
(defun ar:pal-lav (b kendte / d pal nm dep atc gl id a rev r)
  (setq r
    (cond
      ((null (setq d (ar:xml-aabn (ar:pal-xtp b))))
       "filen kan ikke laeses")
      ((null (setq pal (ar:xml-node d "/ToolPaletteExport//PaletteContent/Palette")))
       "ingen palette i filen")
      ((null (setq nm (ar:xml-tekst pal "Properties/ItemName")))
       "paletten har intet navn")
      (T
       (setq dep (ar:xml-tekst d "/ToolPaletteExport/DependentFilesPath"))
       (if (or (null dep) (= dep "")) (setq dep b))
       (setq ar:*pal-dep* (cons (cons (strcase b) dep)
                                (vl-remove-if '(lambda (x) (= (car x) (strcase b)))
                                              ar:*pal-dep*))
             atc (ar:pal-atc b))
       (if (setq gl (ar:xml-aabn atc))
         (progn
           (setq id (ar:xml-tekst gl "/Palette/ItemID/@idValue"))
           (ar:xml-luk gl)
         )
       )
       (if (null id) (setq id (cdr (assoc (strcase nm) kendte))))
       (if (null id) (setq id (ar:xml-tekst pal "ItemID/@idValue")))
       (setq a (ar:xml-ny))
       (if (equal (vlax-invoke-method a 'loadXML (vlax-get-property pal 'xml)) :vlax-true)
         (progn
           (if id
             (vlax-invoke-method (ar:xml-node a "/Palette/ItemID") 'setAttribute "idValue" id)
           )
           ;; samme hoved som de filer AutoCAD selv skriver
           (setq rev (ar:xml-tekst a "/Palette/@Revision"))
           (vlax-invoke-method (vlax-get-property a 'documentElement) 'setAttribute
             "FileRevision" (strcat (ar:rev-hoved (if rev rev (getvar "ACADVER"))) ".0"))
           (vl-mkdir (strcat (ar:pal-root) "\\" dep))
           (vlax-invoke-method a 'save atc)
           nil
         )
         "paletten kan ikke kopieres"
       )
      )
    )
  )
  (ar:xml-luk a)
  (ar:xml-luk d)
  r
)

;; Skriv projektmappens palette-liste (AcTpCatalog.atc) med alle paletter
;; der er lavet. Returnerer palettenavnene.
(defun ar:pal-katalog (/ s ver navne d id nm rev)
  (setq s   ""
        ver (ar:rev-hoved (getvar "ACADVER")))
  (foreach b (ar:pal-alle)
    (if (setq d (ar:xml-aabn (ar:pal-atc b)))
      (progn
        (setq id  (ar:xml-tekst d "/Palette/ItemID/@idValue")
              nm  (ar:xml-tekst d "/Palette/Properties/ItemName")
              rev (ar:xml-tekst d "/Palette/@Revision"))
        (ar:xml-luk d)
        (if (and id nm)
          (setq navne (cons nm navne)
                s     (strcat s
                        "<Palette Revision=\"" (if rev rev (strcat ver ".1")) "\" option=\"0\">"
                        "<ItemID idValue=\"" id "\"/>"
                        "<Url href=\"" (ar:xml-esc (strcat (ar:pal-dep b) "\\" b ".atc")) "\"/>"
                        "<Properties><ItemName>" (ar:xml-esc nm) "</ItemName>"
                        "<Images option=\"0\"/></Properties></Palette>")
          )
        )
      )
    )
  )
  (setq d (ar:xml-ny))
  (if (equal (vlax-invoke-method d 'loadXML
               (strcat "<Catalog FileRevision=\"" ver ".0\" Revision=\"" ver ".1\" option=\"0\">"
                       "<ItemID idValue=\"" ar:*pal-kat-id* "\"/>"
                       "<Properties><ItemName>Murer paletter</ItemName>"
                       "<Images option=\"0\"/></Properties>"
                       "<Source/><Tools/><Palettes>" s "</Palettes>"
                       "<Packages/><Categories/><StockTools/><Catalogs/></Catalog>"))
             :vlax-true)
    (vlax-invoke-method d 'save (strcat (ar:pal-root) "\\AcTpCatalog.atc"))
    (progn
      (princ "\nAutoReload: FEJL - palette-listen kunne ikke skrives.")
      (setq navne nil)
    )
  )
  (ar:xml-luk d)
  (reverse navne)
)

;; Skriv .atc-filerne og palette-listen, og fjern gamle kopier fra
;; AutoCAD's egne lister. Returnerer (lavet fejl antal-fjernede).
(defun ar:pal-skriv (bnavne katfiler / kendte lavet fejl r)
  (setq kendte (ar:pal-kendte katfiler))
  (foreach b bnavne
    (setq r (vl-catch-all-apply 'ar:pal-lav (list b kendte)))
    (if (vl-catch-all-error-p r) (setq r (vl-catch-all-error-message r)))
    (if r
      (setq fejl          (cons (strcat b ": " r) fejl)
            ar:*pal-fejl* (cons (cons (strcase b) (ar:stamp (ar:pal-xtp b))) ar:*pal-fejl*))
      (setq lavet (cons b lavet))
    )
  )
  (list (reverse lavet) (reverse fejl) (ar:pal-ryd katfiler (ar:pal-katalog)))
)

;; Laeg paletterne ind i AutoCAD:
;;  1) AutoCAD peges paa en tom mappe og slipper alle paletter
;;  2) .atc-filerne for bnavne laves, palette-listen skrives, og gamle kopier
;;     med samme navn fjernes fra AutoCAD's egne lister
;;  3) Palettemapperne saettes tilbage - nu med projektmappen - og AutoCAD
;;     laeser alle paletter igen
;; Returnerer (genindlaest-ok lavet fejl antal-fjernede).
(defun ar:pal-opdater (bnavne / sti grund ny tom ok r)
  (setq sti   (ar:tp-sti)
        grund (ar:pal-grund-sti sti)
        ny    (ar:pal-sti-med grund (ar:pal-root))
        tom   (ar:tom-dir))
  (vl-mkdir tom)
  (setenv "AutoReload_PaletteSti" ny)       ; redning hvis AutoCAD gaar ned midt i skiftet
  (setq ok (not (vl-catch-all-error-p (vl-catch-all-apply 'ar:tp-saet (list tom)))))
  (setq r (vl-catch-all-apply 'ar:pal-skriv (list bnavne (ar:pal-andre-kataloger grund))))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'ar:tp-saet (list ny)))
    (progn
      (vl-catch-all-apply 'ar:tp-saet (list sti))
      (setq ok nil)
    )
  )
  (setenv "AutoReload_PaletteSti" "")
  (if (vl-catch-all-error-p r)
    (list ok nil (list (strcat "paletterne kunne ikke skrives -> "
                               (vl-catch-all-error-message r))) 0)
    (cons ok r)
  )
)

;; Laeg de ventende paletter ind og fortael hvad der skete
(defun ar:pal-koer (/ b r)
  (setq b               ar:*pal-venter*
        ar:*pal-venter* nil
        ar:*pal-opsaet* nil
        r               (vl-catch-all-apply 'ar:pal-opdater (list b)))
  (if (vl-catch-all-error-p r)
    (princ (strcat "\nAutoReload: FEJL ved vaerktoejspaletterne -> "
                   (vl-catch-all-error-message r)))
    (progn
      (foreach f (caddr r) (princ (strcat "\nAutoReload: FEJL ved palette " f)))
      (if (cadr r)
        (princ (strcat "\nAutoReload: palette(r) lagt ind: " (ar:join (cadr r) ", ")))
      )
      (if (> (cadddr r) 0)
        (princ (strcat "\nAutoReload: " (itoa (cadddr r))
                       " gammel/gamle kopi(er) fjernet fra AutoCAD's egen paletteliste."))
      )
      (if (car r)
        (princ (strcat "\nAutoReload: AutoCAD har laest paletterne igen fra " (ar:pal-root)))
        (princ "\nAutoReload: AutoCAD kunne ikke laese paletterne igen nu - de kommer frem naeste gang AutoCAD startes.")
      )
    )
  )
  (princ)
)

;; Ved APPLOAD: alle paletter der mangler eller er aendret
(defun ar:pal-start (/ r)
  (setq r (vl-catch-all-apply
            '(lambda ()
               (setq ar:*pal-venter* (ar:pal-skal-laves T)
                     ar:*pal-opsaet* (ar:pal-opsaet-p)))
            nil))
  (cond
    ((vl-catch-all-error-p r)
     (princ (strcat "\nAutoReload: kan ikke tjekke vaerktoejspaletterne -> "
                    (vl-catch-all-error-message r))))
    ((not (or ar:*pal-venter* ar:*pal-opsaet*))
     (princ "\nAutoReload: vaerktoejspaletterne er opdaterede."))
    ((ar:pal-nu-ok-p) (ar:pal-koer))
    (T (princ "\nAutoReload: vaerktoejspaletterne laegges ind, naar denne kommando er faerdig."))
  )
)

;; T hvis AutoReload lige er blevet indlaest (fx af knappen "Genindlaes LISP")
(defun ar:lige-startet-p (/ now)
  (setq now (getvar "MILLISECS"))
  (and ar:*init-tid* now (< (abs (- now ar:*init-tid*)) 5000))
)

;;; ---------------------------------------------------------------- reaktorer

(defun ar:cb-cmd (reactor args)
  ;; Spring over hvis kommandoen blev kaldt inde fra et koerende LISP-program
  (if (zerop (logand (getvar "CMDACTIVE") 32))
    (ar:check nil T)
  )
)

;; Lige FOER en kommando starter: tjek altid (uden 1-sek. graense), saa
;; fx BUER koerer med den nyeste version allerede foerste gang.
;; (Paletterne venter til kommandoen er faerdig.)
(defun ar:cb-start (reactor args)
  (if (zerop (logand (getvar "CMDACTIVE") 32))
    (ar:check T nil)
  )
)

(defun ar:cb-lisp (reactor args)
  (ar:check nil (ar:pal-nu-ok-p))
)

(defun ar:remove-reactors ()
  (foreach grp (vlr-reactors :vlr-command-reactor :vlr-lisp-reactor)
    (foreach r (cdr grp)
      (if (= (vlr-data r) ar:*tag*) (vlr-remove r))
    )
  )
)

(defun ar:init (/ n)
  (ar:remove-reactors)
  (ar:read-config)
  (setq ar:*startup*   (ar:startup-files)
        ar:*stamps*    nil
        ar:*busy*      nil
        ar:*last*      nil
        ar:*pal-venter* nil
        ar:*pal-opsaet* nil
        ar:*pal-fejl*  nil
        ar:*init-tid*  (getvar "MILLISECS")
        n              0
  )
  ;; Alle overvaagede LISP-filer indlaeses, og deres version huskes
  (foreach f (ar:watched)
    (ar:load f)
    (ar:set-stamp f)
    (setq n (1+ n))
  )
  (vlr-command-reactor ar:*tag* '((:vlr-commandWillStart . ar:cb-start)
                                   (:vlr-commandEnded     . ar:cb-cmd)))
  (vlr-lisp-reactor    ar:*tag* '((:vlr-lispEnded    . ar:cb-lisp)))
  (princ (strcat "\nAutoReload aktiv - " (itoa n)
                 " fil(er) indlaest og overvaages. Skriv ARSTATUS for detaljer."))
  ;; Derefter vaerktoejspaletterne
  (ar:pal-start)
)

;;; --------------------------------------------------------------- kommandoer

(defun c:ARSTATUS (/ st r)
  (setq st (mapcar '(lambda (x) (strcase (ar:full x))) ar:*startup*))
  (princ (strcat "\n--- AutoReload: "
                 (if ar:*enabled* "SLAAET TIL" "SLAAET FRA") " ---"))
  (foreach f (ar:watched)
    (princ (strcat "\n  "
                   (cond ((member (strcase f) st) "[startup] ")
                         ((member (strcase f)
                                  (mapcar '(lambda (x) (strcase (ar:full x)))
                                          ar:*files*))
                          "[fil]     ")
                         (T "[mappe]   "))
                   f))
  )
  (foreach d ar:*folders* (princ (strcat "\n  Mappe: " d)))
  (princ (strcat "\n--- Vaerktoejspaletter fra " (ar:pal-root) " ---"))
  (foreach b (ar:pal-alle)
    (princ (strcat "\n  "
                   (cond ((not (findfile (ar:pal-atc b))) "[mangler] ")
                         ((ar:pal-gammel-p b)             "[aendret] ")
                         (T                               "[ok]      "))
                   b
                   (if (member (strcase b) (mapcar 'strcase ar:*pal-auto*))
                     "  (opdateres automatisk)"
                     "")))
  )
  (setq r (vl-catch-all-apply 'ar:tp-sti nil))
  (princ (strcat "\n  AutoCAD laeser paletter fra mappen: "
                 (cond ((vl-catch-all-error-p r) "?")
                       ((ar:i-sti-p (ar:pal-root) r) "JA")
                       (T "NEJ - skriv ARALLE"))))
  (princ)
)

(defun c:ARADD (/ f)
  (if (setq f (getfiled "Vaelg LISP-fil der skal overvaages" "" "lsp;fas;vlx" 0))
    (progn
      (setq ar:*files* (ar:uniq (append ar:*files* (list f))))
      (ar:save-config)
      (ar:load f)
      (ar:set-stamp f)
    )
  )
  (princ)
)

(defun ar:pick-folder (/ sh fo p)
  (setq sh (vlax-create-object "Shell.Application"))
  (if sh
    (progn
      (setq fo (vl-catch-all-apply 'vlax-invoke-method
                 (list sh 'BrowseForFolder 0 "Vaelg mappe der skal overvaages" 0)))
      (if (and fo (not (vl-catch-all-error-p fo)))
        (setq p (vlax-get-property (vlax-get-property fo 'Self) 'Path))
      )
      (vlax-release-object sh)
    )
  )
  p
)

(defun c:ARFOLDER (/ d)
  (if (setq d (ar:pick-folder))
    (progn
      (setq ar:*folders* (ar:uniq (append ar:*folders* (list d))))
      (ar:save-config)
      (foreach f (vl-remove-if 'ar:self-p (ar:folder-files d))
        (ar:set-stamp f)
      )
      (princ (strcat "\nMappe tilfoejet: " d))
    )
  )
  (princ)
)

(defun c:ARREMOVE (/ items i n sel)
  (setq items (append (mapcar '(lambda (x) (cons "F" x)) ar:*files*)
                      (mapcar '(lambda (x) (cons "D" x)) ar:*folders*))
        i 0)
  (if (null items)
    (princ "\nIngen egne filer/mapper at fjerne (Startup Suite styres i APPLOAD).")
    (progn
      (foreach it items
        (setq i (1+ i))
        (princ (strcat "\n  " (itoa i) ": "
                       (if (= (car it) "F") "[fil]   " "[mappe] ")
                       (cdr it)))
      )
      (setq n (getint "\nNummer der skal fjernes: "))
      (if (and n (setq sel (nth (1- n) items)))
        (progn
          (if (= (car sel) "F")
            (setq ar:*files*   (vl-remove (cdr sel) ar:*files*))
            (setq ar:*folders* (vl-remove (cdr sel) ar:*folders*))
          )
          (ar:save-config)
          (princ (strcat "\nFjernet: " (cdr sel)))
        )
      )
    )
  )
  (princ)
)

(defun c:ARNOW ()
  (ar:read-config)
  (setq ar:*startup* (ar:startup-files))
  (ar:check T T)
  (princ "\nAutoReload: tjek udfoert.")
  (princ)
)

(defun c:ARTOGGLE ()
  (setq ar:*enabled* (not ar:*enabled*))
  (princ (strcat "\nAutoReload er nu " (if ar:*enabled* "SLAAET TIL" "SLAAET FRA")))
  (princ)
)

;; Knappen "Genindlaes" i paletten Mur: indlaes alle filer igen med det samme,
;; laeg de paletter ind der mangler eller er aendret, og faa AutoCAD til at
;; laese paletterne igen
(defun c:ARALLE (/ n r)
  (ar:read-config)
  (setq ar:*startup* (ar:startup-files)
        n 0)
  (foreach f (ar:watched)
    (ar:load f)
    (ar:set-stamp f)
    (setq n (1+ n))
  )
  (princ (strcat "\nAutoReload: " (itoa n)
                 " fil(er) genindlaest - de overvaages nu, og nye versioner indlaeses selv."))
  (setq ar:*pal-fejl* nil
        r (vl-catch-all-apply
            '(lambda ()
               (setq ar:*pal-venter* (ar:pal-skal-laves T))
               ;; lige indlaest af knappen -> paletterne er netop lagt ind
               (if (not (ar:lige-startet-p)) (setq ar:*pal-opsaet* T)))
            nil))
  (cond
    ((vl-catch-all-error-p r)
     (princ (strcat "\nAutoReload: kan ikke tjekke vaerktoejspaletterne -> "
                    (vl-catch-all-error-message r))))
    ((or ar:*pal-venter* ar:*pal-opsaet*) (ar:pal-koer))
  )
  (princ)
)

;;; ------------------------------------------------------------------- start
(ar:init)
(princ)
