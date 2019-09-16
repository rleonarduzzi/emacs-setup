
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     t)

(setq package-enable-at-startup nil)
(package-initialize)

;; Install my list of packages
(load (concat (file-name-directory load-file-name)
       "install_my_packages.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO-COMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'auto-complete-config)
;; Remove python-mode from the list of modes where AC is enabled by default
;; because it interferes with elpy's autocompletion
(delq 'python-mode ac-modes)
(ac-config-default)
; Hago que se active AC con el modo matlab también. Parece que no se puede hacer
; con hooks porque AC lleva una lista interna de modos en los que se activa.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURACIONES VARIAS
;;;;;;;;;;;;;;;;;;;;;;;;;

; Saco la barra de herramientas para tener más espacio en la pantalla:
(tool-bar-mode 0)

; Matched parenthesis highlighting by default:
(show-paren-mode 1)

; mostrar los números de línea:
(global-linum-mode 1)

; mostrar números de columna:
(setq column-number-mode t)

; Current line highlighting:
;(global-hl-line-mode 1)

; Column fill a los 81 caracteres (líneas de 80):
(setq default-fill-column 80)

;; Corrijo problemas con caracteres raros en el modo shell
(ansi-color-for-comint-mode-on)

;; Saco el sonido irritante
(setq ring-bell-function 'ignore)

;; Espacios en vez de tabulaciones:
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODO C++
; C-c C-s para compilar
(add-hook 'C++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-s") 'compile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODO C
; C-c C-s para compilar
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-s") 'compile)))

;; Cambio modo de indentación
(setq c-default-style "linux"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings propios:
; C-tab para pasar de una ventana a otra (sinónimo de  C-x o)
(global-set-key (kbd "<C-tab>") 'other-window)


; S-C-y para clipboard-yank
; Aquamacs has some cua thing here, so I unset it first.
;(global-unset-key (kbd "C-S-y"))
;(global-set-key (kbd "C-S-y") 'clipboard-yank)

(global-set-key (kbd "C-x g") 'magit-status)

;; El teclado US-international de mac no tiene el caracter "¿"
;; Redefino la tecla "option_r /", asignada a "÷", para que inserte "¿"
(global-set-key "÷" (lambda ()
                      (interactive)
                      (insert "¿")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED MODE

;; Display sizes in human-readable format
(setq dired-listing-switches "-alh")

;; Activate line highlightinh in dired buffers
(add-hook 'dired-mode-hook 'hl-line-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODO RECENTF
(require 'recentf)
;; Disable auto-cleanup fetaure before starting recentf
;; auto-cleanup doesn't play well with tramp: if there are tramp files in
;; the recentf list it attempts to stat them, with delays and sometimes errors
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODO PYTHON

(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(require 'conda)
;; interactive shell support:
(conda-env-initialize-interactive-shells)
;; eshell support:
;(conda-env-initialize-eshell)
;; auto-activation:
(conda-env-autoactivate-mode t)

;; in "Virtual Envs" menu, show me conda envs
(setenv "WORKON_HOME" "~/anaconda3/envs")

;; Use ein to work on ipython notebooks
(require 'ein)

;; Activación de autofill
;(add-hook 'python-mode-hook 'auto-fill-mode)

; Inmediately show ac menu
;(add-hook 'python-mode-hook
;	  (lambda () (setq ac-show-menu-immediately-on-auto-complete t)))

;(setq python-shell-interpreter "ipython"
;      python-shell-interpreter-args "-i --simple-prompt")
;(setq python-shell-interpreter "python3"
;      python-shell-interpreter-args "-i")

;(require 'jedi)
;; Hook up to auto-complete
;(add-to-list 'ac-sources 'ac-source-jedi-direct)
;; Enable jedi in python mode
;(add-hook 'python-mode-hook 'jedi:setup)

;; Don't let tooltip show up automatically
;(setq jedi:get-in-function-call-delay 10000000)
;; Start completion at method dot
;(setq jedi:complete-on-dot t)
;; Use custom keybinds
;(add-hook 'python-mode-hook
;	  (lambda ()
;	    (local-set-key (kbd "M-.") 'jedi:goto-definition)
;	    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
;	    (local-set-key (kbd "M-?") 'jedi:show-doc)
;	    (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))

;; Change these environment variables that otherwise default to ascii in MacOs X,
;; and prevent execution of scripts with utf-8 characters
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; Small routine to write list of variables to be saved with numpy.savez
(defun python-dict-write-var (var)
  "write new variable"
  (interactive "sEnter variable:")
  (insert (concat "'"  var "': " var ", ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab-emacs
(setq matlab-shell-command "/Applications/MATLAB_R2017a.app/bin/matlab")
(setq matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
(matlab-cedet-setup)
(add-to-list 'ac-modes 'matlab-mode)
(setq matlab-change-current-directory t)
(setq matlab-comment-region-s "% ")
(setq matlab-shell-echoes t)
(setq matlab-handle-simulink nil)

; Activate company for completions in matlab shell
(add-hook 'matlab-shell-hook 'company-mode)

;; Override matlab-mode's C-TAB:
;(define-key matlab-shell-mode-map (kbd "<C-tab>") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURACION PARA ARCHIVOS TEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uso de pdflatex por defecto
(setq TeX-PDF-mode t)

;; Don't ask before saving file
(setq TeX-save-query nil)

;; Activación de RefTex por defecto
(add-hook 'LaTeX-mode-hook 'reftex-mode)

;; Activación de autofill
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'Latex-mode-hook (lambda () (set-fill-column 100)))

;; Activación de autocomplete
;(add-hook 'LaTeX-mode-hook 'auto-complete-mode)
;(require 'ac-math)
;(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
; (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
;   (setq ac-sources
;         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;                 ac-sources))
;   )
;(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
;(global-auto-complete-mode t)
;
;(setq ac-math-unicode-in-math-p t)
;
;(require 'auto-complete-auctex)
;; Lo comento porque interfiere con cdlatex. Debería hacer que cdlatex use una
;; tecla distinta a tab.



;; Verificación de ortografía mientras se escribe
(add-hook 'LaTeX-mode-hook 'flyspell-mode)


;; (add-to-list 'load-path "~/.emacs.d/cdlatex")
;;(require 'cdlatex)
 (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
; cdlatex usa la tabulación, por lo que la pierdo para indentar.

;; Add cdlatex commands for beamer
(setq cdlatex-command-alist
      '(("frm" "Insert a FRAME environment template"   "" cdlatex-environment ("frame") t nil)
        ("blo" "Insert a FRAME environment template" "" cdlatex-environment ("block") t nil)))
(setq cdlatex-env-alist
      '(("frame" "\\begin{frame}[c]\n\\frametitle{?}\n\n\\end{frame}\n" nil)
        ("block" "\\begin{block}{?}\n\n\\end{block}\n" nil)))

;; Add accented vowels because Mac OS seems to interpret the `
;; before cdlatex, and as a results sends accented characters
(setq cdlatex-math-symbol-alist
      '((?à ("\\alpha" ))
        (?À ("\\Alpha" ))
        (?è ("\\epsilon"        "\\varepsilon"    "\\exp"))
        (?È ("\\exists"         ""                "\\ln"))
        (?ì ("\\in"             "\\imath"))
        (?Ì (""                 "\\Im"))
        (?ò ("\\omega"          ))
        (?Ò ("\\Omega"          "\\mho"))
        (?ù ("\\upsilon"        ))
        (?Ù ("\\Upsilon"        ))
        ))

;; Redefine cdlatex-sub-superscript so that it doesn't annoyingly add the dolar
;; signs on every _
(eval-after-load "cdlatex"
  '(defun cdlatex-sub-superscript ()
     "Insert ^{} or _{} unless the number of backslashes before point is odd. When not in LaTeX math environment, _{} and ^{} will have dollars."
     (interactive)
     (if (cdlatex-number-of-backslashes-is-odd)
         ;; Quoted
         (insert (event-basic-type last-command-event))
       ;; Check if we need to switch to math mode
       ;; RFL COMMENT (if (not (texmathp)) (cdlatex-dollar))
       (if (string= (buffer-substring (max (point-min) (- (point) 2)) (point))
                    (concat (char-to-string (event-basic-type last-command-event))
			                "{"))
           ;; We are at the start of a sub/suberscript.  Allow a__{b} and a^^{b}
           ;; This is an undocumented feature, please keep it in.  It supports
           ;; a special notation which can be used for upright sub- and
           ;; superscripts.
           (progn
             (backward-char 1)
             (insert (event-basic-type last-command-event))
             (forward-char 1))
         ;; Insert the normal template.
         (insert (event-basic-type last-command-event))
         ;; RFL: INSERT THE {} ONLY IN MATH MODE:
         (when (texmathp)
           (insert "{}")
           (forward-char -1))))))

; cdlatex modifica la indentación y redefine tab, así que asigno a
; S-TAB una función para indentar y pasar a la línea siguiente.
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<backtab>")
			   (lambda ()
			     (interactive)
			     (indent-according-to-mode)
			     (next-line)))))

;; uso de Skim como visor por defecto. Sacado de:
;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;;
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;(add-hook 'LaTeX-mode-hook (lambda ()
;			     (push
;			      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;				:help "Run latexmk on file")
;			      TeX-command-list)))
;(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))


;; Enable synctex and use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq LaTeX-command "latex -synctex=1")
(add-hook 'LaTeX-mode-hook 'server-start)
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline  -g %n %o %b")))

;; Función para compilar sólo un frame en beamer:
(defun compilar-frame ()
  "Compila el frame de beamer actual."
  (interactive)
  (save-excursion
    (let (beg)
      (search-backward "\\begin{frame}")
      (setq beg (point))
      ;(LaTeX-find-matching-end)
      (search-forward "\\end{frame}")
      (TeX-pin-region beg (point))
      ;(let (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX" )))
    (TeX-command-region))));)

(add-hook 'LaTeX-mode-hook
      (lambda () (local-set-key (kbd "C-c C-v") 'compilar-frame)))

;; En este aquamacs no me está andando C-c ] para cerrar entornos.
;; Lo pongo en C-c +
(add-hook 'LaTeX-mode-hook
	  (lambda () (local-set-key (kbd "C-c +") 'LaTeX-close-environment)))

; Función para formatear tablas, ver man tfmt
(defun tfmt-region (start end)
  "Filter region through tfmt."
  (interactive "r")
  (let ((cmd (read-input "filter region: " "tfmt")))
    (call-process-region start end shell-file-name t t nil "-c" cmd)))

;; Función para aplicar un comando de latex a una selección o palabra
(defun tex-wrap-command (command)
  "Wraps the text in the region or the current word with the given command"
  (interactive "sEnter command:")
  ;; Find region limits, move and insert:
  (let ((reg-ini (if mark-active
		     (region-beginning)
		   (funcall (lambda () (backward-word) (point)))))
	(reg-fin (if mark-active
		     (region-end)
		   (funcall (lambda () (forward-word) (point))))))
    ;; Insert at the end first, to avoid changing reg-fin after insertion at
    ;; the beginning
    (goto-char reg-fin)
    (insert "}")
    (goto-char reg-ini)
    (insert (if (char-equal (aref command 0) ?\\) "" "\\") command "{")
    ;; Leave point after closing paren
    (search-forward "}")))

; La especializo para textcolor y le doy una tecla
(defun tex-wrap-textcolor (color)
  "Wraps text in region with textcolor primitive."
  (interactive "sEnter color:")
  (tex-wrap-command (concat "textcolor{" color "}")))

(add-hook 'LaTeX-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-g") 'tex-wrap-textcolor)))

; Usa bibexport to create a new bibfile with only the bibentries used in the current buffer.
(defun extract-bib (filename)
  "Extract used bib entries from a file.
Creates file 'filename' with the bib entries that where used in a tex
file associated to the current buffer.
Uses bibexport for the extraction"
  (interactive "sEnter name for new bib file: ")
  (shell-command
   (concat "bibexport -o "
           filename " "
           (substring (buffer-name) 0 -3)
           "aux")))

;; Function to remove all instances of a command
;; TODO: add possibility of operating only on region
(defun remove-latex-command (comm-name)
  "Removes all instances of the LaTeX command 'comm'"
  (interactive "sEnter command name:")
  (save-excursion
    (let ((comm-regexp (concat (unless (string-match "\\\\" comm-name)
                                 (concat "\\\\" comm-name ))
                               "\\b")))
      (print comm-regexp)
      (while  (search-forward-regexp comm-regexp (point-max) t)   ; Now point is  before opening brace
        ;;What happens if there are spaces between the command and the brace??
        (let ((comm-end (point)))
          (search-backward "\\")
          (delete-region (point) comm-end))  ; delete the command, point is befor opening brace
        (let ((beg (point)))
          (forward-sexp)  ; move to matching brace
          (backward-delete-char 1)  ; delete closing brace
          (goto-char beg)  ; move to opening brace
          (delete-char 1)  ; delete opening brace
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función para imprimir un buffer a pdf:
; Sacada de StackOverflow:
;  http://stackoverflow.com/questions/7362625/word-wrap-for-emacs-print-buffer-to-pdf
; Parece que tiene algunos problemas con el word wrap
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "~/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 ~/tmp/tmp.ps ~/" (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm ~/tmp/tmp.ps")
  (message (concat "Saved to:  ~/" (buffer-name) ".pdf"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Función para usar el formateador de tablas tfmt en la región
; sacado de http://staff.science.uva.nl/~dominik/
;(defun tfmt-region (start end)
;  "Filter region through tfmt."
;  (interactive "r")
;  (let ((cmd (read-input "filter region: " "tfmt")))
;    (call-process-region start end shell-file-name t t nil "-c" cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uso de ido-mode por defecto
;; (ahora reemplazado por helm)
;(require 'ido)
;(ido-mode t)

;;----------------------------------------------
; Función para contar palabras, sacada de emacs wiki
(defun count-words (&optional ini fin)
  "Cuenta la cantidad de palabras en la region, si está activa, o en el buffer."
  (interactive)
  (save-excursion
    (let ((cuenta 0)
	  (ini (if mark-active (region-beginning) (point-min)))
	  (fin (if mark-active (region-end) (point-max))))
      (goto-char ini)
      (while (< (point) fin)
	(forward-word 1)
	(setq cuenta (1+ cuenta)))
      (message "La región tiene %d palabras" cuenta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pongo meta en la tecla de command también, porque me está volviendo loco.
;; Estoy acostumbrado a que esté al lado de la barra.
;;
;; Ver si después extraño a command por las teclas propias de aquamacs y lo
;; tengo que volver a poner en otro lugar.
;;
;; THIS HAS BEEN FINALLY SOLVED WITH KARABINER
;(setq mac-command-modifier 'meta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

;(require 'helm)
(require 'helm-config)
(helm-mode 1)

; Change default prefix because the default is too close to C-x C-c, which quits emacs.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)  ; This unbinds find-file-read-only
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)

; Change C-z to C-RET which is more confortable.
; This unbinds helm-cr-empty-string
(define-key helm-map (kbd "<C-return>") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "<C-return>") 'helm-execute-persistent-action)



(setq helm-move-to-line-cycle-in-source     nil ; move to end or beginning of source when reaching top or bottom of source.
      helm-M-x-fuzzy-match                  t ;  fuzzy matching for helm-M-x
      helm-mini-fuzzy-match                 t
      helm-recentf-fuzzy-match              t
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

;; Helm find-files does not like the custom multi-hops syntax of tramp (concatenation with "|")
;; Add the proxies to tramp's proxy list instead.

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '("heniochus" "roberto" "/ssh:rleonard@ssh.ens-lyon.fr:"))
(add-to-list 'tramp-default-proxies-alist
             '("digitale" "rleonarduzzi" "/ssh:digitale-ext:"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wgrep

(setq wgrep-auto-save-buffer t)

; Define keys to start and finish wgrep by similarity with wdired
(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For some reason Aquamacs is not loading the right path environment variable
;; from bash. While I find what's going on, I manually add ~/bin
(setenv "PATH" (concat "/Library/TeX/texbin/:/Users/roberto/bin:/usr/texbin/:" (getenv "PATH")))
(setq exec-path (append exec-path (list "/usr/texbin" "/Users/roberto/bin")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("tabular")
     ("tabular*")
     ("align")
     ("align*")
     ("array")
     ("eqnarray")
     ("eqnarray*")
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing"))))
 '(conda-anaconda-home "~/anaconda3")
 '(csv-separators (quote ("," "	" ";")))
 '(ispell-program-name "aspell")
 '(matlab-change-current-directory t)
 '(matlab-shell-echoes t)
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote none))
 '(ns-right-command-modifier (quote super))
 '(package-selected-packages
   (quote
    (csv-mode ein jedi wgrep web-mode matlab-mode magit helm elpy ebib conda cdlatex auto-complete-c-headers auto-complete-auctex auctex ac-math)))
 '(semantic-matlab-dependency-system-include-path
   (quote
    ("/Applications/MATLAB_R2017a.app/toolbox/matlab/funfun" "/Applications/MATLAB_R2017a.app/toolbox/matlab/general"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
