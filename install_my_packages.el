
;; List of packages to install if not present
(setq my-package-list
      '(ac-math
        auctex
        auto-complete
        auto-complete-auctex
        auto-complete-c-headers
        cdlatex
        company
        conda
        csv-mode
        ein
        ebib
        elpy
        helm
        helm-core
        highlight-indentation
        magit
        math-symbol-lists
        matlab-mode
        parsebib
        web-mode
        wgrep
        magit
        magit-popup))

;; Download and make list of packages available if needed
;; (This will be needed in a fresh install of emacs)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages in list, if not already installes
(dolist (pkg my-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
