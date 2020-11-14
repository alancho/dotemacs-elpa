(setq user-full-name "Alan Severini")
(setq user-mail-address "severini.alan@gmail.com")

(require 'cl)

(load "package")
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; Y estos son mis paquetes
(defvar alancho/packages '(color-theme
			   color-theme-tango
			   ag
			   anaconda-mode
			   auctex
			   auctex-latexmk
			   auto-complete
			   company-math
			   company-anaconda
			   conda
			   ctable
			   dash
			   deferred
			   deft
			   elpy
			   epc
			   epl
			   ess
			   ess-view
			   exec-path-from-shell
			   expand-region
			   idle-highlight-mode
			   ido-ubiquitous
			   ivy-bibtex
			   latex-extra
			   ;; lsp-python-ms
			   magit
			   markdown-mode
			   pandoc-mode
			   paredit
			   pkg-info
			   popup
			   polymode
			   poly-markdown
			   poly-R
			   projectile
			   request
			   s
			   smex
			   solarized-theme
			   stan-mode
			   shell-pop
			   synonyms
			   wc-mode
			   wc-goal-mode
			   websocket
			   window-margin
			   writegood-mode
			   writeroom-mode
			   yaml-mode
			   yasnippet
			   use-package
			   avy
			   counsel
			   swiper
			   ivy
			   ivy-hydra
			   wgrep
			   zenburn-theme)
  "Default packages")

(defun alancho/packages-installed-p ()
  (loop for pkg in alancho/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (alancho/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg alancho/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Exec-path-from-shell
;; ========================================================
;; (require 'exec-path-from-shell) ;; if not using the ELPA package
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

;; Start in *scratch*
;; ========================================================
(setq inhibit-startup-message t)

;; AucTeX
;; ========================================================
;; (add-hook 'LaTeX-mode-hook '(lambda ()
;; 			      (if (string-match "\\.Rnw\\'" buffer-file-name)
;; 				  (setq fill-column 80))))

(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
(setq-default TeX-master nil)
(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)
(setq TeX-electric-sub-and-superscript t)
(add-hook 'LaTeX-mode-hook
	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
			  (cons "$" "$"))))

(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
(add-hook 'TeX-mode-hook 'visual-line-mode)
;; (add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'LaTeX-mode-hook 'writeroom-mode)
(setq font-latex-fontify-script nil)

(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(autoload 'ivy-bibtex "ivy-bibtex" "" t)

(setq bibtex-completion-bibliography
      '(;; "/home/alancho/Dropbox/consumo_agua_selvi_2019_06_07/compartidos_github/biblio.bib"
	;; "/home/alancho/Dropbox/POSGRADO/TESIS/compartidos_para_git/biblio.bib"
	;; "/home/alancho/Dropbox/paper_rooting-depth-screening/02_rewriting_Rmd/roots.bib"
	;; "/home/alancho/Dropbox/cic_2020/cic2020.bib"
	"/home/alancho/Dropbox/cronosoja/paper/cronosoja.bib"
	))

;; (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(global-set-key (kbd "<f8>") 'ivy-bibtex)

;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (call-process "evince" nil 0 nil fpath)))

(setq bibtex-completion-cite-prompt-for-optional-arguments nil)
(setq bibtex-completion-cite-commands '("citep" "citet" "citeauthor" "citeyear"))

;; (ivy-set-actions
;;  'ivy-bibtex
;;  '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
;;    ("i" ivy-bibtex-insert-citation "Edit notes")))

;; Default directory
;; ========================================================
;; (setq default-directory "~/Dropbox/")

;; Python
;; ========================================================

;; (elpy-enable)

;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i")

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; (define-key elpy-mode-map [(shift return)] 'elpy-shell-send-region-or-buffer)
;; (define-key elpy-mode-map [(C-return)] 'elpy-company-backend)

;; Encryption
;; ========================================================
;; (require 'epa-file)
;; (epa-file-enable)

;; ESS
;; ========================================================
(defun forbid-vertical-split ()
  "Only permit horizontal window splits."
  (setq-local split-height-threshold nil)
  (setq-local split-width-threshold 0))

(require 'ess-site)
(require 'ess-view)

(add-hook 'ess-mode-hook
          'forbid-vertical-split)

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;; (setq ess-eval-visibly-p t)

(define-key
  ess-r-mode-map "_" #'ess-insert-assign)

(define-key
  inferior-ess-r-mode-map "_" #'ess-insert-assign)

(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'ess-eval-region-or-function-or-paragraph-and-step)))
(add-hook 'Rnw-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'ess-eval-region-or-function-or-paragraph-and-step)))
(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))

;; This is to be quicker when writing the %>% operator from dplyr
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent)
  )

(define-key ess-mode-map (kbd "C-<return>") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-<return>") 'then_R_operator)

(defun then_ggplot_plus ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "+")
  (reindent-then-newline-and-indent))

(define-key ess-mode-map (kbd "M-<return>") 'then_ggplot_plus)
(define-key inferior-ess-mode-map (kbd "M-<return>") 'then_ggplot_plus)

;; RStudio indentation!
(setq ess-default-style 'RStudio)

(setq fill-column 72)
(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)

;; Define key-bindings for calling 'spin';
;; 'M-n n' - spin the current buffer
(define-key ess-mode-map "\M-nn" 'ess-swv-render)

(defun ess-swv-render ()
  "Run spin on the current .R file."
  (interactive)
  (ess-swv-run-in-R "rmarkdown::render"))


;; expand region
;; ========================================================
(global-set-key (kbd "C-=") 'er/expand-region)

;; This are my faces, fonts, etcetera
;; ========================================================
(set-face-attribute 'default nil :height 105 :family "monospace" :weight 'normal :width 'normal)

;; I don't need to highlight current line
;; ========================================================
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; My magit setup
;; ========================================================
(global-set-key (kbd "C-x g") 'magit-status)

;; Recent mode
;; ========================================================
(recentf-mode t)
(setq recentf-max-saved-items 100)
(add-to-list 'recentf-exclude "\\.ido\\.last")
(add-to-list 'recentf-exclude "bookmark")
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))

;; This is my color theme
;; ========================================================
(load-theme 'tango-dark)

;; This is how I prefer text to behave
;; ========================================================
(display-time-mode 1)
(delete-selection-mode 1)
(setq shift-select-mode t)
(transient-mark-mode t)
(setq-default line-spacing 1)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(visual-line-mode 1)
(setq visible-bell t)
;; (setq ispell-program-name "aspell")
(prefer-coding-system 'utf-8)
(setq column-number-mode t)
;; (auto-fill-mode -1)

(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; I've found that it's better and a best practice not to wrap lines
;; when editing in LaTeX
(set-default 'truncate-lines t)
;; That said, the following must be deactivated
;; (global-visual-line-mode 1)

;; Display line numbers in margin
(global-linum-mode 1)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(define-key global-map "\C-\M-Q" 'unfill-region)

;; Smooth movement of buffer when scrolling or moving with arrow keys
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Uniquify
;; ========================================================
;; (require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; Slightly more debatable
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Yasnippets
;; ========================================================
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-.") 'yas-expand)

;; I need smex
;; ========================================================
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)

;; To answer quicker
;; ========================================================
(defalias 'yes-or-no-p 'y-or-n-p)

;; This is for dired mode to omit extensions I don't want to see
(require 'dired-x)
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; I use to enable ido mode in org-mode. Because I don't use org
;; anymore I need to define how to use ido next
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)

(setq org-support-shift-select t)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Para editar los yaml a ingresar en R scripts cuando render
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "<return>" 'newline-and-indent)))
;; Para ver las lineas vacias tambien
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Backup files no sirven para nada y nunca los he usado
(setq make-backup-files nil)

;; Pandoc mode está bueno porque pese a que no uso sus keybindings me
;; permite tener syntax highlighting específico para pandoc
;; (e.g. citations)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-reftex)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add `recentf-mode' and bookmarks to `ivy-switch-buffer'.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 15)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((ivy-switch-buffer . ivy--regex-fuzzy)
          (counsel-find-file . ivy--regex-plus)
          (counsel-locate . ivy--regex-plus)
          (counsel-M-x . ivy--regex-plus)
          (t   . ivy--regex-plus))))

;; Otra oportunidad a counsel
(use-package counsel
  :demand t
  :bind*
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("M-x" . counsel-M-x)
   ;; ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ;; ("C-c d" . counsel-dired-jump)
   ;; ("C-c j" . counsel-git-grep)
   ;; ("C-x C-d" . counsel-ag)
   ("C-x C-d" . counsel-rg)
   ("C-x C-r" . counsel-recentf)
   ("C-x C-l" . counsel-locate)
   ("M-y" . counsel-yank-pop))
  :config
  (ivy-mode 1)
  (setq ivy-height 10)
  ;; (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
  )

;; Y otra oportunidad a swiper
(use-package swiper
  :bind*
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-M-s" . swiper-all))
  :bind
  (:map read-expression-map
        ("C-r" . counsel-expression-history)))

;; Y vamos a probar avy
(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2))
  :config
  (setq avy-keys '(?h ?t ?n ?s)))

;; Para que counsel-ag sólo busque R, python, y markdown
;; (setq counsel-ag-base-command "ag --nocolor --nogroup --r %s /home/alancho/")

(setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s . -tr")

;; Para evitar ciertos directorios en counsel find file
(setq ivy-sort-file-function 'string-lessp)
(setq ivy-extra-directories nil)
(setq counsel-find-file-ignore-regexp (regexp-opt '(".dropbox")))


;; Para que no alerte cuando se llega al comienzo o final de un buffer
(setq ring-bell-function #'ignore)


;; Disable overwrite-mode for ever!
(define-key global-map [(insert)] nil)

;; Para que los archivos .apsim se abran en xml-mode
(add-to-list 'auto-mode-alist '("\\.apsim$" . nxml-mode))

(setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")

(require 'polymode)
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
 (setq polymode-display-process-buffers nil
        polymode-exporter-output-file-format "%s")

;; Para que las oraciones con punto seguido sean reconocidas con un solo espacio
(setq sentence-end-double-space nil)

(setq writeroom-width 120)

;; completion key bindings
(setq tab-always-indent 'complete)
;; (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
;; (define-key company-mode-map (kbd "C-M-i") 'company-complete)
;; (define-key company-mode-map (kbd "C-M-S-i") 'counsel-company)
(put 'downcase-region 'disabled nil)

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3")))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" default)))
 '(package-selected-packages
   (quote
    (zotxt zenburn-theme yaml-mode writeroom-mode writegood-mode window-margin wgrep websocket wc-mode wc-goal-mode use-package synonyms stan-mode solarized-theme smex poly-R pkg-info paredit pandoc-mode org-journal org-gcal org-cliplink magit latex-extra ivy-hydra ivy-bibtex ido-ubiquitous idle-highlight-mode flyspell-correct-ivy expand-region exec-path-from-shell ess epc elpy deft counsel conda company-math color-theme-tango avy auto-complete auctex-latexmk))))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  ;; (setq projectile-project-search-path '("~"))
  (projectile-mode))

(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; ;; A ver cómo anda python lsp
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-italic-face ((t (:inherit nil)))))
