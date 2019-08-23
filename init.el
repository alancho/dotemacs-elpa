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

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))

;; Y estos son mis paquetes
(defvar alancho/packages '(;autopair
			   ;; arduino-mode
			   color-theme
			   color-theme-tango
			   auctex
			   auctex-latexmk
			   auto-complete
			   company-math
			   ctable
			   dash
			   deferred
			   deft
			   elpy
			   epc
			   epl
			   ess
			   ;; esup
			   exec-path-from-shell
			   expand-region
			   ;; helm
			   idle-highlight-mode
			   ido-ubiquitous
			   ivy-bibtex
			   latex-extra
			   magit
			   markdown-mode
			   org
			   org-cliplink
			   org-gcal
			   org-journal
			   pandoc-mode
			   paredit
			   pkg-info
			   popup
			   polymode
			   poly-markdown
			   poly-R
			   request
			   s
			   smex
			   solarized-theme
			   stan-mode
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
			   zenburn-theme
			   zotxt)
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
(add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'LaTeX-mode-hook 'writeroom-mode)
(setq font-latex-fontify-script nil)

;; (setq reftex-cite-prompt-optional-args t)
;; (setq reftex-plug-into-AUCTeX t)
;; ;; (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
;; (setq reftex-default-bibliography '("/home/alancho/Dropbox/Papers/bib/library.bib"))
;; (setq reftex-cite-format; Get ReTeX with biblatex, see https://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
;;       '((?t . "\\textcite[]{%l}")
;; 	(?a . "\\autocite[]{%l}")
;; 	(?c . "\\cite[]{%l}")
;; 	(?s . "\\smartcite[]{%l}")
;; 	(?f . "\\footcite[]{%l}")
;; 	(?n . "\\nocite{%l}")
;; 	(?b . "\\blockcquote[]{%l}{}")))

(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:

;; (setq ivy-re-builders-alist
;;       '((ivy-bibtex . ivy--regex-ignore-order)
;;         (t . ivy--regex-plus)))

(setq bibtex-completion-bibliography
      '("/home/alancho/Dropbox/consumo_agua_selvi_2019_06_07/compartidos_github/biblio.bib"
	"/home/alancho/Dropbox/POSGRADO/TESIS/compartidos_para_git/biblio.bib"
	"/home/alancho/Dropbox/convocatoria_cofinanciada_2019/proyecto/biblio.bib"))

;; (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

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
;; No estoy usando Python ni elpy, y de acuerdo a esup este ultimo
;; esta usando bastante tiempo del startup. Por ende, lo desactivo:
(elpy-enable)
;; (elpy-use-ipython)
;; (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")
;; (setq elpy-rpc-backend "jedi")
;; (setq ac-modes (remove 'python-mode ac-modes))
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

(add-hook 'ess-mode-hook
          'forbid-vertical-split)

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(define-key
  ess-r-mode-map "_" #'ess-insert-assign)

(define-key
  inferior-ess-r-mode-map "_" #'ess-insert-assign)

;; (eval-after-load "ess-mode"
;;   '(progn
;;      (define-key ess-mode-map [(control return)] nil)
;;      (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step))
;;   )

;; (add-hook 'ess-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key [(shift return)] 'ess-eval-region-or-line-and-step)))
;; (add-hook 'Rnw-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key [(shift return)] 'ess-eval-region-or-line-and-step)))
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
  (reindent-then-newline-and-indent))

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

;; (global-company-mode 1)

;; (add-hook 'ess-mode-hook
;;           (defun my-R-mode-hook ()
;;             (company-mode)
;;             (local-set-key (kbd "TAB") 'company-indent-or-complete-common)))

;; (setq company-selection-wrap-around t
;;       company-show-numbers 1
;;       company-tooltip-align-annotations t
;;       company-idle-delay 10
;;       company-minimum-prefix-length 2
;;       company-tooltip-limit 10)

;; (define-key company-active-map [return] nil)
;; (define-key company-active-map [tab] 'company-complete-selection)
;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)

;; (use-package company-math)
;; (use-package company
;;   :config
;;   (progn
;;     (setq company-tooltip-limit 20
;;           company-idle-delay 0.5
;;           company-show-numbers t
;;           company-tooltip-align-annotations t)
;;     (global-company-mode))
;;   :bind (("<tab>" . company-indent-or-complete-common)))

;; expand region
;; ========================================================
;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; This are my faces, fonts, etcetera
;; ========================================================
;; (set-face-attribute 'default nil :family "Ubuntu Mono" :height 110 :weight 'normal)
;; (set-face-attribute 'default nil :height 125 :family "Inconsolata")
(set-face-attribute 'default nil :height 110 :family "Monaco")
;; (set-face-attribute 'default nil :height 110 :family "Dejavu Sans Mono")


;; This is to unfill paragraphs
;; ========================================================
(defun my-fill-latex-paragraph ()
  "Fill the current paragraph, separating sentences w/ a newline.
AUCTeX's latex.el reimplements the fill functions and is *very*
convoluted. We use part of it --- skip comment par we are in."
  (interactive)
  (if (save-excursion
	(beginning-of-line) (looking-at TeX-comment-start-regexp))
      (TeX-comment-forward)
    (let ((to (progn
		(LaTeX-forward-paragraph)
		(point)))
	  (from (progn
		  (LaTeX-backward-paragraph)
		  (point)))
	  (to-marker (make-marker)))
      (set-marker to-marker to)
      (while (< from (marker-position to-marker))
	(forward-sentence)
	(setq tmp-end (point))
	(LaTeX-fill-region-as-paragraph from tmp-end)
	(setq from (point))
	(unless (bolp)
	  (LaTeX-newline))))))

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "M-q") 'my-fill-latex-paragraph))

;; I don't need to highlight current line
;; ========================================================
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; When C-x C-f I don't want to see this extensions
;; ========================================================
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(completion-ignored-extensions
;;    (quote
;;     (".docx" ".xlsx" ".wmf" ".doc" ".xls" ".csv" ".bib" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".odt" ".pptx" ".ppt" ".txt" ".dat")))
;;  '(elpy-modules
;;    (quote
;;     (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
;;  '(package-selected-packages
;;    (quote
;;     (stan-mode yaml-mode writeroom-mode window-margin websocket use-package synonyms smex request popup paredit pandoc-mode org-journal markdown-mode magit ivy-hydra ido-ubiquitous idle-highlight-mode expand-region exec-path-from-shell ess epc elpy deft counsel-projectile company-math color-theme-tango cl-generic avy autopair auctex ag)))
;;  '(send-mail-function (quote smtpmail-send-it)))

;; My magit setup
;; ========================================================
;; (require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Recent mode
;; ========================================================
(recentf-mode t)
(setq recentf-max-saved-items 100)
(add-to-list 'recentf-exclude "\\.ido\\.last")
(add-to-list 'recentf-exclude "bookmark")
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))

;; (defun ido-recentf ()
;;   "Use ido to select a recently opened file from the `recentf-list'"
;;   (interactive)
;;   (let
;;       ((home (expand-file-name (getenv "HOME"))))
;;     (find-file
;;      (ido-completing-read
;;       "Recentf open: "
;;       (mapcar (lambda (path)
;; 		(replace-regexp-in-string home "~" path))
;; 	      recentf-list)
;;       nil t))))

;; (global-set-key (kbd "C-x C-r") 'ido-recentf)

;; This is my color theme
;; ========================================================
(load-theme 'tango-dark)

;; This is how I prefer text to behave
;; ========================================================
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
(setq ispell-program-name "aspell")
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
;; (require 'smex)
;; (global-set-key (kbd "M-x") 'smex)
;; (setq smex-save-file "~/.smex-items")
;; (defun smex-update-after-load (unused)
;;   (when (boundp 'smex-cache)
;;     (smex-update)))
;; (add-hook 'after-load-functions 'smex-update-after-load)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; To answer quicker
;; ========================================================
(defalias 'yes-or-no-p 'y-or-n-p)

;; This is for dired mode to omit extensions I don't want to see
(require 'dired-x)
;; (setq-default dired-omit-files-p t) ; Buffer-local variable
;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; In dired, sort directories first
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; Ignore case in eshell
(setq eshell-cmpl-ignore-case t)
;; (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
;; Start emacs in eshell
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (cd default-directory)
;;             (eshell)))

;; I use to enable ido mode in org-mode. Because I don't use org
;; anymore I need to define how to use ido next
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)

;; full screen
;; (defun fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen
;; 		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;; (global-set-key [f10] 'fullscreen)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; (require 'autopair)

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

;; Autocomplete
;; ========================================================
;; (require 'auto-complete)
;; (ac-config-default)
;; (setq ac-auto-show-menu nil)
;; (setq ac-use-quick-help nil)

;; (defun my-ac-ess-config ()
;;   (setq ac-souces
;; 	'(ac-source-R)))

;; (add-hook 'ess-mode-hook 'my-ac-ess-config)
;; (add-hook 'ess-post-run-hook 'my-ac-ess-config)

;; (setq ac-source-R
;;       '((prefix . ess-ac-start)
;; 	(requires . 2)
;; 	(candidates . ess-ac-candidates)
;; 	(document . ess-ac-help)))

;; So that RefTeX finds my bibliography
;; (setq reftex-default-bibliography '("/home/alancho/Dropbox/Papers/All_zotero_library/all-my-zotero-library.bib"))

;; (defvar reftex-cite-format-markdown
;;   '((?p . "[@%l]")
;;     (?k . "@%l")))

;; ;; Enable math
;; (setq markdown-enable-math t)

;; (defun my-markdown-mode-hook()
;;   (define-key markdown-mode-map [f8]
;;     (lambda ()
;;       (interactive)
;;       (let ((reftex-cite-format reftex-cite-format-markdown))
;; 	(reftex-citation))))
;;   (setq-local
;;    company-backends
;;    (append '(company-math-symbols-latex) company-backends))
;;   (setq-local company-math-allow-latex-symbols-in-faces t)
;;   (setq-local company-math-disallow-latex-symbols-in-faces nil)
;;   (setq-local company-math-allow-unicode-symbols-in-faces t)
;;   (setq-local company-math-disallow-unicode-symbols-in-faces nil))

(add-hook 'markdown-mode-hook 'turn-on-reftex)

;; (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
;; ;; (add-hook 'markdown-mode-hook 'flyspell-mode)
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (add-hook 'markdown-mode-hook 'wc-mode)
;; (add-hook 'markdown-mode-hook 'writeroom-mode)
;; ;; (add-hook 'markdown-mode-hook 'writegood-mode)

;; ;; Esto es porque linum mode en markdown causa conflicto con writeroom
;; (add-hook 'markdown-mode-hook (lambda () (linum-mode -1)))
;; ;; (add-hook 'markdown-mode-hook 'turn-on-window-margin-mode)

;; Pandoc mode está bueno porque pese a que no uso sus keybindings me
;; permite tener syntax highlighting específico para pandoc
;; (e.g. citations)
(add-hook 'markdown-mode-hook 'pandoc-mode)

(setq compilation-finish-function
      (lambda (buf str)
	(if (null (string-match ".*exited abnormally.*" str))
	    ;;no errors, make the compilation window go away in a few seconds
	    (progn
	      (run-at-time
	       "2 sec" nil 'delete-windows-on
	       (get-buffer-create "*compilation*"))
	      (message "No Compilation Errors!")))))

;; Use synonyms package
(setq synonyms-file "/home/alancho/mthesaur.txt")
(setq synonyms-cache-file "/home/alancho/mthesaur.txt.cache")
(require 'synonyms)


(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Para cambiar el idioma de ispell con un shortcut
(global-set-key
 [f5]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "spanish")))
(global-set-key
 [f6]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "british")))

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

(use-package ivy-hydra)
(setq ivy-switch-buffer-faces-alist
      '((emacs-lisp-mode . swiper-match-face-1)
        (dired-mode . ivy-subdir)
        (org-mode . org-level-4)))

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
   ("C-x C-d" . counsel-ag)
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
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Para que counsel-ag sólo busque R, python, y markdown
;; (setq counsel-ag-base-command "ag --nocolor --nogroup --r %s /home/alancho/")

;; Para evitar ciertos directorios en counsel find file
(setq ivy-sort-file-function 'string-lessp)
(setq ivy-extra-directories nil)
;; (setq counsel-find-file-ignore-regexp (regexp-opt '(".dropbox")))


;; Para que no alerte cuando se llega al comienzo o final de un buffer
(setq ring-bell-function #'ignore)

;; Una oportunidad a org gtd
(require 'org)

;; This is to have no blank lines inserted after headings
(setq org-blank-before-new-entry nil)

;; This is to view all at startup
(setq org-startup-folded nil)

;; Change todo state with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; Para ver bien
(add-hook 'org-mode-hook #'visual-line-mode)
;; (setq org-support-shift-select 'always)

;; (global-set-key (kbd "C-c i") 'org-capture)
(global-set-key (kbd "<f9>") 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)


(setq org-agenda-files (list "~/Dropbox/gtd/inbox.org"
			     "~/Dropbox/gtd/gtd.org"
			     ))

(setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/gtd/someday.org" :maxlevel . 1)
                           ))


(setq org-capture-templates
      (quote (("i" "Inbox" entry (file "~/Dropbox/gtd/inbox.org")
               "* TODO %?")
              ("a" "Inbox" entry (file "~/Dropbox/gtd/inbox.org")
               "* TODO %?\n%a")
	      ("l" "Inbox" entry (file "~/Dropbox/gtd/inbox.org")
               "* %(org-cliplink-capture)")
	      )))

(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/gtd/gtd.org")
  )

(global-set-key (kbd "C-c g") 'gtd)

(setq org-refile-use-outline-path 'file)

(setq org-outline-path-complete-in-steps nil)

;; Para poder poner código R en org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))


;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; ;; This is to have always the 10 coming days in the week
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 21)

(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "orange red" :weight bold)
              ("DONE" :foreground "light green" :weight bold)
              ("CANCELLED" :foreground "gray" :weight bold)
	      )))

;; ;; Tags with fast selection keys
;; (setq org-tag-alist (quote (("@inta" . ?i)
;;                             ("@home" . ?h)
;;                             ("@compu" . ?c)
;; 			    )))

(setq org-hide-leading-stars t)
(setq org-startup-indented t)

;; Para que org-agenda ocupe toda la pantalla
(setq org-agenda-window-setup 'current-window)

;; (defun my-org-agenda-skip-all-siblings-but-first ()
;;   "Skip all but the first non-done entry."
;;   (let (should-skip-entry)
;;     (unless (org-current-is-todo)
;;       (setq should-skip-entry t))
;;     (save-excursion
;;       (while (and (not should-skip-entry) (org-goto-sibling t))
;;         (when (org-current-is-todo)
;;           (setq should-skip-entry t))))
;;     (when should-skip-entry
;;       (or (outline-next-heading)
;;           (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-archive-mark-done nil)
(setq org-alphabetical-lists t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Display tags farther right
(setq org-agenda-tags-column -102)

;; Para que el mouse no haga highlight sobre la agenda
(add-hook 'org-finalize-agenda-hook
	  (lambda () (remove-text-properties
		      (point-min) (point-max) '(mouse-face t))))

;; Para que org exporte ODT con cierto formato
(setq org-odt-styles-file "~/Dropbox/templates/Frontiers_Template.ott") 

;; (setq ivy-do-completion-in-region nil)
;; (define-key ess-mode-map (kbd "C-/") 'complete-symbol)

;; Disable overwrite-mode for ever!
(define-key global-map [(insert)] nil)

;; Para que los archivos .apsim se abran en xml-mode
(add-to-list 'auto-mode-alist '("\\.apsim$" . nxml-mode))

;; Para cambiar line endings
(defun unix-file ()
  "Change the current buffer to Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'unix t))

(defun dos-file ()
  "Change the current buffer to DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'dos t))


(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(require 'polymode)
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; Para que las oraciones con punto seguido sean reconocidas con un solo espacio
(setq sentence-end-double-space nil)

;; Esto es para pegar links desde el clipboard
(global-set-key (kbd "C-x p i") 'org-cliplink)

(setq writeroom-width 120)
