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
(defvar alancho/packages '(autopair
			   color-theme
			   color-theme-tango
			   auctex
			   ;; auto-complete
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
			   julia-mode
			   magit
			   markdown-mode
			   org
			   pandoc-mode
			   paredit
			   pkg-info
			   popup
			   request
			   s
			   smex
			   synonyms
			   websocket
			   window-margin
			   writeroom-mode
			   yaml-mode
			   yasnippet
			   use-package
			   avy
			   counsel
			   swiper
			   ivy
			   ivy-hydra
			   wgrep)
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
(exec-path-from-shell-initialize)

;; Start in *scratch*
;; ========================================================
(setq inhibit-startup-message t)

;; AucTeX
;; ========================================================
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(add-hook 'LaTeX-mode-hook '(lambda ()
			      (if (string-match "\\.Rnw\\'" buffer-file-name)
				  (setq fill-column 80))))

(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;; Para que use biber en lugar de bibtex
;; (http://tex.stackexchange.com/questions/154751/biblatex-with-biber-configuring-my-editor-to-avoid-undefined-citations)
;; (setq TeX-parse-self t)
;; (setq LaTeX-biblatex-use-Biber t)
;; (setq LaTeX-always-use-Biber t)

;; To save and then run LaTeX in one command
(defun my-run-latex ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-master-file -1)
  ;; (TeX-command "Biber" 'TeX-master-file -1)
  ;; (TeX-command "LaTeX" 'TeX-master-file -1)
  )

(defun my-LaTeX-hook ()
  (local-set-key (kbd "C-c C-c") 'my-run-latex))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)

;; Default directory
;; ========================================================
;; (setq default-directory "~/Dropbox/")

;; Python
;; ========================================================
;; No estoy usando Python ni elpy, y de acuerdo a esup este ultimo
;; esta usando bastante tiempo del startup. Por ende, lo desactivo:
(elpy-enable)
(elpy-use-ipython)
(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")
;; (setq elpy-rpc-backend "jedi")
;; (setq ac-modes (remove 'python-mode ac-modes))
(define-key elpy-mode-map [(shift return)] 'elpy-shell-send-region-or-buffer)
;; (define-key elpy-mode-map [(C-return)] 'elpy-company-backend)

;; Encryption
;; ========================================================
;; (require 'epa-file)
;; (epa-file-enable)

;; ESS
;; ========================================================
(require 'ess-site)

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;; (setq ess-use-ido t)

;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;       (progn
;; 	(delete-other-windows)
;; 	(setq w1 (selected-window))
;; 	(setq w1name (buffer-name))
;; 	(setq w2 (split-window w1 nil t))
;; 	;; (setq w2 (split-window-below w1 nil t))
;; 	;; (setq w2 (split-window-horizontally w1 nil t))
;; 	(R)
;; 	(set-window-buffer w2 "*R*")
;; 	(set-window-buffer w1 w1name))))

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (setq cur-window (selected-window))
        (print cur-window)
        (case (length (window-list))
          (1 (select-window (split-window-right)))
          (t (other-window 1)))
        (R)
        (print "R started")
        (select-window cur-window))))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)))

(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))

(add-hook 'Rnw-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)))

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

(define-key ess-mode-map (kbd "C-+") 'then_ggplot_plus)

(define-key inferior-ess-mode-map (kbd "C-+") 'then_ggplot_plus)

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

;; En lugar de auto-complete empezar a usar company-mode con estos keybindings
;; (add-hook 'after-init-hook 'global-company-mode)

;; (define-key company-active-map [tab] 'company-complete-selection)
;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;; (define-key company-active-map [return] 'company-complete-selection)
;; (define-key company-active-map (kbd "RET") 'company-complete-selection)
;; (define-key company-active-map (kbd "M-n") nil)
;; (define-key company-active-map (kbd "M-p") nil)
;; (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
;; (define-key company-active-map (kbd "M-,") 'company-select-next)
;; (define-key company-active-map (kbd "M-k") 'company-select-previous)

;; Expand region
;; ========================================================
;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; This are my faces, fonts, etcetera
;; ========================================================
;; (set-face-attribute 'default nil :family "Ubuntu Mono" :height 110 :weight 'normal)
;; (set-face-attribute 'default nil :height 125 :family "Inconsolata")
(set-face-attribute 'default nil :height 110 :family "Monaco")


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions
   (quote
    (".docx" ".xlsx" ".wmf" ".doc" ".xls" ".csv" ".bib" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".odt" ".pptx" ".ppt" ".txt" ".dat")))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (yaml-mode writeroom-mode window-margin websocket use-package synonyms smex request popup paredit pandoc-mode org-journal markdown-mode magit ivy-hydra ido-ubiquitous idle-highlight-mode expand-region exec-path-from-shell ess epc elpy deft counsel-projectile company-math color-theme-tango cl-generic avy autopair auctex ag))))

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
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
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

(require 'autopair)

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
(setq reftex-default-bibliography '("/home/alancho/Dropbox/Papers/all-my-zotero-library.bib"))

(defvar reftex-cite-format-markdown
  '((?p . "[@%l]")
    (?k . "@%l")))

;; Enable math
(setq markdown-enable-math t)

(defun my-markdown-mode-hook()
  (define-key markdown-mode-map [f8]
    (lambda ()
      (interactive)
      (let ((reftex-cite-format reftex-cite-format-markdown))
	(reftex-citation))))
  (setq-local
   company-backends
   (append '(company-math-symbols-latex) company-backends))
  (setq-local company-math-allow-latex-symbols-in-faces t)
  (setq-local company-math-disallow-latex-symbols-in-faces nil)
  (setq-local company-math-allow-unicode-symbols-in-faces t)
  (setq-local company-math-disallow-unicode-symbols-in-faces nil))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
;; Esto es porque linum mode en markdown causa conflicto con writeroom
(add-hook 'markdown-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'markdown-mode-hook 'turn-on-window-margin-mode)

;; Pandoc mode está bueno porque pese a que no uso sus keybindings me
;; permite tener syntax highlighting específico para pandoc
;; (e.g. citations)
(add-hook 'markdown-mode-hook 'pandoc-mode)

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


;; Lo siguiente lo saqué de http://www.jethrokuan.com/init.html#orgheadline40
;; Flx is required for fuzzy-matching.
;; (use-package flx)

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
  :bind (:map help-map
              ("f" . counsel-describe-function)
              ("v" . counsel-describe-variable)
              ("l" . counsel-info-lookup-symbol))
  :config
  (ivy-mode 1)
  (setq ivy-height 10)
  (setq counsel-find-file-at-point t)
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
(setq counsel-ag-base-command "ag --nocolor --nogroup --r --python --markdown --tex %s /home/alancho/")

;; Para evitar ciertos directorios en counsel find file
(setq ivy-sort-file-function 'string-lessp)
(setq ivy-extra-directories nil)
(setq counsel-find-file-ignore-regexp (regexp-opt '(".dropbox")))

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

;; (global-set-key (kbd "<f2>") 'calendar)
;; (global-set-key (kbd "<f9>") 'org-journal-new-entry)
(global-set-key (kbd "<f9>") 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-files (list "~/Dropbox/archive/gtd/projects.org"
			     "~/Dropbox/archive/gtd/inbox.org"))

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3)))

;; Capture templates for TODO tasks, Notes, and journal
(setq org-capture-templates
      (quote (("i" "Inbox" entry (file+datetree "~/Dropbox/archive/gtd/inbox.org")
               "* TODO %?\n")
              )))

;; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

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
      (quote (("TODO" :foreground "cyan" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "gray" :weight bold)
	      )))

;; Tags with fast selection keys
(setq org-tag-alist (quote (("@oficina" . ?o)
                            ("@home" . ?h)
                            ("@galpon" . ?g)
                            ("@campo" . ?c)
                            ("@sabado_temprano" . ?s)
			    )))

(setq org-hide-leading-stars t)
(setq org-startup-indented t)

;; Para que org-agenda ocupe toda la pantalla
(setq org-agenda-window-setup 'current-window)

;; Con solo tres priorities no me alcanza -- necesito desde A a E, en
;; concordancia con Eat That Frog
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?B)

(setq org-agenda-custom-commands
      '(("n" "Next actions de mis proyectos" todo "TODO"
         ((org-agenda-overriding-header "Next actions")
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/projects.org"))
	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
	("o" "En la oficina, standalone tasks" tags "@oficina"
         ((org-agenda-overriding-header "En la oficina")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
	   (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/inbox.org"))))
	("h" "En casa, standalone tasks" tags "@home"
         ((org-agenda-overriding-header "En casa")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/inbox.org"))))
	("g" "En el galpón, standalone tasks" tags-todo "@galpon"
         ((org-agenda-overriding-header "En el galpón")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/inbox.org"))))
	("c" "En el campo, standalone tasks" tags-todo "@campo"
         ((org-agenda-overriding-header "En el campo")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/inbox.org"))))
	("s" "El sábado temprano, standalone tasks" tags-todo "@sabado_temprano"
         ((org-agenda-overriding-header "Sábado temprano")
	  (org-agenda-sorting-strategy '(priority-down))
	  (org-agenda-files '("~/Dropbox/archive/gtd/inbox.org"))))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

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

(setq ivy-do-completion-in-region nil)
(define-key ess-mode-map (kbd "C-/") 'completion-at-point)
(define-key ess-mode-map (kbd "C-/") 'completion-at-point)
