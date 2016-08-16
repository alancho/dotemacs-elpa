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

;; (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))

;; Y estos son mis paquetes
(defvar alancho/packages '(autopair
			   color-theme
			   color-theme-tango
			   auctex
			   auto-complete
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
			   org
			   org-journal
			   paredit
			   pkg-info
			   popup
			   request
			   s
			   smex
			   websocket
			   yaml-mode
			   ;; yasnippet
			   )
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

;; To save and then run LaTeX in one command
(defun my-run-latex ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-master-file -1))

(defun my-LaTeX-hook ()
  (local-set-key (kbd "C-c C-c") 'my-run-latex))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)

;; Default directory
;; ========================================================
(setq default-directory "~/Dropbox/")

;; Python
;; ========================================================
;; No estoy usando Python ni elpy, y de acuerdo a esup este ultimo
;; esta usando bastante tiempo del startup. Por ende, lo desactivo:
;; (elpy-enable)
;; (elpy-use-ipython)
;; (setq elpy-rpc-backend "jedi")
;; (define-key elpy-mode-map [(shift return)] 'elpy-shell-send-region-or-buffer)
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

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        ;; (setq w2 (split-window-below w1 nil t))
        ;; (setq w2 (split-window-horizontally w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))

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

;; Expand region
;; ========================================================
;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; This are my faces, fonts, etcetera
;; ========================================================
(set-face-attribute 'default nil :height 120 :family "Inconsolata")


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
 '(completion-ignored-extensions (quote (".docx" ".xlsx" ".wmf" ".doc"
					 ".xls" ".csv" ".bib" ".o" "~" ".bin" ".bak"
					 ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln"
					 ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc"
					 ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/"
					 ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class"
					 ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl"
					 ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl"
					 ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp"
					 ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs"
					 ".tps" ".vrs" ".pyc" ".pyo" ".odt" ".pptx" ".ppt" ".txt" ".dat"))))

;; My magit setup
;; ========================================================
;; (require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Recent mode
;; ========================================================
(recentf-mode t)
;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'ido-recentf)

(defun ido-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let
      ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read
      "Recentf open: "
      (mapcar (lambda (path)
                (replace-regexp-in-string home "~" path))
              recentf-list)
      nil t))))

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
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (global-set-key (kbd "C-.") 'yas/expand)

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
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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

;; Start emacs in eshell
(add-hook 'emacs-startup-hook
          (lambda ()
            (cd default-directory)
            (eshell)))

;; I use to enable ido mode in org-mode. Because I don't use org
;; anymore I need to define how to use ido next
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f10] 'fullscreen)

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
(ac-config-default)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help nil)

(defun my-ac-ess-config ()
  (setq ac-souces
	'(ac-source-R)))

(add-hook 'ess-mode-hook 'my-ac-ess-config)
(add-hook 'ess-post-run-hook 'my-ac-ess-config)

(setq ac-source-R
      '((prefix . ess-ac-start)
	(requires . 2)
	(candidates . ess-ac-candidates)
	(document . ess-ac-help)))

;; My org-mode setup
;; =================
(require 'org)

;; This is to have no blank lines inserted after headings
(setq org-blank-before-new-entry nil)

;; This is to view all at startup
(setq org-startup-folded nil)

;; Change todo state with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; Fixing task state with S-arrow
;; (setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; To allow S-arrow to work only with time stamps
;; (setq org-support-shift-select (quote always))

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11>") 'org-capture)

(setq org-agenda-files (list "~/Dropbox/Scripts/gtd/tutti.org"))

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3)))

;; Capture templates for TODO tasks, Notes, and journal
(setq org-capture-templates
      (quote (("t" "Tareas" entry (file+headline "~/Dropbox/Scripts/gtd/tutti.org" "Tareas")
               "* TODO %?%(org-set-tags)\n%U\n")
              ;; ("n" "Notes" entry (file+headline "~/Dropbox/gtd/tutti.org" "Notas")
              ;;  "* %? :NOTE:\n%U\n")
	      ("c" "Calendar" entry (file+headline "~/Dropbox/Scripts/gtd/tutti.org" "Agenda")
               "* %? \n%U\n")
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

(setf org-todo-keyword-faces '(("TODO" . (:foreground "cyan" :bold t :weight bold))
			       ("NEXT" . (:foreground "yellow" :bold t :weight bold))
			       ("STARTED" . (:foreground "green" :bold t :weight bold))
			       ("WAITING" . (:foreground "orange" :bold t :weight bold))
			       ("DONE" . (:foreground "gray50" :bold t :weight bold))))

;; Tags with fast selection keys
(setq org-tag-alist (quote (("supermercado" . ?s)
                            ("oficina" . ?o)
                            ("casa" . ?c)
                            ("finde" . ?f)
                            ("NOTE" . ?n))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
	      )))

(setq org-hide-leading-stars t)
(setq org-startup-indented t)

;; Para que org-agenda ocupe toda la pantalla
(setq org-agenda-window-setup 'current-window)

;; Con solo tres priorities no me alcanza -- necesito desde A a E, en
;; concordancia con Eat That Frog
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(setq org-agenda-custom-commands
      '(("n" "Notes"
	 ((tags "NOTE")))
	("s" "Supermercado"
	 ((tags "supermercado")))
	("h" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	("o" "En la oficina"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags-todo "oficina/STARTED" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "oficina/NEXT" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "oficina/WAITING" ((org-agenda-sorting-strategy '(priority-down))))))
	("c" "En casa"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags-todo "casa/STARTED" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "casa/NEXT" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "casa/TODO" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "casa/WAITING" ((org-agenda-sorting-strategy '(priority-down))))))
	("f" "El fin de semana"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags-todo "finde/STARTED" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "finde/TODO" ((org-agenda-sorting-strategy '(priority-down))))
	  (tags-todo "finde/WAITING" ((org-agenda-sorting-strategy '(priority-down))))))))

;; Vamos a empezar a usar org-journal
(setq org-journal-dir "~/Dropbox/Scripts/gtd/journal/")

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-archive-mark-done nil)
(setq org-alphabetical-lists t)

;; ;; Always highlight the current agenda line
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#3e4446")
;; (set-face-foreground 'highlight nil)
;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (hl-line-mode 1))
;;           'append)
;; ;; But I don't need to highlight current line globally
;; (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)


;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;; Para que el mouse no haga highlight sobre la agenda
(add-hook 'org-finalize-agenda-hook
	  (lambda () (remove-text-properties
		      (point-min) (point-max) '(mouse-face t))))

;; A ver si esto sirve
;; (require 'helm-config)

;; Voy a empezar a usar habits tracking en org. Para ello lo primero
;; es habilitar el modulo
(setq org-modules (quote (org-habit)))

;; Formato en que habits aparecen en la agenda
(setq org-habit-graph-column 50)

;; Para crear entries con F9
(global-set-key (kbd "<f9>") 'org-journal-new-entry)

;; Para que las entries se abran en el mismo buffer, sin split
(setq org-journal-find-file 'find-file)
