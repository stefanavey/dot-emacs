;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_packages.el                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Libraries ;;
;;;;;;;;;;;;;;;

(use-package f
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer t)

(use-package cl)
(use-package color)

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "BOX_FOLDER")
  (exec-path-from-shell-copy-env "SIGNATURES_BOX")
  (exec-path-from-shell-initialize)
)


(use-package date2name
  :ensure t
)

;;;;;;;;;;;;;;;;
;; Aesthetics ;;
;;;;;;;;;;;;;;;;
;; Turn minor mode "default text scale" on in order to globally adjust text size
;; with C-M-= and C-M--
(use-package default-text-scale
  :ensure t
  :config (default-text-scale-mode t))


;; TODO: use-package is not working here
;; (use-package presentation
;;   :load-path (lambda () (xah-get-fullpath "lisp/presentation-mode/"))
;;   :preface
;;   (defun my-presentation-on ()
;;     (global-display-line-numbers-mode 1))
;;   (defun my-presentation-off ()
;;     (global-display-line-numbers-mode -1))
;;   :hook ((presentation-on  . my-presentation-on)
;;          (presentation-off . my-presentation-off)))

(defun my-presentation-on ()
  (global-display-line-numbers-mode 1))
(defun my-presentation-off ()
  (global-display-line-numbers-mode -1))
(load (xah-get-fullpath "lisp/presentation-mode/presentation.el"))
(add-hook 'presentation-on-hook #'my-presentation-on)
(add-hook 'presentation-off-hook #'my-presentation-off)

(use-package smart-mode-line
  :ensure t
  :init (add-hook 'after-init-hook 'sml/setup)
  :config
  (setq sml/theme 'automatic)
  (setq sml/modified-char "m")
  (add-to-list 'sml/replacer-regexp-list '("^~/Box Sync/" ":Box:") t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :pin melpa-stable
  :init
  (setq custom-safe-themes t) ;; treat all themes as safe
  :config
  (color-theme-sanityinc-solarized--define-theme dark))

(use-package emojify
  :ensure t
  :pin melpa-stable
  :defer 10
  :hook ((text-mode org-mode) . global-emojify-mode)
  :config
  (setq emojify-program-contexts '(comments))
  :bind ("C-c E" . emojify-insert-emoji))

;; dired-x is not available?
(use-package dired-x
  :disabled t
  :ensure t
  :after dired)

(use-package dired-du
  :ensure t
  :after dired
  :defer t
  :config
  (setq dired-du-size-format t)
  (setq dired-du-update-headers t))

;; ;; Disable due to error after upgrading to Emacs 28+. Getting syntax
;; ;; highlighting now from diredfl
;; (use-package dired+
;;   :load-path (lambda () (xah-get-fullpath "lisp/diredp/"))
;;   :after dired
;;   :config
;;   ;; TODO: Fix so that 'e' command in dired is not overwritten
;;   (setq diredp-hide-details-initially-flag nil)
;;   (unbind-key "M-b" dired-mode-map)
;;   ;; (set-face-attribute 'diredp-dir-name nil :foreground "DarkRed")
;;   )

(use-package diredfl
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode)
  ;; Based on init file from John Soo (https://github.com/jsoo1/dotfiles)
  ;; Define colors with base colors coming from solarized color theme
  (defvar base03  "#002b36" "Theme base03.")
  (defvar base02  "#073642" "Theme base02.")
  (defvar base01  "#586e75" "Theme base01.")
  (defvar base00  "#657b83" "Theme base00.")
  (defvar base3   "#fdf6e3" "Theme base3.")
  (defvar base2   "#eee8d5" "Theme base2.")
  (defvar base1   "#93a1a1" "Theme base1.")
  (defvar base0   "#839496" "Theme base0.")
  (defvar yellow  "#b58900" "Theme yellow.")
  (defvar red     "#dc322f" "Theme red.")
  (defvar green   "#859900" "Theme green.")
  (defvar blue    "#286bd2" "Theme blue.")
  (defvar cyan    "#2aa198" "Theme cyan.")
  (defvar magenta "#d33682" "Theme magenta.")
  (defvar orange  "#cb4b16" "Theme orange.")
  (defvar violet  "#6c71c4" "Theme violet.")
  ;; Assign the colors for diredfl to overwrite the defaults which are not good
  ;; when using solarized. NOTE: This is customized for solarized-dark but can
  ;; be modified for solarized-light using the other base values.
  (set-face-attribute
   diredfl-dir-heading nil
   :foreground blue
   :background base03)
  (set-face-attribute
   diredfl-number nil
   :foreground green
   :background base03)
  (set-face-attribute
   diredfl-date-time nil
   :foreground yellow
   :background base03)
  (set-face-attribute
   diredfl-file-name nil
   :foreground base0
   :background base03)
  (set-face-attribute
   diredfl-file-suffix nil
   :foreground green
   :background base03)
  (set-face-attribute
   diredfl-dir-name nil
   :foreground blue
   :background base03)
  (set-face-attribute
   diredfl-symlink nil
   :foreground cyan
   :background base03)
  (set-face-attribute
   diredfl-no-priv nil
   :foreground base0
   :background base03)
  (set-face-attribute
   diredfl-dir-priv nil
   :foreground blue
   :background base03)
  (set-face-attribute
   diredfl-read-priv nil
   :foreground base0
   :background base03)
  (set-face-attribute
   diredfl-write-priv nil
   :foreground cyan
   :background base03)
  (set-face-attribute
   diredfl-exec-priv nil
   :foreground magenta
   :background base03)
  (set-face-attribute
   diredfl-rare-priv nil
   :foreground magenta
   :background base03)
  (set-face-attribute
   diredfl-other-priv nil
   :foreground orange
   :background base03)
  (set-face-attribute
   diredfl-deletion nil
   :foreground red
   :background base03)
  (set-face-attribute
   diredfl-deletion-file-name nil
   :foreground red
   :background base03)
  (set-face-attribute
   diredfl-flag-mark nil
   :foreground violet
   :background base03)
  (set-face-attribute
   diredfl-flag-mark-line nil
   :foreground violet
   :background base03)
  (set-face-attribute
   diredfl-ignored-file-name nil
   :foreground base01
   :background base03))

;; (use-package dired-quick-sort
;;   :config
;;   (dired-quick-sort-setup))

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :after (color cl)
  :preface
  (defun hsl-to-hex (h s l)
    "Convert H S L to hex colours"
    (let (rgb)
      (setq rgb (color-hsl-to-rgb h s l))
      (color-rgb-to-hex (nth 0 rgb)
			(nth 1 rgb)
			(nth 2 rgb))))
  (defun bracket-colors ()
    "Calculate the bracket colours based on background.
Used for better rainbow colors than default.
source: `https://emacs.stackexchange.com/questions/21303/looking-for-a-better-way-of-tweaking-rainbow-delimiters'"
    (let (hexcolors lightvals)
      (if (>= (color-distance  "white"
                               (face-attribute 'default :background))
              (color-distance  "black"
                               (face-attribute 'default :background)))
          (setq lightvals (list 0.65 0.55))
	(setq lightvals (list 0.35 0.30)))
      (concatenate 'list
                   (dolist (n'(.71 .3 .11 .01))
                     (push (hsl-to-hex (+ n 0.0) 1.0 (nth 0 lightvals)) hexcolors))
                   (dolist (n '(.81 .49 .17 .05))
                     (push (hsl-to-hex (+ n 0.0) 1.0 (nth 1 lightvals)) hexcolors)))
      (reverse hexcolors)))
  :config
  ;; Commented out to use defaults since this wasn't looking right
  ;; (set-face-attribute 'popup-face nil
  ;; 		      :background "white" :foreground "black")
  ;; (set-face-attribute `rainbow-delimiters-depth-1-face nil
  ;; 		      :foreground "#888")
  ;; (set-face-attribute `rainbow-delimiters-depth-2-face nil
  ;; 		      :foreground (nth 0 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-3-face nil
  ;; 		      :foreground (nth 1 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-4-face nil
  ;; 		      :foreground (nth 2 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-5-face nil
  ;; 		      :foreground (nth 3 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-6-face nil
  ;; 		      :foreground (nth 4 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-7-face nil
  ;; 		      :foreground (nth 5 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-8-face nil
  ;; 		      :foreground (nth 6 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-depth-9-face nil
  ;; 		      :foreground (nth 7 (bracket-colors)))
  ;; (set-face-attribute `rainbow-delimiters-unmatched-face nil
  ;; 		      :foreground "white" :background "red")
  :hook ((prog-mode ess-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-r-colors t)
  :hook ((prog-mode ess-mode) . rainbow-mode))

;; highlight todo keywords
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (:map global-map
	      ("M-o" . ace-window)))

(use-package browse-kill-ring
  :ensure t
  :pin melpa-stable
  :config
  (setq browse-kill-ring-replace-yank t)
  :bind (:map global-map
	      ("M-y" . browse-kill-ring)))

;; TODO: Needs some customization to work with ESS
(use-package scratch
  :ensure t
  :pin melpa-stable)

(use-package popup
  :ensure t
  :config
  (set-face-attribute 'popup-face nil :background "white" :foreground "black")
  (set-face-attribute 'popup-tip-face nil :background "white" :foreground "black")
  (set-face-attribute 'popup-menu-selection-face nil :background "#92C1F0" :foreground "black")
  (set-face-attribute 'popup-isearch-match nil :background "#92C1F0" :foreground "black"))

(use-package use-package-hydra
  :ensure t)

;; Can't figure out use-package-hydra
(use-package image+
  :ensure t
  :after (image hydra use-package-hydra)
  :config
  (setq imagex-auto-adjust-mode 1)
  (setq imagex-quiet-error t))
  ;; :bind (:map global-map ("C-x C-l" . nil))
  ;; :hydra
  ;; imagex-sticky-binding ( )
  ;; "Manipulating Image"
  ;; ("+" imagex-sticky-zoom-in "zoom in")
  ;; ("-" imagex-sticky-zoom-out "zoom out")
  ;; ("M" imagex-sticky-maximize "maximize")
  ;; ("O" imagex-sticky-restore-original "restore original")
  ;; ("S" imagex-sticky-save-image "save file")
  ;; ("r" imagex-sticky-rotate-right "rotate right")
  ;; ("l" imagex-sticky-rotate-left "rotate left"))

;; Previously:
  ;; `(when (require 'hydra nil t)
  ;;    (defhydra imagex-sticky-binding (global-map "C-x C-l")
  ;;      "Manipulating Image"
  ;;      ("+" imagex-sticky-zoom-in "zoom in")
  ;;      ("-" imagex-sticky-zoom-out "zoom out")
  ;;      ("M" imagex-sticky-maximize "maximize")
  ;;      ("O" imagex-sticky-restore-original "restore original")
  ;;      ("S" imagex-sticky-save-image "save file")
  ;;      ("r" imagex-sticky-rotate-right "rotate right")
  ;;      ("l" imagex-sticky-rotate-left "rotate left"))))

(use-package csv-mode
  :ensure t
  :mode
  ("\\.csv\\'" . csv-mode)
  ("\\.tsv\\'" . csv-mode)
  :init
  (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))
  (add-hook 'csv-mode-hook (lambda () (visual-line-mode -1)))
  (add-hook 'csv-mode-hook #'flyspell-mode-off)
					; This hook makes csv-mode align the fields by default in the entire buffer
  (add-hook 'csv-mode-hook
	    (lambda () (csv-align-fields nil (point-min) (point-max)))))

(use-package org
  :diminish org-indent
  :preface
  (defun spa/mtime (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))
  (defun spa/latest-file (path)
    "Get latest file (including directory) in PATH."    
  (let ((e (f-entries path)))
    (car (sort e (lambda (a b)
                   (not (time-less-p (spa/mtime a)
                                     (spa/mtime b))))))))
  (defun spa/org-link-screenshot ()
    "Insert link to last screenshot"
    (interactive)
    (setq dir "/Users/savey/OneDrive - JnJ/Pictures/Screenshots")
    (setq path (spa/latest-file dir))
    (setq name (read-from-minibuffer "Link Name: " nil nil nil
				     'minibuffer-history))
    (setq string (concat "[[" path "][" name "]]"))
    (insert string)
    )
  (defun spa/diminish-org-indent ()
    (interactive)
    (diminish 'org-indent-mode ""))
  ;; (defun spa/on-org-clock-out ()
  ;;   "Insert the script pushRepos.sh into the subshell process"
  ;;   ;;  (shell-command "~/Documents/pushAllRepos.sh")) ;; old way!
  ;;   (shell) ;; open a new subshell in emacs
  ;;   (insert "~/Documents/pushRepos.sh")) ;; Run the script interactively in emacs
  ;; ORG-MODE:  * My Task
  ;;              SCHEDULED: <%%(diary-last-day-of-month date)>
  ;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
  ;; See also:  (setq org-agenda-include-diary t)
  ;; source: https://emacs.stackexchange.com/questions/31683/schedule-org-task-for-last-day-of-every-month
  ;; (diary-last-day-of-month '(2 28 2017))
  (defun diary-last-day-of-month (date)
    "Return `t` if DATE is the last day of the month."
    (let* ((day (calendar-extract-day date))
	   (month (calendar-extract-month date))
	   (year (calendar-extract-year date))
	   (last-day-of-month
            (calendar-last-day-of-month month year)))
      (= day last-day-of-month)))
  :config
  ;; Don't change the face of DONE headlines
  (setq org-fontify-done-headline nil)
  ;; Increase org-imenu-depth to be able to search deeper headings
  (setq org-imenu-depth 6)
  ;; Enforce TODO Dependencies so parent cannot be done when child is undone or ordering
  ;; must be followed
  (setq org-enforce-todo-dependencies t)
  ;; Set properties for how org-agenda shows clock consistency
 (setq org-agenda-clock-consistency-checks
   (quote
    (:max-duration "4:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
		   ("4:00" "12:30")
		   :default-face
		   ((:background "DarkRed")
		    (:foreground "white"))
		   :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))  
  ;; Set format for org duration in hours rather than default days:hours:minutes
  (setq org-duration-format (quote h:mm))
  ;; Show notifications within Org mode in the minibuffer
  (setq org-show-notification-handler 'message)
  ;; Setup org mode to view latex formula previews
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; Set the directory for org files
  (setq org-agenda-files '("/Users/savey/OneDrive - JNJ/org"))
  ;; Set default priority of TODO items to be the same as [#C]
  (setq org-default-priority ?C)
  ;; Set default range from displaying clocks to all time until now
  ;; Use C-u C-u prefix to choose a different range   
  (setq org-clock-display-default-range 'untilnow)
  ;; Set clocktable defaults
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :compact t))
  ;; Load pandoc back-end for exporting in org-mode
  ;; ;; Include built-in diary in org agenda to get holidays
  ;; (setq org-agenda-include-diary t)
  ;; restore window configuration when closing agenda
  (setq org-agenda-restore-windows-after-quit t)
  ;; use cornered arrow instead of elipse for hidden items
  (setq org-ellipsis "⤵")
  ;; hide the leading stars (even when not in indent-mode)
  (setq org-hide-leading-stars t)
  ;; warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)
  ;; don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; Default notes and agenda files
  (setq org-default-notes-file "~/OneDrive - JNJ/org/notes.org")
  ;; Specify where items can be refiled (any agenda file up to 9th level)
  (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  ;; org-capture templates
  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
	(quote (("t" "todo" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("r" "respond" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		("n" "note" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "~/OneDrive - JNJ/org/diary.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
		("w" "org-protocol" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t)
		("m" "Meeting" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		("p" "Phone call" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
		("h" "Habit" entry (file "~/OneDrive - JNJ/org/refile.org")
		 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
  ;; format string used when creating CLOCKSUM lines and when generating a
  ;; time duration (avoid showing days)
  (setq org-time-clocksum-format
	'(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  ;; Press C-a twice to get to start of heading instead of stars
  (setq org-special-ctrl-a/e 'reversed)
  ;; Log a "CLOSED" timestamp when something is done
  (setq org-log-done 'time)
  (setq org-log-reschedule 'note)
  ;; '@' log timestamp AND note when this keyword is entered
  ;; '!' log timestamp when you leave keyword
  ;; stamp when the state changes
  (setq org-todo-keywords
	(quote ((sequence "LEARN(l!)" "|" "LEARNED(L!)")
		(sequence "TODO(t!)" "|" "DONE(d!)" "DELEGATED(o@/!)")
		(type "PROJECT(P!)" "PROJECT_HOLD(H!)" "|" "PROJECT_DONE(D!)"
		      "PROJECT_TRANSFERRED(T@)")
		(sequence "REPLAY(r)" "|" "REPLAYED(p!)")
		(sequence "TOREAD(e)" "|" "READ(E!)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
			  "PHONE" "MEETING")
		(sequence "RUNNING(R@)" "ERROR(e@)" "|"  "FINISHED(f@)"))))
  (setq org-agenda-custom-commands
	'(
	  ("p" . "List All Projects and High-Level Tasks") ; describe prefix "p"
	  ("pe" "Everything Concrete" tags "-MAYBE/PROJECT")
	  ("pp" "Portfolio Projects" tags "-MAYBE+portfolio/PROJECT")
	  ("pn" "Non-Portfolio Projects" tags "-MAYBE-portfolio/PROJECT")
	  ("pm" "Someday or Maybe Projects" tags "+MAYBE/PROJECT")
	  ("l" "What to do during Lunch?" tags-todo "lunch")
	  ("n" "Agenda and TODOs (without Projects)"
	   ((agenda #1="")
	    (todo "TODO|LEARN|TOREAD|REPLAY|WAITING|HOLD|RUNNING")))
	  ("q" "Quick tasks" tags-todo "EFFORT>=\"0:01\"&EFFORT<=\"0:15\"")
          ("d" "Timeline for today" ((agenda "" ))
           (;(org-agenda-ndays 1)
	    (org-agenda-span 'day)
            (org-agenda-show-log t)
            (org-agenda-log-mode-items '(clock closed))
            (org-agenda-clockreport-mode t)
	    (org-agenda-time-grid nil)
            (org-agenda-entry-types '())))
          ("W" "Waiting on Others" todo "WAITING")
	  ("w" "Weekly Review"
	   ((tags "CLOSED>=\"<-1w>\"")	; NOTE: This is slow
	    (agenda ""))
	   (
	    ;; (org-agenda-format-date "")
	    (org-agenda-overriding-header "WEEKLY TIMESHEET")
	    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
	    (org-agenda-span 'week)
	    (org-agenda-start-on-weekday 1)
	    (org-agenda-start-with-clockreport-mode t)
            (org-agenda-show-log t)
	    (org-agenda-show-clocking-issues) ; NOTE: Not working to show clocking issues
	    (org-agenda-time-grid nil)
	    ))
	  ("P" "Printed agenda"
           ((agenda "" ((org-agenda-span 7)                      ;; overview of appointments
			(org-agenda-start-on-weekday nil)         ;; calendar begins today
			(org-agenda-repeating-timestamp-show-all t)
			(org-agenda-entry-types '(:timestamp :sexp))))
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
			(org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
			(org-agenda-todo-keyword-format "[ ]")
			(org-agenda-scheduled-leaders '("" ""))
			(org-agenda-prefix-format "%t%s")))
            (todo "TODO"                                          ;; todos sorted by context
                  ((org-agenda-prefix-format "[ ] %T: ")
                   (org-agenda-sorting-strategy '(priority-down tag-up))
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
           (
	    ;; (org-agenda-with-colors nil)
            (org-agenda-compact-blocks t)
            (org-agenda-remove-tags t)
            ;; (ps-number-of-columns 2)
            ;; (ps-landscape-mode t)
	    ))
	   ))
  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("PROJECT" :foreground "#3288bd" :weight bold)
		("DELEGATED" :foreground "forest green" :weight bold)
		("COMPLETED" :foreground "forest green" :weight bold)
		("TRANSFERRED" :foreground "forest green" :weight bold)
		("REPLAY" :foreground "red" :weight bold)
		("REPLAYED" :foreground "forest green" :weight bold)
		("LEARN" :foreground "red" :weight bold)
		("LEARNED" :foreground "forest green" :weight bold)
		("TOREAD" :foreground "red" :weight bold)
		("READ" :foreground "forest green" :weight bold)
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "orange" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold)
		("PHONE" :foreground "forest green" :weight bold)
		("RUNNING" :foreground "orange" :weight bold)
		("ERROR" :foreground "magenta" :weight bold)
		("FINISHED" :foreground "forest green" :weight bold))))
  ;; (setq org-todo-keywords
  ;; 	   '((sequence "TODO(t)" "|" "DONE(d@)")
  ;; 	     (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(f@)")
  ;; 	     (sequence "RUNNING(R@)" "ERROR(E@)" "|"  "FINISHED(F@)")
  ;; 	     (sequence "ANALYZING(a@)" "|"  "ANALYZED(A@)")
  ;; 	     (sequence "WAITING(w@)" "|" "CANCELED(c@)")
  ;; 	     (sequence "HYPOTHESIS(h)" "|" "Results(e)")))

  ;; (setq org-agenda-archives-mode nil)
  ;; (setq org-agenda-skip-comment-trees nil)
  ;; (setq org-agenda-skip-function nil)

  ;; Effort and global properties
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

  ;; Set global Column View format
  (setq org-columns-default-format '"%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM(Clock)")
  
  ;; Prefer a vertical split in Org mode for the agenda
  (defadvice org-agenda (around split-vertically activate)
    (let ((split-width-threshold 80))  ; or whatever width makes sense for you
      ad-do-it))
  ;; (add-hook 'org-capture-mode-hook 'org-clock-out) ; Clock out of task when org-capture is run
  ;; (add-hook 'org-clock-out-hook 'spa/on-org-clock-out)
  ;; ;; Change the default PDF viewer to acroread
  ;; (eval-after-load "org"
  ;;   '(progn
  ;; 	  ;; Change .pdf association directly within the alist
  ;; 	  (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))
  ;; Trigger org-trello for each org file
  ;; (add-hook 'org-mode-hook 'org-trello-mode)
  ;; Open links to directories in Emacs (dired) instead of the default
  ;; "open" command that opens the finder
  (add-to-list 'org-file-apps '(directory . emacs))
  ;; Customize org-goto to search headlines via completion
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-max-level 8)
  :hook
  (org-indent-mode . spa/diminish-org-indent)
  (org-mode . turn-on-font-lock)
  (org-mode . visual-line-mode)
  (org-agenda-mode . hl-line-mode)
  :bind (:map global-map
	      ("C-c a" . org-agenda))
  (:map org-mode-map
	;; ("C-c b" . org-iswitchb)	; now using ido-mode for buffer switching
	("C-c l" . org-store-link)
	("C-c c" . org-capture)
	("C-c C-f" . org-forward-heading-same-level)
	("C-c s" . 'spa/org-link-screenshot)
	("C-c &" . 'org-mark-ring-goto)))

(use-package org-clock-convenience
  :ensure t
  :init
  (setq org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\): *\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)? +Clocked: +\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
  (defun spa/org-agenda-mode-fn ()
      (define-key org-agenda-mode-map
	(kbd "<S-up>") #'org-clock-convenience-timestamp-up)
      (define-key org-agenda-mode-map
	(kbd "<S-down>") #'org-clock-convenience-timestamp-down)
      (define-key org-agenda-mode-map
	(kbd "o") #'org-clock-convenience-fill-gap)
      (define-key org-agenda-mode-map
	(kbd "e") #'org-clock-convenience-fill-gap-both))
  (add-hook 'org-agenda-mode-hook #'spa/org-agenda-mode-fn))

(use-package ox-pandoc
  :ensure t
  :after (org))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package google-this
  :ensure t
  :bind
  ("C-c C-g" . google-this))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package osx-dictionary
  :ensure t
  :bind
  ("C-c d" . osx-dictionary-search-word-at-point))

(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1)
  (unbind-key "M-n" smartscan-map)
  (unbind-key "M-p" smartscan-map)
  :bind (:map smartscan-map
	      ("M-<down>" . smartscan-symbol-go-forward)
	      ("M-<up>" . smartscan-symbol-go-backward)))

(use-package openwith
  :ensure t
  :init
  :config
  (openwith-mode t)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf" "html" "svg" "doc" "docx" "xls" "xlsx" "xlsm" "ppt" "pptx" "pptm" "pzfx"))
               "open"
               '(file))
         )))

(use-package doc-view
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . doc-view-mode-maybe)
  :config
  (setq doc-view-continuous t))

(use-package smex
  :ensure t
  :config
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      ad-do-it))
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

(use-package writegood-mode
  :ensure t)

(use-package pdf-tools
  :ensure t
  :defer t
  :pin melpa-stable
  :config
  (pdf-tools-install))

(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  :bind (:map global-map
	      ("C-h z" . which-key-show-major-mode)))

(use-package goto-chg
  :ensure t
  :bind
  ("C-c l" . goto-last-change))

(use-package ivy
  :ensure t
  :diminish
  :preface
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)  
  (setq ivy-count-format "(%d/%d) ")
  ;; Polymode seems to have taken care of this so don't need to ignore these
  ;; buffer-specific modes. If I do in current Polymode, can't actually find the
  ;; file I have open.
  ;; (add-to-list 'ivy-ignore-buffers "\\[r\\]")
  ;; (add-to-list 'ivy-ignore-buffers "\\[poly-head-tail\\]")
  ;; (add-to-list 'ivy-ignore-buffers "\\[yaml\\]")
  ;; (add-to-list 'ivy-ignore-buffers "\\[latex\\]")
  ;; (add-to-list 'ivy-ignore-buffers "\\[fallback\\]")
  ;; (add-to-list 'ivy-ignore-buffers "\\[sas\\]")
  (set-face-attribute 'ivy-current-match nil :background "ns_selection_bg_color" :foreground "ns_selection_fg_color")
  :bind
  ("C-x b" . ivy-switch-buffer))

(use-package counsel
  :ensure t
  :diminish
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c r" . counsel-minibuffer-history)
  ("C-c M" . counsel-imenu))

;; TODO: Update refcard with this bit of C-o doing swiper from regular isearch
(use-package swiper
  :ensure t
  :after ivy
  :bind (:map isearch-mode-map
	      ("C-o" . swiper-from-isearch)))

(use-package projectile
  :diminish
  :init
  (setq projectile-project-search-path '("~/repos/"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind* ("C-c P" . (lambda () (interactive)
                      (projectile-cleanup-known-projects)
                      (projectile-discover-projects-in-search-path)))
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package ibuffer-projectile
  :ensure t
  :defer 1
  :after (ibuffer projectile))

(use-package ibuffer-vc
  :ensure t
  :defer 1
  :after (ibuffer))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  ("M-g M-c" . avy-goto-char-2)
  ("M-g M-g" . avy-goto-line))

(use-package anzu
  :ensure t
  :diminish
  :config
  (global-anzu-mode)
  :bind (:map global-map
	      ("M-%" . anzu-query-replace)
	      ("C-M-%" . anzu-query-replace-regexp)))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-lintr-linters "with_defaults(todo_comment_linter = NULL, trailing_blank_lines_linter = NULL, line_length_linter(80))")
  :hook ((elpy-mode ess-mode) . flycheck-mode)
  :commands
  (flycheck-mode
   flycheck-next-error
   flycheck-previous-error))

(use-package template
  :load-path (lambda () (xah-get-fullpath "lisp/template/lisp/"))
  :config
  (template-initialize)
  ;; TODO: Try to set default directory to one in dot-emacs repos!
  ;; Tried many ways and it doesn't work
  ;; (setq template-default-directories (xah-get-fullpath "lisp/template/templates/"))
  :bind
  ("C-c i" . template-expand-template))

(use-package yasnippet
  :ensure t
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (use-package stan-snippets
    :ensure t)
  (yas-global-mode 1)
  (add-to-list #'yas-snippet-dirs (xah-get-fullpath "lisp/yasnippet-ess/")))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

(use-package reveal-in-osx-finder
  :ensure t
  :bind (:map global-map
	      ("C-c o" . reveal-in-osx-finder)))

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish
  :init
  (setq company-minimum-prefix-length 2) ; do idle completion with at least 2 characters
  (setq company-selection-wrap-around t) ; wrap around during selection
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Customize company faces for dark background (ok for light background too)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package htmlize
  :ensure t
  :defer 10
  :pin melpa-stable)

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;
(use-package ein
  :ensure t
  :defer 5
  :commands ein:notebooklist-open
  :config
  ;; ideally would like this to be the same as polymode chunks which switch
  ;; when theme changes but this is good enough for now
  ;; (set-face-attribute 'ein:cell-input-area nil :background "#fd4ef3e3da54")
  )


(use-package matlab-mode
  :ensure t
  :defer 5
  :mode ("\\.m$" . matlab-mode)
  :preface
  (defun spa/matlab-insert-assign ()
    "Insert asignment operator for Matlab"
    (interactive)
    (just-one-space)
    (insert "=")
    (just-one-space))
  :config
  (load-library "matlab-load")
  (setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))
  (setq matlab-indent-level 4)
;; (matlab-cedet-setup)
;; (setq mlint-programs (quote ("/Applications/MATLAB_R2018b.app/bin/maci64/mlint")))
;; (add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))
;; (setq matlab-show-mlint-warnings t)
;; (setq matlab-highlight-cross-function-variables t)
  (add-hook 'matlab-mode-hook #'my-auto-hook)
   :bind (:map matlab-mode-map
               ("M--" . spa/matlab-insert-assign))) 
;; TODO: Does load but does NOT put function in Matlab mode map or set command switches as expected

(use-package ess
  :ensure t
  :pin melpa
  :init (require 'ess-site)
  :preface
  (defun spa/ess-insert-pipe (arg)
    "Insert pipe '%>%' operator, add a newline and indent. With prefix arg, don't add newline" 
    (interactive "P")
    (if arg
	(progn
	  (just-one-space)
	  (insert "%>%")
	  )
      (progn
	(just-one-space)
	(insert "%>%")
	;; (ess-newline-and-indent))
	(ess-roxy-newline-and-indent))))
  (defun spa/ess-insert-assign ()
    "Insert asignment operator for R"
    (interactive)
    (just-one-space)
    (insert "<-")
    (just-one-space))
  ;; TODO: Improve this by determining source of error messages
  ;; and automatically linking scratch R buffer to process
  (defun spa/R-scratch ()
    "Bring up a 'scratch' R script and console for quick calculations."
    (interactive)
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
	(R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name))
  :config
  (setq ess-smart-S-assign-key nil)
  ;; Don't ask me for a directory on startup
  (setq ess-ask-for-ess-directory nil)
  ;; Don't use ido so I can use ivy completion
  (setq ess-use-ido nil)
  ;; Would like to use flymake but it's not currently working
  (setq ess-use-flymake nil)
  ;; (setq ess-help-own-frame t)
  (setq ess-ask-about-transfile nil)
  ;; (setq inferior-ess-own-frame t) ; for 'dedicated' *R* buffers
  (setq inferior-R-args "--no-save") ; default is don't ask to save workspace
  (setq-default ess-dialect "R")
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  ;; ElDoc
  ;; This seemed to slow things down too much so disabled
  (setq ess-eldoc-show-on-symbol nil) ; shows function arguments even if not in ()
  (setq ess-plain-first-buffername nil)
  (setq ess-tab-complete-in-script t)
  ;; Modify the indentation style so that continued statements
  ;; like piping and adding operations only indent the first line
  ;; (add-to-list 'ess-style-alist
  ;;              '(my-style
  ;;                (ess-indent-level . 2)
  ;;                (ess-first-continued-statement-offset . 2)
  ;;                (ess-continued-statement-offset . 0)
  ;;                (ess-brace-offset . -4)
  ;;                (ess-expression-offset . 4)
  ;;                (ess-else-offset . 0)
  ;;                (ess-close-brace-offset . 0)
  ;;                (ess-brace-imaginary-offset . 0)
  ;;                (ess-continued-brace-offset . 0)
  ;;                (ess-arg-function-offset . 4)
  ;;                (ess-arg-function-offset-new-line . '(4))
  ;;                ))
  ;; (setq ess-default-style 'my-style)
  ;; Agressive indentations in ESS mode
  ;; (add-hook 'ess-mode-hook #'aggressive-indent-mode)
  ;; ;; For not substituting '_' with '->'
;;; ESS Roxygen customization to place nice with my function templates
  (setq ess-roxy-template-alist
	(list '("param" . "")
	      '("return" . "")))
  ;; Tried to fix e-mail address because @ gets parsed incorrectly
  ;; and it should be '@@' but too much work and it didn't work
  ;; (defun spa/fix-mail-address ()
  ;;   (save-excursion
  ;;     (while (search-backward "savey@its.jnj.com" nil t)
  ;;       (replace-match "savey@@its.jnj.com"))))
  ;; (add-hook 'ess-roxy-update-entry 'spa/fix-mail-address)
  ;; Set the font lock keywords to maximize font locking in ESS
  (setq ess-R-font-lock-keywords
	(quote
	 ((ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:bare-keywords . t)
	  (ess-R-fl-keyword:control-flow-keywords . t)
	  (ess-R-fl-keyword:signal-keywords . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-fl-keyword:fun-calls)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . t)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t))))
  ;; Toggle camel case / underscore / etc.
  (add-hook 'inferior-ess-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle)))
  (add-hook 'ess-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle)))
  ;; Unbind ess-display-help-on-object from C-c C-v since it's on multiple keys
  (add-hook 'ess-mode-hook
            '(lambda ()
               (local-unset-key (kbd "C-c C-v"))))
  ;; Get proper R program depending on local vs. server
  (add-hook 'ess-mode-hook
            '(lambda ()
	       ;; returns non-nil if buffer is under tramp connection (remote)
	       (if (file-remote-p default-directory)
		   (setq inferior-ess-r-program "/usr/bin/R")
		 (setq inferior-ess-r-program "/usr/local/bin/R"))))
  (add-hook 'inferior-ess-mode-hook
            '(lambda ()
	       ;; returns non-nil if buffer is under tramp connection (remote)
	       (if (file-remote-p default-directory)
		   (setq inferior-ess-r-program "/usr/bin/R")
		 (setq inferior-ess-r-program "/usr/local/bin/R"))))
  ;; ;; TODO Try to execute some code every time inferior R process starts but
  ;; ;; doesn't work here
  ;; (add-hook 'inferior-ess-mode-hook
  ;; 	    (ess-execute "utils::rc.options(funarg.suffix = \" = \")"))

  ;; Auto-completion using company
  (setq ess-use-company nil) ; don't auto-insert ess backends so I can define it custom below
  (defun comp-ess-config ()
    (make-variable-buffer-local 'company-backends)
    (add-to-list 'company-backends
		 '(company-R-args company-R-objects company-dabbrev-code :separate)))
  (add-hook 'ess-mode-hook #'comp-ess-config)
  ;; From jabranham's Emacs init file
  (defun spa/ess-beginning-of-pipe-or-end-of-line ()
    "Find point position of end of line or beginning of pipe %>%."
    (if (search-forward "%>%" (line-end-position) t)
        (goto-char (match-beginning 0))
      (end-of-line)))
  (defun spa/ess-eval-pipe-through-line (vis)
    "Like `ess-eval-paragraph' but only evaluates up to the pipe on this line.

If no pipe, evaluate paragraph through the end of current line.

Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
    (interactive "P")
    (save-excursion
      (let ((end (progn
                   (spa/ess-beginning-of-pipe-or-end-of-line)
                   (point)))
            (beg (progn (backward-paragraph)
                        (ess-skip-blanks-forward 'multiline)
                        (point))))
        (ess-eval-region beg end vis))))
  :bind (:map ess-mode-map
	      ("<backtab>" . ess-complete-object-name)
	      ("C-c M-c" . ess-eval-paragraph-and-go)
	      ("C-c C-\\" . spa/ess-eval-pipe-through-line)
	      ("M-=" . spa/ess-insert-pipe)
	      ("M--" . spa/ess-insert-assign)
	      ("C-c ?" . ess-help))
  :bind (:map inferior-ess-mode-map
	      ("M-r" . comint-history-isearch-backward)
	      ("C-u M-r" . comint-history-isearch-backward-regexp)
	      ("M-=" . spa/ess-insert-pipe)
  	      ("M--" . spa/ess-insert-assign)
	      ("C-c ?" . ess-help))
  :bind (:map global-map
	      ("C-x 9" . spa/R-scratch)))

;; (use-package ess-edit)
;; (load (xah-get-fullpath "lisp/ess-R-object-popup"))
(use-package ess-view
  :ensure t
  :diminish
  :init
  (setq ess-view--spreadsheet-program "open")
  :config
  ;; Redefine package function to be smart about getting the correct R process
  (defun ess-view-extract-R-process ()
    "Return the name of R running in current buffer."
    (let*
	((proc (ess-get-process))         ; Modified from (proc (get-buffer-process (current-buffer)))
	 (string-proc (prin1-to-string proc))
	 (selected-proc (s-match "^#<process \\(R:?[0-9]*\\)>$" string-proc)))
      (nth 1 (-flatten selected-proc))
      )
    ))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("\\`readme\\.md\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command "pandoc -c ~/.emacs.d/github-pandoc.css --from gfm --to html5+smart --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --highlight-style pygments --standalone --self-contained --quiet"))

(use-package pandoc-mode
  :ensure t
  :hook ((markdown-mode) . pandoc-mode))

(use-package quarto-mode)

(use-package poly-markdown
  ;; :pin melpa-stable
  :ensure t)

(use-package poly-R
  ;; :pin melpa-stable
  :ensure t)

(use-package poly-noweb
  ;; :pin melpa-stable
  :ensure t)

(use-package polymode
  :ensure t
  ;; :pin melpa-stable
  :after (poly-markdown poly-R poly-noweb)
  :init
  (require 'polymode-core)
  :mode
  ("\\.Rmd" . poly-markdown+r-mode)
  :preface
  (defvar pm/chunkmode)
  (defvar rmd-render-history nil "History list for spa/rmd-render.")
  (declare-function pm-map-over-spans "polymode-core")
  (declare-function pm-narrow-to-span "polymode-core")
  (defun spa/rmd-render (arg)
    "Render the current Rmd file to first output format in YAML header.

With a prefix arg, edit the R command in the minibuffer"
    (interactive "P")
    ;; Find the first output type and use that
    (save-excursion
      (beginning-of-buffer)
      (search-forward-regexp "^output[:]")
      (next-line)
      (setq output-format (thing-at-point 'line t))
      (setq output-format (s-replace ":\n" "" output-format))
      (setq output-format (replace-regexp-in-string
			   (rx (or (: bos (* (any " \t\n")))
                                   (: (* (any " \t\n")) eos)))
                           ""
                           output-format)))
    ;; Build the default R render command
    (setq rcmd (concat "rmarkdown::render('" buffer-file-name "',"
		       "output_dir = '../reports',"
		       "output_format = '" output-format "')"))
    ;; Check for prefix argument
    (if arg
	(progn
	  ;; Use last command as the default (if non-nil) 
	  (setq prev-history (car rmd-render-history))
	  (if prev-history
	      (setq rcmd prev-history)
	    nil)
	  ;; Allow the user to modify rcmd 
	  (setq rcmd
		(read-from-minibuffer "Run: " rcmd nil nil 'rmd-render-history))
	  )
      ;; With no prefix arg, add default rcmd to history
      (setq rmd-render-history (add-to-history 'rmd-render-history rcmd)))
    ;; Build and evaluate the shell command
    (setq command (concat "echo \"" rcmd "\" | R --vanilla"))
    (compile command))
  ;; TODO: Debug this for edge cases. Usually this works ok
  (defun spa/rmd-run ()
    "Start a Shiny server for the given R markdown document, 
   and render it for display."
    (interactive)
    (save-excursion
      (setq orig-buffer (buffer-name))
      ;; Build the R run command
      (setq rcmd (concat "rmarkdown::run('" buffer-file-name "')"))
      ;; Change buffer name to [r] buffer associated with ESS process
      (if (string-match-p "\[r\]" orig-buffer)
	  (setq r-buf-name orig-buffer)
	(setq r-buf-name (concat orig-buffer "[r]")))
      (switch-to-buffer r-buf-name)
      ;; Send the R command to the R process
      (setq process (ess-get-process))
      ;; Interrupt current R process
      (switch-to-buffer (process-buffer process))
      (interrupt-process)
      (switch-to-buffer orig-buffer)
      (ess-send-string process rcmd t)))
  (defun spa/rmd-eval-chunk-and-step ()
    "Evaluate current R chunk and move point to next chunk."
    (interactive)
    (polymode-eval-chunk (point))
    (search-forward "```")
    (search-forward "```")
    (forward-line))
  (defun spa/insert-r-code-chunk (arg)
    "Insert R Markdown code chunk. With prefix arg, read in chunk header contents"
    (interactive "P")
    (if arg
	(progn
	  (setq contents (read-from-minibuffer "Chunk Header: " nil nil nil))
	  (setq str (concatenate 'string  "```{" contents "}\n"))
	  (insert str)
	  )
      (progn
	(insert "```{r}\n")))
    (insert "\n")
    (save-excursion
      (insert "\n")
      (insert "\n")
      (insert "```\n")))
  :config
  ;; NOTE. Can use M-n v v to eval current chunk (or chunks in active region)
  ;; but does not step. Can use M-n v b to eval all chunks in buffer, M-n v u/d
  ;; to evaluate all code before (u) or start at point and evaluate until end of
  ;; code (d).
  (define-key poly-markdown+R-mode-map (kbd "C-c C-f")  'spa/rmd-eval-chunk-and-step)
  (define-key markdown-mode-map (kbd "C-c C-a c") 'spa/insert-r-code-chunk)
  :bind (:map polymode-minor-mode-map
	      ("C-c r" . spa/rmd-render)
	      ("C-c s" . spa/rmd-run)))

(use-package elpy
  :ensure t
  :pin melpa-stable
  :init
  (setq elpy-shell-use-project-root nil)
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook (lambda () (elpy-shell-set-local-shell
				   (elpy-project-root))))
  ;;  Switch to flycheck instead of flymake for real-time syntax checking
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; Set the default shell interpreter to Jupyter for interactive Python
  (setq elpy-rpc-python-command "python3")
  ;; (setq elpy-shell-echo-output nil)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))
;; ;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(use-package yaml-mode
  :ensure t
  :defer 1
  :mode
  ("\\.yml\\'" . yaml-mode))

(use-package stan-mode
  :ensure t
  :defer 2
  :pin melpa-stable
  :mode
  ("\\.stan\\'" . stan-mode))

(use-package s3ed
  :ensure t
  :init
  (setq s3ed-profile-name "itx-acd")
  :bind (:map global-map
	      (("C-c C-r f" . s3ed-find-file)
	       ("C-c C-r s" . s3ed-save-file))))

;; ;; Emacs X Window Manager
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)

;; (use-package exwm
;;   :ensure t)
;; (use-package exwm-config
;;   :ensure t
;;   :config
;;   (exwm-config-example))


(use-package auto-complete
  :ensure t
  :pin melpa-stable)

(use-package speed-type
  :ensure t
  :pin melpa-stable)

;; Minor mode for typing speed
(load (xah-get-fullpath "lisp/typing-speed/typing-speed.el"))
