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

;;;;;;;;;;;;;;;;
;; Aesthetics ;;
;;;;;;;;;;;;;;;;
;; Turn minor mode "default text scale" on in order to globally adjust text size
;; with C-M-= and C-M--
(use-package default-text-scale
  :ensure t
  :config (default-text-scale-mode t))

(use-package smart-mode-line
  :ensure t
  :init (add-hook 'after-init-hook 'sml/setup)
  :config
  (setq sml/theme 'respectful)
  (setq sml/modified-char "m")
  (add-to-list 'sml/replacer-regexp-list '("^~/Box Sync/" ":Box:") t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :after (smart-mode-line)
  :init
  (setq custom-safe-themes t) ;; treat all themes as safe
  :config
  (color-theme-sanityinc-solarized--define-theme light)
  (load-theme 'smart-mode-line-respectful))

(use-package emojify
  :ensure t
  :defer 15
  :hook ((text-mode org-mode) . global-emojify-mode)
  :config
  (setq emojify-program-contexts '(comments))
  :bind ("C-c E" . emojify-insert-emoji))

;; dired-x is not available?
(use-package dired-x
  :disabled t
  :ensure t
  :after dired)

(use-package color)
(use-package cl)

(use-package rainbow-delimiters
  :ensure t
  :after (color cl)
  :preface
  ;; Better rainbow colors (source: https://emacs.stackexchange.com/questions/21303/looking-for-a-better-way-of-tweaking-rainbow-delimiters)
  (defun hsl-to-hex (h s l)
  "Convert H S L to hex colours."
  (let (rgb)
    (setq rgb (color-hsl-to-rgb h s l))
    (color-rgb-to-hex (nth 0 rgb)
                      (nth 1 rgb)
                      (nth 2 rgb))))
  (defun bracket-colors ()
  "Calculate the bracket colours based on background."
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
  (defun colorise-brackets ()
  "Apply my own colours to rainbow delimiters."
  (interactive)
  (require 'rainbow-delimiters)
  (custom-set-faces
   ;; or use (list-colors-display)
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#888"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,(nth 0 (bracket-colors))))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,(nth 1 (bracket-colors))))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,(nth 2 (bracket-colors))))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,(nth 3 (bracket-colors))))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,(nth 4 (bracket-colors))))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,(nth 5 (bracket-colors))))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,(nth 6 (bracket-colors))))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,(nth 7 (bracket-colors))))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "red"))))
   ))
  :config
  (colorise-brackets)			; color brackets as well as parentheses
  :hook ((prog-mode ess-mode inferior-ess-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode ess-mode) . rainbow-mode))

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
  :config
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
  ;; Refile
  (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
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
  ;; Capture time stamps when TODO states change
  (setq org-log-done 'time)
  ;; '@' sign means that I want to log a note with time
  ;; stamp when the state changes
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "|" "DONE(d)")
		(sequence "REPLAY(r)" "|" "REPLAYED(p)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")
		(sequence "RUNNING(R@)" "ERROR(E@)" "|"  "FINISHED(F@)")	      )))

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("REPLAY" :foreground "red" :weight bold)
		("REPLAYED" :foreground "forest green" :weight bold)		
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold)
		("PHONE" :foreground "forest green" :weight bold))))
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
  :hook
  (org-indent-mode . spa/diminish-org-indent)
  (org-mode . turn-on-font-lock)
  (org-mode . visual-line-mode)
  (org-agenda-mode . hl-line-mode)
  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      )
  (:map org-mode-map
	;; ("C-c b" . org-iswitchb)	; now using ido-mode for buffer switching
	("C-c l" . org-store-link)
	("C-c c" . org-capture)
	("C-c C-f" . org-forward-heading-same-level)
	("C-c s" . 'spa/org-link-screenshot)))

(use-package org-clock-convenience
  :ensure t
  :after (org)
  :init
  (setq org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\): *\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)? +Clocked: +\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
  :bind (:map org-agenda-mode-map
   	   ("<S-up>" . org-clock-convenience-timestamp-up)
   	   ("<S-down>" . org-clock-convenience-timestamp-down)
   	   ("o" . org-clock-convenience-fill-gap)
   	   ("e" . org-clock-convenience-fill-gap-both)))

(use-package ox-pandoc
  :ensure t
  :after (org))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :after (org))

(use-package google-this
  :ensure t
  :bind
  ("C-c C-g" . google-this))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
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
  :bind (("M-<down>" . smartscan-symbol-go-forward)
	 ("M-<up>" . smartscan-symbol-go-backward)))

(use-package openwith
  :disabled t
  :ensure t
  :config (openwith-mode t))

(use-package doc-view
  :ensure t
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode-maybe))
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
  :config
  (pdf-tools-install))

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;
(use-package ein
  :ensure t)

(use-package matlab-mode
  :ensure t
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
  :init (require 'ess-site)
  :config
  ;; Don't ask me for a directory on startup
  (setq ess-ask-for-ess-directory nil)
  ;; Use ido mode for ESS
  (setq ess-use-ido t)
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
  :bind (:map ess-mode-map
	      ("<backtab>" . ess-complete-object-name)
	      ("C-c M-c" . ess-eval-paragraph-and-go))
  :bind (:map inferior-ess-mode-map
	      ("M-r" . comint-history-isearch-backward)
	      ("C-u M-r" . comint-history-isearch-backward-regexp)))

;; (use-package ess-edit)
;; (load (xah-get-fullpath "lisp/ess-R-object-popup"))
;; Issue 8 version
(load (xah-get-fullpath "lisp/ess-view.el"))
(setq ess-view--spreadsheet-program "open")


