;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_vanilla_settings.el                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aesthetics
(set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 160 :width 'normal :foundry "nil" :family "Hack")
(set-face-attribute 'secondary-selection nil :background "PaleTurquoise2")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in Modes                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show matching parentheses
(show-paren-mode 1)

;; overwrite selected text
(delete-selection-mode t)

;; column-number-mode
;; show row and column in status bar
(column-number-mode 1)                  

(use-package abbrev
  :defer 5
  :diminish
  :config
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq default-abbrev-mode t))

(use-package prog-mode
  :after (auto-complete)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

  (add-hook 'prog-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle))))

(use-package dired
  :diminish dired-omit-mode
  :preface
  (defun spa/dired-find-file-follow-symlinks ()
    "In Dired, visit the file or directory on the line, following symlinks

Source: `https://emacs.stackexchange.com/questions/41286/follow-symlinked-directories-in-dired'"
    (interactive)
    (let ((find-file-visit-truename t))
      (dired-find-file)))
  (defun dired-do-ispell (&optional arg)
    "Check multiple buffers or files marked in dired for spelling with ispell"
    (interactive "P")
    (dolist (file (dired-get-marked-files
                   nil arg
                   #'(lambda (f)
                       (not (file-directory-p f)))))
      (save-window-excursion
	(with-current-buffer (find-file file)
          (ispell-buffer)))
      (message nil)))
  (defun spa/dired-open-externally ()
    "Run `open` on Mac OS X to open the file externally (e)."
    (interactive)
    (dired-do-shell-command "open" nil (dired-get-marked-files)))
  (defun spa/tags-query-replace (from to &optional delimited file-list-form)
    "Do `query-replace' of FROM with TO on all files listed in tags table.                     
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.                  
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace                  
with the command \\[tags-loop-continue].                                                     
Fourth arg FILE-LIST-FORM non-nil means initialize the replacement loop.                     
Fifth and sixth arguments START and END are accepted, for compatibility                      
with `query-replace', and ignored.                                                           

If FILE-LIST-FORM is non-nil, it is a form to evaluate to
produce the list of files to search.

See also the documentation of the variable `tags-file-name'.

Source: `http://stackoverflow.com/questions/15038277/find-and-replace-without-regexp-in-dired'"
    (interactive (query-replace-read-args "Tags query replace" nil t))
    (require 'etags)
    (setq tags-loop-scan `(let ,(unless (equal from (downcase from))
                                  '((case-fold-search nil)))
                            (if (search-forward ',from nil t)
				;; When we find a match, move back
				;; to the beginning of it so perform-replace
				;; will see it.
				(goto-char (match-beginning 0))))
          tags-loop-operate `(perform-replace ',from ',to t nil ',delimited
                                              nil multi-query-replace-map))
    (tags-loop-continue (or file-list-form t)))
(defun spa/dired-do-query-replace (from to &optional delimited)
  "Do `query-replace' of FROM with TO, on all marked files (no regexp).
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

Source: `http://stackoverflow.com/questions/15038277/find-and-replace-without-regexp-in-dired'"
  
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace in marked files" nil t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (require 'dired-aux)
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
                        buffer-read-only))
          (error "File `%s' is visited read-only" file))))
  (spa/tags-query-replace
   from to delimited '(dired-get-marked-files nil nil 'dired-nondirectory-p)))
  :config
  ;; To copy from one dired dir to another shown in split window
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;; Can use `C-u s` in dired to modify switches
  ;; -S option to sort by file size, largest first
  ;; -X option to sort alphabetically by extension
  (setq dired-listing-switches "-AlhG")
  :bind (:map dired-mode-map
	      ("<C-return>" . spa/dired-find-file-follow-symlinks)
	      ("C-c s" . dired-do-ispell)
	      ("e" . spa/dired-open-externally)
	      ("C-c Q" . spa/dired-do-query-replace)))
  
(use-package auto-revert
  :no-require t
  :hook ((dired-mode doc-view-mode) . auto-revert-mode)
  :config
  (setq auto-revert-verbose nil))
  
(use-package winner
  :unless noninteractive
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  :bind (("C-c C-v" . ediff-revision)))

(use-package tramp
  :defer 5
  :preface
  (defun spa/find-file-biostat ()
  "Calls find-file on filename on remove server using TRAMP.

Requires a password"
  (interactive)
  (find-file (read-file-name
  	      "Find File on BiostatShiny: "
  	      "/ssh:savey@biostatshiny.jnj.com:~")))
  :config
  (setq password-cache-expiry 28800)
  ;; Set Bash shell to /bin/bash since only log into Linux machines
  (setq explicit-shell-file-name "/bin/bash") 
  :bind (("C-x C-g" . spa/find-file-biostat)))

(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :defer 15
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args   '("-d en_US"))))

    ;; Save a new word to personal dictionary without asking
    (setq ispell-silently-savep t))
  :bind
  ;; Similar binding to flyspell's C-. to make it easy to remember
  ;; even though this overwrites a global binding
  ("C-M-." . ispell-word))

(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :after auto-complete
  :preface
  ;; Flyspell signals an error if there is no spell-checking tool is
  ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
  ;; to try to enable flyspell only if a spell-checking tool is available.
  (defun modi/ispell-not-avail-p (&rest args)
    "Return `nil' if `ispell-program-name' is available; `t' otherwise."
    (not (executable-find ispell-program-name)))  
  :hook
  ((text-mode org-mode) . turn-on-flyspell)
  ((prog-mode) . flyspell-prog-mode)
  :config
  (ac-flyspell-workaround)
  (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
  (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p))

(use-package prettify-symbols-mode
  ;; Can modify `ess-r-prettify-symbols` to add additional symbols
  :hook
  ((prog-mode ess-mode inferior-ess-mode) . prettify-symbols-mode))

(use-package calendar
  :init
  (setq calendar-latitude 40.231846)
  (setq calendar-longitude -75.252461)
  (setq calendar-location-name "North Wales, PA")
  (setq calendar-time-zone -300)
  (setq calendar-standard-time-zone-name "EST")
  (setq calendar-daylight-time-zone-name "EDT"))

(use-package auto-complete
  :defer 15
  :preface
  (defun my-auto-hook ()
    (auto-complete-mode 1)
    (define-key ac-completing-map [return] nil)
    (define-key ac-completing-map "\r" nil))
  :hook
  ((prog-mode ess-mode inferior-ess-mode) . (my-auto-hook)))

;; TODO: This isn't getting turned on in ESS modes
(use-package subword-mode
  :hook
  ((ess-mode-hook inferior-ess-mode-hook) . subword-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skip splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Turn off tool bar mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; disable scroll bars in GUI and menu in terminal
(if window-system
    (scroll-bar-mode -1)
  (progn
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    )
  )

;; Sort results from apropos by relevance
(setq apropos-sort-by-scores t)

;; Don't warn about opening large files! I know they are large, yes I want you
;; to open them, that is why I asked you to open it.
(setq large-file-warning-threshold nil)

;; use hippie-expand instead of dabbrev
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Set the fill column to be 80 by default
(setq-default fill-column 80)

;; Better wild cards in search
(setq search-whitespace-regexp ".*?")

;; Set these so that `shell-command` and `compile` understand aliases in .bashrc
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; Location Settings
(setq display-time-world-list (quote
			       (("America/Los_Angeles" "La Jolla, US")
				("America/New_York" "Spring House, US")
				("Europe/London" "London, UK")
				("Europe/Brussels" "Beerse, BE")
				("Asia/Shanghai" "Shanghai, CN")
				("Asia/Tokyo" "Tokyo, JP"))))

;; Add the system clipboard to the Emacs kill-ring
;; (setq save-interprogram-paste-before-kill t)

;; compilation
(setq compilation-scroll-output 'first-error)

(setq trash-directory "~/.Trash")
