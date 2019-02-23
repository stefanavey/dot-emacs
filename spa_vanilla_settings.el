;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_vanilla_settings.el                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; abbrev-mode
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...
(setq default-abbrev-mode t)

;;;;;;;;;;;;;;;
;; prog-mode ;;
;;;;;;;;;;;;;;;
;; highlights keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; Turn on string inflection cycling in programming modes
(add-hook 'prog-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-u") 'string-inflection-cycle)))

;;;;;;;;;;;;;;;;
;; dired-mode ;;
;;;;;;;;;;;;;;;;
;; To copy from one dired dir to another shown in split window
(setq dired-dwim-target t)
(setq delete-by-moving-to-trash t)
;; (setq dired-listing-switches "-alh")
(setq dired-listing-switches "-AlhG")
;; Add -S option to sort by file size, largest first (C-u s in dired)
;; Add -X option to sort alphabetically by extension
(setq trash-directory "~/.Trash")

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq auto-revert-verbose nil)		; make it quiet

;; Add on package to get recursive sizes 'in place'
;; Use `M-x dired-du-mode` to turn this on
;; (add-hook 'dired-mode-hook #'dired-du-mode)
(setq dired-du-size-format t)

;; Use ls-lisp to emulate lisp since ls on mac does not support dired
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;;;;;;;;;;;;;;;;;
;; Winner Mode ;;
;;;;;;;;;;;;;;;;;
;; Remember window configurations
(winner-mode 1)

;;;;;;;;;;;
;; ediff ;;
;;;;;;;;;;;
(setq ediff-split-window-function 'split-window-horizontally)

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
