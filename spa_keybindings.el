;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_keybindings.el                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; Occur ;;
;;;;;;;;;;;
(keymap-global-set "C-c C-o o" 'occur)
(keymap-global-set "C-c C-o m" 'multi-occur-in-matching-buffers)

(keymap-global-set "C-M-]" 'indent-region)
(keymap-global-set "C-c m" 'man)

;; Insert Pair of quotes
;; Use M-1 M-" to wrap current word in quotes
(keymap-global-set "M-\"" 'insert-pair)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions ;;
;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-c A" 'spa/acro-search)
(keymap-global-set "C-c n" 'spa/insert-common-name)
(keymap-global-set "C-cw" 'swap-quotes)
(keymap-global-set "C-x p" 'copy-full-path-to-kill-ring)
;; (keymap-global-set "C-c o" 'spa/open-finder)
(keymap-global-set "C-c e" 'spa/wiki)
(keymap-global-set "C-c b n" 'comment-box)
(keymap-global-set "C-c b w" 'spa/comment-box)
(keymap-global-set "C-c f" 'spa/plot-to-file-or-not)
(keymap-global-set "C-c C-xs" 'just-one-space-in-region)
(keymap-global-set "C-c L" 'org-toggle-latex-fragment)
(keymap-global-set "C-c k" 'delete-this-buffer-and-file)
(keymap-global-set "C-c t" 'transpose-buffers)
;; (keymap-global-set "C-`" 'switch-to-previous-buffer)

