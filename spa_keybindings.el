;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_keybindings.el                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; Occur ;;
;;;;;;;;;;;
(global-set-key (kbd "C-c C-o o") 'occur)
(global-set-key (kbd "C-c C-o m") 'multi-occur-in-matching-buffers)

(global-set-key "\C-\M-]" 'indent-region)
(global-set-key (kbd "C-c m") 'man)

;; Insert Pair of quotes
;; Use M-1 M-" to wrap current word in quotes
(global-set-key (kbd "M-\"") 'insert-pair)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions ;;
;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c A") 'spa/acro-search)
(global-set-key (kbd "C-c n") 'spa/insert-common-name)
(global-set-key "\C-cw" 'swap-quotes)
(global-set-key (kbd "C-x p") 'copy-full-path-to-kill-ring)
;; (global-set-key (kbd "C-c o") 'spa/open-finder)
(global-set-key (kbd "C-c e") 'spa/wiki)
(global-set-key (kbd "C-c b n") 'comment-box)
(global-set-key (kbd "C-c b w") 'spa/comment-box)
(global-set-key (kbd "C-c f") 'spa/plot-to-file-or-not)
(global-set-key "\C-c\C-xs" 'just-one-space-in-region)
(global-set-key (kbd "C-c L") 'org-toggle-latex-fragment)
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)
(global-set-key (kbd "C-c t") 'transpose-buffers)
;; (global-set-key (kbd "C-`") 'switch-to-previous-buffer)

