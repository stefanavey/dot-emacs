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

