;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_functions.el                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; Writing Style ;;
;;;;;;;;;;;;;;;;;;;
;; Send selection to GNU Style
(defun spa/run-style (&optional start end)
  (interactive "r")
    (if (get-buffer "*Style*")
	(kill-buffer "*Style*")
      nil)
    (shell-command-on-region start end "style" "*Style*")
    (switch-to-buffer "*Style*")
    (read-only-mode)
    (switch-to-previous-buffer))

;;;;;;;;;;;
;; ediff ;;
;;;;;;;;;;;
(defun command-line-ediff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

;; Add a command line switch to run ediff on 2 files
;; Usage: emacs -ediff file1 file2
(add-to-list 'command-switch-alist '("ediff" . command-line-ediff))

;; Run ediff on a pair of marked files in (multiple) dired buffers
(defun spa/ediff-marked-pair ()
  "Run ediff-files on a pair of files marked in dired buffer"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          (t (error "mark exactly 2 files, at least 1 locally")))))
;; TODO: Move keybinding and/or function to dired
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "E") 'spa/ediff-marked-pair))

;;;;;;;;;;
;; sudo ;;
;;;;;;;;;;
;; Excerpt From: Mickey Petersen. “Mastering Emacs.”
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
