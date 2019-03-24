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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle between most recent buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/SwitchingBuffers#toc5
(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;; ;; set key binding

;; Transpose two buffers
;; function from http://www.emacswiki.org/emacs/SwitchingBuffers#toc6
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete file and buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source: http://tuxicity.se/emacs/elisp/2010/11/16/delete-file-and-buffer-in-emacs.html
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;
;; TODO: Move this!
;; Set correct parser for tex mode
(add-hook 'tex-mode-hook
	  #'(lambda () (setq ispell-parser 'tex)))

;; Start comments with 2 `%` characters in LaTeX mode
;; (setq comment-add 1)
(add-hook 'text-mode-hook (lambda () (setq-local comment-add 1)))

;; Word count in LaTeX
(defun latex-word-count ()
  (interactive)
  (shell-command (concat dot-emacs-path "/src/texcount.pl "
			 ;; "optional arguments" ;
			 (buffer-file-name))))

;; Word count master file in LaTeX
(defun latex-word-count-master ()
  (interactive)
  (shell-command (concat dot-emacs-path "/src/texcount.pl"
			 " -inc " ; include /input and /include
			 (buffer-file-name))))

;; Word count in LaTeX
;; Try to pass in command arguments, not sure
;; (defun latex-word-count-2 (&optional args)
;;   (interactive "S")
;;   (shell-command (concat dot-emacs-path "/src/texcount.pl "
;; 			 args
;; 			 (buffer-file-name))))

;; Turn off auto-fill-mode in text mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
;; (add-hook 'text-mode-hook (auto-fill-mode -1))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;
;; Just-one-space ;;
;;;;;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R-specific helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spa/plot-to-file-or-not ()
  "(Un)comment out the command to write to a pdf file. Comments will be toggled on the lines with matches to pdf(, Pause( and dev.off("
  (interactive)
  (spa/comment-or-uncomment-strings "pdf(" "Pause(" "dev.off(" )
  )


(defun spa/debug-pipeline ()
  "Wrapper function to call `pipecleaner::debug_pipeline()` on
the highlighted region"
  (interactive)
  ;; If there is a valid active region, run debug_pipeline on it. NOTE: Doesn't
  ;; currently check for pipes or valid code so requires user knows what they're
  ;; doing.
  (if (use-region-p)
      (progn
	(setq region (buffer-substring (mark) (point)))
	(setq command (concat "pipecleaner::debug_pipeline(\n" region "\n)"))
	(ess-execute command 'buffer)
	(ess-switch-to-inferior-or-script-buffer nil))
    (warn "`spa/debug-pipeline()` requires a marked region")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box	      ;;
;; from http://irreal.org/blog/?p=374 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spa/comment-box (b e &optional arg)
  "Draw a box comment around the region but arrange for the region to extend to at least the fill column (exactly if possible). Place the point after the comment box."
  (interactive "*r\np")
  (when (not (region-active-p))
    (setq b (point-at-bol))
    (setq e (point-at-eol)))
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (+ (current-column) (+ 3 (* arg 2)))))
    (comment-box b e arg)
    (goto-char e)
    (set-marker e nil)))


;; Pair of functions that allows me to comment out the next call to pdf( ) ... dev.off() to change
;; a figure from being written to a file to being written to the standard device and vice versa

(defun spa/comment-or-uncomment-strings (s1 s2 s3)
  "(Un)comment each line matching the strings using regexp"
  (interactive "sString1: \nsString2 \nsString3: ")
  ;; Get point of current cursor and search forward in file for 'pdf(' string                          
  ;; comment/uncomment that region smartly                                                             
  (save-excursion
    (let (p1 p2 p3)
      (search-forward-regexp s3)
      (setq p3 (line-beginning-position))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (search-backward-regexp s1) ; search backwards for s1                                            
      (setq p1 (line-beginning-position))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      ;; Must reset p3 to use in if statement as a bound because position might                        
      ;; change after adding comments!                                                                 
      (search-forward-regexp s3)
      (setq p3 (line-beginning-position))
      (search-backward-regexp s1) ; search backwards for s1                                            
      ;; if s2 is not found between s1 and s3, no commenting/uncommenting is done                      
      (if (search-forward-regexp s2 p3 t nil) ; return nil if s2 not found before s3                   
          (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
      )
    )
  )

;; Test whether string is an integer                                                                   
(defun string-integer-p (string)
  (if (string-match "\\`[-+]?[0-9]+\\'" string)
      t  
    nil))

;; Return all matches to regexp in a string (e.g. that returned by buffer-string) as a list
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)
    )
)

(defun pos-at-line-col (l c)
  (save-excursion
    (goto-char (point-min))
    (forward-line l)
    (move-to-column c)
    (point)))

(defun spa/getWhatIWant ()
  (let (term)
    (cond
     ((region-active-p) (setq term (buffer-substring (mark) (point))))
     ((thing-at-point 'symbol) (setq term (thing-at-point 'symbol)))
     ((thing-at-point 'word) (setq term (thing-at-point 'word)))
     (t (setq term (thing-at-point 'line))))
    term))

(defun spa/wiki ()
  "Opens a browser to view Wikipedia page for the search term near the point"
  (interactive)
  (message (concat "Searching Wikipedia for: " (spa/getWhatIWant)))
  (browse-url (concatenate 'string "https://en.wikipedia.org/wiki/Special:Search?search=" (spa/getWhatIWant))))

;; (defun spa/open-finder()
;;   "Open the `finder' file browser with the contents of the current path"
;;   (interactive)
;;   (call-process-shell-command (concatenate 'string "open " "'" (file-truename default-directory) "'" " &"))
;; )

;; Function to get the full path name of the current buffer
;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name)))
  (message "Copied path to kill ring"))

;; browse-kill-ring if the last command wasn't a yank when running M-y
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

(ad-activate 'yank-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill line if no region active ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs-fu.blogspot.co.uk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swap single with double quotes or vice-versa ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source: http://www.masteringemacs.org/articles/2014/08/26/swapping-quote-symbols-emacs-parsepartialsexp/

(defun point-in-string-p (pt)
  "Returns t if PT is in a string"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun beginning-of-string ()
  "Moves to the beginning of a syntactic string"
  (interactive)
  (unless (point-in-string-p (point))
    (error "You must be in a string for this command to work"))
  (while (point-in-string-p (point))
    (forward-char -1))
  (point))

(defun swap-quotes ()
  "Swaps the quote symbols in a string"
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (beginning-of-string)))
          (eos (save-excursion
                 (beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
          (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun spa/insert-common-name()
  (interactive)
  (set 'names (read-lines "~/.emacs.d/contacts/common_names.txt"))
  (setq name (ivy-completing-read "Name: " names))
  (insert name)
  )

;; Define a global variable which holds a hash table between common names and
;; e-mails. Note that common name is NOT unique so it IS dangerous to use this
;; function without discretion because hash values with the same key will
;; overwrite previous values with new e-mails
(defun spa/make-name-hash()
  (setq key-value-pairs (read-lines "~/.emacs.d/contacts/common_name_email.txt"))
  ;; (setq key-value-pairs (read-lines "~/.emacs.d/contacts/temp.txt"))  
  (setq my-hash (make-hash-table :size (/ (length key-value-pairs) 2) :test 'equal))
  ;; While loop to populate the hash table
  (setq num 0)
  (while (< num (length key-value-pairs))
    (puthash (nth num key-value-pairs) (nth (1+ num) key-value-pairs) my-hash)
    (setq num (+ num 2)))
  my-hash)
(setq spa/name-email-hash (spa/make-name-hash))

;; See above not about how this is dangerous if used without discretion. Would
;; be better to use Swiper on the file itself in order to see multiple matching
;; entries.
(defun spa/kill-email()
  (interactive)
  (setq names (read-lines "~/.emacs.d/contacts/common_names.txt"))
  (setq name (ivy-completing-read "Name: " names))
  (setq email (gethash name spa/name-email-hash))
  (kill-new email)
  (message "Copied %s\'s email: %s" name email))

;; Search company acronyms stored in a TSV file using swiper
(defun spa/acro-search()
  (interactive)
  ;; save excursion to return windows to current state after function executes
  (save-window-excursion
    ;; Search for active region or prompt user for acronym to search for
    (cond
     ((region-active-p) (setq acro (buffer-substring (mark) (point))))
     (t (setq acro (read-from-minibuffer "Acronym: " nil nil nil 'minibuffer-history))))
    (setq str (concatenate 'string "^" acro))
    ;; Open a buffer or create a new buffer and insert the acronym file contents
    (if (get-buffer "*acronyms*")
	(pop-to-buffer "*acronyms*")
      (progn
	(pop-to-buffer "*acronyms*")      
	(insert-file-contents "~/.emacs.d/acronyms/acronyms.txt")))
    ;; Prevent any accidental changes to the buffer
    (read-only-mode)
    ;; Use swiper to search for the acronym
    (swiper str)))		  

;; Run htop in ansi-term
(defun spa/htop()
  (interactive)
  (if (get-buffer "*htop*")
      (progn
	(pop-to-buffer "*htop*")
	(if (not (process-running-child-p "*htop*"))
	    (process-send-string "*htop*" "htop\n")))
    (progn
      (ansi-term "/bin/bash" "htop")
      (process-send-string "*htop*" "htop\n"))))
