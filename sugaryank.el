;;;
;; Funny Modify
;;;

;;; Commentary:
;;
;; Funny Modify (Fmodify) has two major modes to modify
;; text. `edit-mode' and `overwrite-mode'.
;;
;; Edit mode (replace)
;; ---------
;; C-c C-i   Insert modified text
;; C-c C-c   Insert modified text
;; C-c C-q   Quit fmodify mode
;;  o        To overwrite-mode
;;  r        show prompt for replace commands
;; [return]  replace region (if mark is active), or query replace
;; M-RET     query replace with regexp
;;  .        set mark
;;  
;;  ;        query replace
;;  :        query replace with regexp
;;  ,        query replace in the region
;;  +        replace string
;;  *        replace regexp
;;  m        replace only math number
;;
;;  c        capitalize word
;;  u        upcase word
;;  l        downcase word
;;
;;  d        delete character
;;  h        backward delete character with untabify
;;  D        delete word
;;  k        delete line after the point
;;  z        zap to character
;;
;;
;; Overwrite mode
;; ---------
;; C-c C-i   Insert modified text
;; C-c C-c   Insert modified text
;; C-c C-q   Quit fmodify mode
;; C-c C-e   To edit-mode
;;

(defvar fmodify-replace-copied-text nil
  "*t replace copied text modified one.
nil just append modified text to kill-ring.")

(defvar fmodify-default-mode 'edit
  "*Default mode in fmodify buffer.
symbols are 'edit or 'overwrite.")

(defvar fmodify-modify-buffer " *modify*"
  "Name of modify buffer.")


;;;###autoload
(defun fmodify-mode ()
  "Major mode for edit copied text.
fmodify has two major mode; Edit mode and Overwrite mode.

If called directly, modified text is the last kill-ring element."
  (interactive "*")
  ;; Case of fmodify called directly; Without fcopy.
  ;; And if kill-ring is nil, then error message displays.
  (if (null kill-ring)
      (error "kill-ring is empty, Funny Modify doesn't work."))

  (if (fcopy-called-interactively-p)
      (setq fcopy-window (current-window-configuration)
	    fcopy-point  (point)))
  (let ((mode fmodify-default-mode))
    (fmodify-pop-to-buffer)
    (cond
     ((eq mode 'edit)        (fmodify-edit-mode))
     ((eq mode 'overwrite)   (fmodify-overwrite-mode))
     (t  (unwind-protect
	     (error "Funny Modify failed.  There is no text in kill-ring.")
	   (fcopy-exit))))))

(defun fmodify-pop-to-buffer ()
  (pop-to-buffer fmodify-modify-buffer)
  (erase-buffer)
  (insert (car kill-ring))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  ;; resize window
  (let* ((lines  (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (diff   (- height lines))
	 (min    (1+ window-min-height)))
    (if (< lines height)
	(if (< min diff)
	    (enlarge-window (- min height))
	  (enlarge-window (- diff))))))

(defun fmodify-insert ()
  "Insert modified text to original position where Funny Copy is done."
  (interactive)
  (fcopy-append-to-kill-ring (point-min) (point-max)
			     fmodify-replace-copied-text)
  (fcopy-insert))

(defun fmodify-quit ()
  "Exit fmodify mode."
  (interactive)
  (if (equal (buffer-name (current-buffer)) fmodify-modify-buffer)
      (kill-buffer fmodify-modify-buffer))
  (fcopy-exit))


;;; Edit mode

(defvar fmodify-edit-mode-map nil "Used in fmodify-edit mode")
(if fmodify-edit-mode-map
    nil
  (let ((map (make-keymap))
	(key ?!))
    ;; destroy default keymap
    (while (<= key ?~)
      (define-key map
	(char-to-string key) 'undefined)
      (setq key (1+ key)))
    (define-key map "\C-i"     'fmodify-insert)
    (define-key map "\C-c\C-i" 'fmodify-insert)
    (define-key map "\C-c\C-c" 'fmodify-insert)
    (define-key map "\C-c\C-q" 'fmodify-quit)
    (define-key map "\C-c\C-o" 'fmodify-overwrite-mode)
    (define-key map "o"        'fmodify-overwrite-mode)
    ;; digit argument
    (define-key map "0" 'digit-argument)
    (define-key map "1" 'digit-argument)
    (define-key map "2" 'digit-argument)
    (define-key map "3" 'digit-argument)
    (define-key map "4" 'digit-argument)
    (define-key map "5" 'digit-argument)
    (define-key map "6" 'digit-argument)
    (define-key map "7" 'digit-argument)
    (define-key map "8" 'digit-argument)
    (define-key map "9" 'digit-argument)
    (define-key map "-" 'negative-argument)
    ;; Move
    (define-key map "f" 'forward-word)
    (define-key map "b" 'backward-word)
    (define-key map "F" 'forward-sexp)
    (define-key map "B" 'backward-sexp)
    (define-key map "a" 'beginning-of-line)
    (define-key map "e" 'end-of-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "A" 'backward-sentence)
    (define-key map "E" 'forward-sentence)
    (define-key map "N" 'forward-paragraph)
    (define-key map "P" 'backward-paragraph)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)

    (define-key map "r" 'fmodify-replace-prompt)
    (define-key map "\C-m" 'fmodify-query-replace)
    (define-key map "\e\C-m" 'fmodify-query-replace-regexp)
    (define-key map "w" 'fmodify-replace-word)
    (define-key map ";" 'query-replace)
    (define-key map ":" 'query-replace-regexp)
    (define-key map "+" 'replace-string)
    (define-key map "*" 'replace-regexp)
    (define-key map "," 'fmodify-replace-region)
    (define-key map "m" 'fmodify-math-replace)

    (define-key map "c" 'capitalize-word)
    (define-key map "u" 'upcase-word)
    (define-key map "l" 'downcase-word)

    (define-key map "." 'fcopy-set-mark)
    (define-key map "@" 'fcopy-set-mark)
    (define-key map "^" 'fcopy-pop-mark-ring)
    (define-key map "x" 'exchange-point-and-mark)

    (define-key map "i" 'fmodify-insert-from-minibuffer)

    (define-key map "d" 'delete-char)
    (define-key map "h" 'backward-delete-char-untabify)
    (define-key map "D" 'fmodify-delete-word)
    (define-key map "k" 'fmodify-kill-line)
    (define-key map "z" 'zap-to-char)

    (define-key map "?" 'describe-mode)
    (setq fmodify-edit-mode-map map)))


(defun fmodify-edit-mode ()
  "Major mode for editing copied text without inserting.

The other major mode is overwrite mode.  To enter overwrite mode,
type \\[fmodify-overwrite-mode].

Edit mode is easy to call replace command.  For example,
\\[query-replace] is for `query-replace' and \\[query-replace-regexp]
is for `query-replace-regexp'.

When editing was finished, type \\[fmodify-insert] to insert modified
text to position where fcopy starts.

\\{fmodify-edit-mode-map}

Turning on Funny Modify overwrite mode runs the normal hook
`fmodify-overwrite-mode-hook'."
  (interactive)
  (overwrite-mode -1)
  (setq major-mode 'fmodify-edit-mode
	mode-name  "Funny Modify edit")
  (use-local-map fmodify-edit-mode-map)
  (message "Type C-c C-c to insert.  C-c C-q to quit.  C-c C-o to Overwrite mode."))


(defun fmodify-replace-prompt (replace)
  "Provide many replacement commands with one stroke.
key  Command
---  ------
m    fmodify-math-replace
r    fmodify-replace-region
w    fmodify-replace-word
s    replace-string
q    query-replace

Capital letter -- S and Q -- do replacement with REGEXP."
  (interactive "cReplace:  M)ath  W)ord  R)egion  S)tring  Q)uery  -- Capital for REGEXP")
  (cond
   ((eq replace ?m) (fmodify-math-replace))
   ((eq replace ?n) (fmodify-math-replace))
   ((eq replace ?-) (fmodify-math-replace))
   ((eq replace ?r) (call-interactively 'fmodify-replace-region))
   ((eq replace ? ) (call-interactively 'fmodify-replace-region))
   ((eq replace ?w) (fmodify-replace-word))
   ((eq replace ?s) (call-interactively 'replace-string))
   ((eq replace ?S) (call-interactively 'replace-regexp))
   ((>= replace ?a) (call-interactively 'query-replace))
   ((<= replace ?Z) (call-interactively 'query-replace-regexp))
   ))


(defun fmodify-kill-line (&optional arg)
  "Kill the rest of the current line.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

fmodify-kill-line kills the whole line including its terminating
newline, when used at the beginning of a line with no argument.  As a
consequence, you can always kill a whole line by typing `a C-k'"
  (interactive "p")
  (cond
   ((not arg) (setq arg 1))
   ((< arg 0) (setq arg (1+ arg)))
   ((= arg 0) (setq arg 1)))
  (delete-region (point) (progn (end-of-line arg) (point))))

(defun fmodify-insert-from-minibuffer (string)
  "Insert STRING from minibuffer in fmodify-edit-mode.
If you want to insert in overwrite mode, type `M-x overwrite-mode'.
Overwrite mode is turn off so that is to say, this is text mode."
  (interactive "sinsert: ")
  (insert string))

(defun fmodify-delete-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun fmodify-query-replace (arg)
  "If mark is active call `fmodify-replace-region', otherwise `query-replace'.
With prefix arg, use `replace-string' instead of `query-replace'.
Use \\[query-replace] if you hope the mark restrict the region of replacement."
  (interactive "P")
  (if (fcopy-mark-active-p)
      (call-interactively 'fmodify-replace-region)
    (if arg
	(call-interactively 'replace-string)
      (call-interactively 'query-replace))))

(defun fmodify-query-replace-regexp (arg)
  "Do `query-replace-regexp', or `replace-regexp' if called with prefix arg."
  (interactive "P")
  (if arg
      (call-interactively 'replace-regexp)
    (call-interactively 'query-replace-regexp)))

(defun fmodify-replace-region (beg end &optional all)
  "Replace string in region to TO-STRING.
With prefix argument, replace all strings with query."
  (interactive "r\nP")
  (let* ((from-string (buffer-substring beg end))
	 (to-string   (read-string
		       (format "Replace \"%s\" with: " from-string))))
    (if all
	(progn
	  (goto-char (point-min))
	  (query-replace from-string to-string))
      (delete-region beg end)
      (insert to-string))))

(defun fmodify-replace-word (&optional arg)
  "Replace ARG's word with TO-STRING.
Used from `fmodify-replace-prompt', Prefix argument can't work."
  (interactive "P")
  (let* ((arg (if arg arg 1))
	 (beg (point))
	 (end (progn (forward-word arg) (point)))
	 (from-string (buffer-substring beg end))
	 (to-string (read-string
		     (format "Replace \"%s\" with: " from-string))))
    (delete-region beg end)
    (insert to-string)))

(defun fmodify-math-replace ()
  "Search to numeric number and replace it"
  (interactive)
  (catch 'math-quit
    (while (re-search-forward "[+-＋−]?[0-9０-９,，]+[.．]?[0-9０-９]*" nil t)
      (let*
	  ((beg    (match-beginning 0))
	   (end    (match-end 0))
	   (string (buffer-substring beg end))
	   (char   (progn
		     (message "S)kip  Q)uit  Replace:  %s -> " string)
		     (read-char))))
	(cond
	 ((or (equal char ?-) (equal char ?+) (and (>= char ?0) (<= char ?9)))
	  (let
	      ((to-string
		(read-string
		 (format "Replace: %s -> " string)
		 (char-to-string char))))
	    (delete-region beg end)
	    (insert to-string)))
	 ;; N)ot replace
	 ((equal char ?n)  nil)
	 ;; S)kip replace
	 ((equal char ?s)  nil)
	 ;; Q)uit math replace
	 ((equal char ?q)
	  (throw 'math-quit t))
	 (t (error "Failed math replacement."))) ;exit cond block
	))))


;;; Overwrite mode

(defvar fmodify-overwrite-mode-map nil
  "Used in fmodify-overwrite mode")
(if fmodify-overwrite-mode-map
    nil
  (let ((map (copy-keymap text-mode-map)))
    (define-key map "\C-c\C-i" 'fmodify-insert)
    (define-key map "\C-c\C-c" 'fmodify-insert)
    (define-key map "\C-c\C-q" 'fmodify-quit)
    (define-key map "\C-c\C-e" 'fmodify-edit-mode)
    (define-key map " " 'fmodify-insert-space)
    (setq fmodify-overwrite-mode-map map)))


(defun fmodify-overwrite-mode ()
  "Major mode for modify copied text in overwrite minor mode.

The other major mode is edit mode.  To enter edit mode, type
\\[fmodify-edit-mode].

In overwrite mode, every command but [space] overwrite char.  [space]
do not overwrite but insert itself.

When modifying was finished, type \\[fmodify-insert] to insert modified
text to position where fcopy starts.

\\{fmodify-overwrite-mode-map}

Turning on Funny Modify overwrite mode runs the normal hook
`fmodify-overwrite-mode-hook'."
  (interactive)
  (overwrite-mode 1)
  (setq major-mode 'text-mode
	mode-name  "Funny Modify overwrite")
  (use-local-map fmodify-overwrite-mode-map)
  (run-hooks 'fmodify-overwrite-mode-hook)
  (message "Type C-c C-c to insert.  C-c C-q to quit.  C-c C-e to Edit mode."))


(defun fmodify-insert-space (&optional arg)
  "insert ARG's space to overwrite mode buffer."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((overwrite-mode nil))
    (insert-char ? arg)))

