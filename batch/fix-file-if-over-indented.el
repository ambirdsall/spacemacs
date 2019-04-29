(defun last-line-of-buffer? ()
  "Return non-nil if the cursor is at the last line of the
buffer."
  (save-excursion (end-of-line) (/= (forward-line) 0)))

(defun current-line-empty? ()
  "Does what it says on the tin, saving cursor position."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun current-line-starts-with-tab-or-empty? ()
  "Returns nil if the first character of current line is something other than a tab or newline.

Assumes that it will not be called when point is after the final newline of the buffer."
  (beginning-of-line)
  (or (current-line-empty?) (eq (char-after) 9)))

(defun current-buffer-over-indented? ()
  "Returns t if every non-empty line of the current buffer starts with a tab character"
  (save-excursion
    (goto-char (point-min))
    (let ((is-over-indented t))
      (while (not (last-line-of-buffer?))
        (or (current-line-starts-with-tab-or-empty?)
            (setq is-over-indented nil))
        (next-line))
      is-over-indented)))

(defun fix-file-if-over-indented ()
  "Checks if every non-empty line begins with a tab; if so, deletes that first tab char from each line."
  (goto-char (point-min))
  (if (current-buffer-over-indented?)
      (progn
        (while (not (last-line-of-buffer?))
          (beginning-of-line)
          (and (eq (char-after) 9) (delete-char 1))
          (next-line))
        (save-buffer))))
