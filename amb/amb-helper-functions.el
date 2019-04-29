;;; -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'f)
(require 'amb-predicate-aliases)

(defun amb/insert-jira-ticket-org-link (ticket-id)
  "inserts an org link to the given jira ticket at point, with `ticket-id' as the visible text.

`ticket-id' is a jira number like COP-123 or MRY-444 or whatever."
  (interactive (list (read-string "Jira ticket: ")))
  (insert (s-concat "[[//jira.sigfig.com/browse/" ticket-id "][" ticket-id "]]")))

(defun amb/org-new-subheading (is-todo)
  (interactive "P")
  (if is-todo
      (progn
        (amb/org-insert-subheading-respect-content)
        (org-todo))
    (amb/org-insert-subheading-respect-content)))

(defun amb/org-new-heading (is-todo)
  (interactive "P")
  (if is-todo
      (progn
        (org-insert-heading-after-current)
        (org-todo))
    (org-insert-heading-after-current)))

(defun source-dotspacemacs-user-config ()
  (interactive)
  (dotspacemacs/user-config))

(defun evil-open-above-without-leaving-normal-state (count)
  "Insert a new line above the current line and move the cursor to the new line without changing editing state."
  (interactive "p")
  (evil-open-above count)
  (normal-mode))

(defun evil-open-below-without-leaving-normal-state (count)
  "Insert a new line below the current line and move the cursor to the new line without changing editing state."
  (interactive "p")
  (evil-open-below count)
  (normal-mode))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun move-current-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun amb/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun copy-filepath (relative-to-repo)
  "Copy the filename of the current buffer to the system clipboard, even if it's disabled."
  (interactive "P")
  (let ((select-enable-clipboard t)
        (filepath (when (buffer-file-name)
                    (if relative-to-repo
                        (f-relative (buffer-file-name) (projectile-project-root))
                      (buffer-file-name)))))
    (if (display-graphic-p)
        (when filepath (gui-select-text filepath))
      (when filepath (kill-new filepath)))))

(defun copy-filename ()
  "Copy the filename of the current buffer to the system clipboard, even if it's disabled."
  (interactive)
  (let ((select-enable-clipboard t)
        (filename (f-filename (buffer-file-name))))
    (if (display-graphic-p)
        (gui-select-text filename)
      (when filename (kill-new filename)))))

(defun amb/type-check-current-buffer-file ()
  "Pass the current buffer file to `tsc` for a type-checking pass.
Does not emit any compiled js."
  (interactive)
  (shell-command (s-concat "tsc --noEmit " (buffer-file-name))))

(defun amb/org-insert-subheading-respect-content ()
  "Opens a new subheading without affecting the current line"
  (interactive)
  (call-interactively 'spacemacs/evil-insert-line-below)
  (call-interactively 'next-line)
  (call-interactively 'org-insert-subheading))

(defun amb/keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(defun amb/current-keymap ()
  "Print the name of the symbol to which the current keymap is bound, if any such symbol exists"
  (interactive)
  (if-let ((current-keymap (amb/keymap-symbol (current-local-map))))
    (message (symbol-name current-keymap))))

;; Avoid polluting the system clipboard
(defun amb/toggle-clipboard ()
  "Toggles whether the system clipboard is accessable to emacs.

If it's connected, you can paste from the system clipboard, but all deleted or killed text will end
up polluting the system clipboard, which can get annoying fast.

If not, the system clipboard doesn't get polluted, but there's no great way to quickly grab text
from outside applications."
  (interactive)
  (if select-enable-clipboard
      (progn
        (setq select-enable-clipboard nil)
        (message "The system clipboard is safe and sound again!"))
    (progn
      (setq select-enable-clipboard t)
      (message "Copy and paste away, slugger!"))))

(defvar clipboard-was-enabled?)
(defun amb/paste-from-clipboard ()
  "Inserts the contents of the system clipboard at point."
  (interactive)
  (let ((clipboard-was-enabled? select-enable-clipboard))
    (setq select-enable-clipboard t)
      (call-interactively 'evil-paste-after))
    (setq select-enable-clipboard clipboard-was-enabled?))

(defun amb/evil-yank-to-clipboard ()
  "Ensures the system clipboard is enabled and then calls evil-yank.

You should probably just use this on a region? Not totally sure
how evil-motions work yet, tbh."
  (interactive)
  (let ((clipboard-was-enabled? select-enable-clipboard))
    (setq select-enable-clipboard t)
    (call-interactively 'evil-yank)
    (setq select-enable-clipboard clipboard-was-enabled?)))

(defun amb/dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))

(defun amb/prettify-buffer ()
    "Invoke the shell command prettier on region, replacing
contents with reformatted version."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "/Users/abirdsall/workspace/ngts/ngts_dev_tools/bin/ngts-reformat"
                           t ; direct command output to current buffer
                           t ; replace buffer contents
                           ))

(defun amb/prettify-region ()
  "Invoke the shell command prettier on region, replacing
contents with reformatted version.

This only works on self-contained semantic units, unfortunately: that is, you
can reformat a single class from a file, but not a single private method from a
class. \"Not working\" here means \"is replaced with an error message, not
a reformatted version of itself\"."
  (interactive)
  (let ((range-start (int-to-string (min (point) (mark))))
        (range-end (int-to-string (max (point) (mark)))))
        (shell-command
         (s-concat "prettier --range-start=" range-start " --range-end=" range-end " --parser typescript --use-tabs --trailing-comma 'all' " (buffer-file-name))
         (current-buffer))))

(defun amb/copy-file-path-relative-to-project-root ()
  "Put the current buffer's filepath relative to the project root in the kill ring. Good for imports"
  (interactive)
  ;; TODO if text is selected, add it into the saved string as the imported object
  ;; TODO select buffer with helm and insert in current buffer, respecting aliases and resolving
  ;; relative to the current buffer if they're in the same subproject
  (let* ((relative-path (f-relative (buffer-file-name) (projectile-project-root)))
         (trimmed-path (replace-regexp-in-string "\.ts$" "" relative-path))
         )
   (kill-new trimmed-path)))

(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(defun amb/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun amb/jump-around (restrict-to-open-buffers)
  "Grab the helm and go to a project file quickly.

If called with a prefix arg, restricts to open buffers; by default, any file."
  (interactive "P")
  (if restrict-to-open-buffers
      (call-interactively #'helm-buffers-list)
    (if (projectile-project-p)
        (call-interactively #'helm-projectile-find-file)
      (call-interactively #'helm-find-files))))

(defun amb/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org --wrap=none"))
  (yank))

(defun dired-projectile-project-root ()
  "If in a projectile project, open a dired buffer in the project root directory."
  (interactive)
  (and (projectile-project-p) (dired (projectile-project-root))))

(defun amb/tabify-buffer ()
  "tabify current buffer, the whole current buffer, and nothing but the current buffer."
  (interactive)
  (save-excursion (tabify (point-min) (point-max))))

(defun amb/untabify-buffer ()
  "untabify current buffer, the whole current buffer, and nothing but the current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(defun amb/find-alternate-file ()
  "Tries to find a conventionally-located test file based on the current file's filename and location."
  (interactive)
  (cond
   ((s-matches? ".scala$" (buffer-file-name))
    (if (s-matches? "Spec.scala" (buffer-file-name))
        (let ((implementation-file-path (s-replace-all '(("Spec.scala" . ".scala") ("test" . "main")) (buffer-file-name))))
          (if (f-exists? implementation-file-path) (find-file implementation-file-path)
            (message (s-concat "could not find that fucker " implementation-file-path))))
      (let ((spec-file-path (s-replace-all '((".scala" . "Spec.scala") ("main" . "test")) (buffer-file-name))))
        (if (f-exists? spec-file-path) (find-file spec-file-path)
          (message (s-concat "could not find that fucker " spec-file-path))))))
   ((t (message "don't know how to find the alternate file for this file type")))))

(defmacro find-file-as-command (filename)
    `(lambda (P-is-for-prefix-arg)
       (interactive "P")
       (if current-prefix-arg (split-window-right-and-focus))
       (find-file ,filename)))

(cl-flet ((amb/ (lambda (filename) (s-concat user-emacs-directory "amb/" filename))))
  (fset 'amb/edit-helper-functions (find-file-as-command (amb/ "amb-helper-functions.el")))
  (fset 'amb/edit-keybindings (find-file-as-command (amb/ "amb-keybindings.el")))
  (fset 'amb/edit-text-objects (find-file-as-command (amb/ "text-objects.el")))
  (fset 'amb/edit-variables (find-file-as-command (amb/ "variables.el")))
  (fset 'amb/edit-elisp-notes (find-file-as-command (amb/ "elisp.org")))
  (fset 'amb/edit-aliases (find-file-as-command (amb/ "amb-predicate-aliases.el"))))
(fset 'amb/edit-cli-primer (find-file-as-command "~/notes/cli-primer.org"))
(fset 'amb/edit-ngts-todos (find-file-as-command "~/workspace/ngts/TODOs.org"))
(fset 'amb/open-agenda-file (find-file-as-command "~/notes/agenda.org"))
(fset 'amb/edit-sigfig-notes (find-file-as-command "~/notes/sigfig.org"))
(fset 'amb/edit-org-mode-glossary-notes (find-file-as-command "~/notes/org-mode-glossary.org"))

(defmacro helm-edit-file-from-directory (helm-title dir)
  `(lambda (_prefix)
     (interactive "P")
     (helm :sources `((name . ,,helm-title)
                      (candidates . ,(-map (lambda (f)
                                             `(,(f-relative f ,dir) . ,f))
                                           (f-files ,dir
                                                    (lambda (g) (not (s-matches? "\.DS_Store" g))))))
                      (action . (lambda (c)
                                  (if current-prefix-arg (split-window-right-and-focus))
                                  (find-file c)))))))

(fset 'amb/pick-a-note-why-dont-ya (helm-edit-file-from-directory "NOTES" "/Users/abirdsall/notes"))
(fset 'amb/pick-an-elisp-file-why-dont-ya (helm-edit-file-from-directory "elisp files" "/Users/abirdsall/.emacs.d/amb"))

(defmacro on-string-or-region (fn)
  `(lambda (string &optional from to)
     (interactive
      (if (use-region?)
          (list nil (region-beginning) (region-end))
        (let ((bds (bounds-of-thing-at-point 'paragraph)))
          (list nil (car bds) (cdr bds)))))

     (let* ((work-on-string? (if string t nil))
            (input-str (if work-on-string?
                           string
                         (buffer-substring-no-properties from to)))
            (output-str (funcall ,fn input-str)))

       (if work-on-string?
           output-str
         (save-excursion
           (delete-region from to)
           (goto-char from)
           (insert output-str))))))

(fset 'kebab-case (on-string-or-region #'s-dashed-words))
(fset 'pascal-case (on-string-or-region #'s-upper-camel-case))
(fset 'camel-case (on-string-or-region #'s-lower-camel-case))
(fset 'snake-case (on-string-or-region #'s-snake-case))
(fset 'screaming-snake-case (on-string-or-region #'(lambda (str) (s-upcase (s-snake-case str)))))
(fset 'lower-words-case (on-string-or-region #'(lambda (str) (s-join " " (-map #'s-downcase (s-split-words str))))))

(provide 'amb-helper-functions)
