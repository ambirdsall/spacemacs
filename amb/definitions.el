;;; -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'f)

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

(defun amb/paste-from-clipboard ()
  "Inserts the contents of the system clipboard at point."
  (interactive)
  (let ((clipboard-was-enabled? select-enable-clipboard))
    (setq select-enable-clipboard t)
    (call-interactively 'evil-paste-after)
    (setq select-enable-clipboard clipboard-was-enabled?)))

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

(defun amb/helm-ag-in-projectile-root ()
  "Calls helm-do-ag, but prefills the search directory with the
projectile project root so you can get directly to greppin'."
  (interactive)
  (call-interactively (helm-do-ag (projectile-project-root))))

(defun amb/jump-around (dont-restrict-to-buffers)
  "Grab the helm and go to a project file quickly.

If called with a prefix arg, restricts to open buffers; by default, any file."
  (interactive "P")
  (if dont-restrict-to-buffers
      (if (projectile-project-p)
          (call-interactively #'helm-projectile-find-file)
        (call-interactively #'helm-find-files))
    (call-interactively #'helm-buffers-list)))

(defmacro find-file-as-command (filename)
    `(lambda (P-is-for-prefix-arg)
       (interactive "P")
       (if current-prefix-arg (split-window-right-and-focus))
       (find-file ,filename)))

(cl-flet ((amb/ (lambda (filename) (s-concat user-emacs-directory "amb/" filename))))
  (fset 'amb/edit-definitions (find-file-as-command (amb/ "definitions.el")))
  (fset 'amb/edit-keybindings (find-file-as-command (amb/ "keybindings.el")))
  (fset 'amb/edit-text-objects (find-file-as-command (amb/ "text-objects.el")))
  (fset 'amb/edit-variables (find-file-as-command (amb/ "variables.el")))
  (fset 'amb/edit-elisp-notes (find-file-as-command (amb/ "elisp.org")))
  (fset 'amb/edit-aliases (find-file-as-command (amb/ "aliases.el"))))
(fset 'amb/edit-cli-primer (find-file-as-command "~/notes/cli-primer.org"))
(fset 'amb/edit-ngts-todos (find-file-as-command "~/workspace/ngts/TODOs.org"))
(fset 'amb/open-agenda-file (find-file-as-command "~/notes/agenda.org"))
(fset 'amb/edit-sigfig-notes (find-file-as-command "~/notes/sigfig.org"))

(defun amb/pick-a-note-why-dont-ya (_prefix)
  (interactive "P")
  (helm :sources `((name . "NOTES")
                   (candidates . ,(-map (lambda (f)
                                          `(,(f-relative f "/Users/abirdsall/notes") . ,f))
                                        (f-files "~/notes"
                                                 (lambda (g) (not (s-matches? "\.DS_Store" g))))))
                   (action . (lambda (c)
                               (if current-prefix-arg (split-window-right-and-focus))
                               (find-file c))))))

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
(fset 'screaming-snake-case (on-string-or-region #'(lambda (str) (s-upcase (s-snake-case str)))))
(fset 'lower-words-case (on-string-or-region #'(lambda (str) (s-join " " (-map #'s-downcase (s-split-words str))))))
