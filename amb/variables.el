;;; -*- lexical-binding: t -*-

(setq auto-save-default nil)
(setq bart-manage-window t)
(setq bart-station '24th)
(setq fill-column 100)
(setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s --ignore-file '*/dist-*' ")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
(setq helm-info-default-sources '(helm-source-info-emacs helm-source-info-elisp helm-source-info-org helm-source-info-magit))
(setq initial-scratch-message ";; wrap-round:              M-(
;; close-round-and-newline: M-)
;; forward-slurp-sexp:      C-)
;; forwards-barf-sexp:      C-}
;; kill-sexp:               C-M-k")
(setq insert-directory-program (executable-find "gls"))
(setq mc/always-run-for-all t)
(setq neo-theme 'nerd)
(setq x-select-enable-clipboard nil)
(if (display-graphic-p)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setq ns-function-modifier 'hyper))
  (progn
    (setq powerline-default-separator 'utf-8)))
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default major-mode 'org-mode)
(setq-default standard-indent 2)
(setq-default truncate-lines t)
(setq-default typescript-indent-level 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
