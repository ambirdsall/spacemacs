(setq-default major-mode 'org-mode)
(setq auto-save-default nil)
(setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq fill-column 100)
(setq x-select-enable-clipboard nil)
(setq helm-info-default-sources '(helm-source-info-emacs helm-source-info-elisp helm-source-info-org helm-source-info-magit))
(setq company-tooltip-align-annotations t)
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s --ignore-file '*/dist-*' ")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

(setq-default indent-tabs-mode t)

(provide 'amb-better-defaults)
