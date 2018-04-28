;;; -*- lexical-binding: t -*-

(add-hook 'dired-mode-hook 'amb/dired-mode-setup)
(add-hook 'org-babel-after-execute-hook 'amb/fix-inline-images)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; paredit!
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
