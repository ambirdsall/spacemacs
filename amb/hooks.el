;;; -*- lexical-binding: t -*-

(add-hook 'dired-mode-hook 'amb/dired-mode-setup)
(add-hook 'org-babel-after-execute-hook 'amb/fix-inline-images)
(add-hook 'outline-minor-mode-hook 'outshine-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'comment-auto-fill)
(add-hook 'js2-mode-hook (lambda () (jasminejs-mode)))
(add-hook 'jasminejs-mode-hook (lambda () (jasminejs-add-snippets-to-yas-snippet-dirs)))
(add-hook 'typescript-mode-hook 'setup-tide-mode)

;; paredit!
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
