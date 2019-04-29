;;; -*- lexical-binding: t -*-

;; (helm-linum-relative-mode 1)

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1))))

(provide 'amb-terminal)
