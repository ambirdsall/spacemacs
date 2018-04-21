;;; -*- lexical-binding: t -*-

;; turn that sucker on in the terminal. There's good reasons not to, and its terminal UI ain't
;; nothing to call home about, but I like having the visual cues
(unless (or menu-bar-mode (display-graphic-p)) (menu-bar-mode))

(helm-linum-relative-mode 1)

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1))))
