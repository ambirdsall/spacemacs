;; turn that sucker on in the terminal. There's good reasons not to, and its terminal UI ain't
;; nothing to call home about, but I like having the visual cues
(unless (or menu-bar-mode (display-graphic-p)) (menu-bar-mode))

(helm-linum-relative-mode 1)
