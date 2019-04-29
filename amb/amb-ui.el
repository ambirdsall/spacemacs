;;; -*- lexical-binding: t -*-

(require 'whitespace)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
(setq whitespace-style '(face tabs trailing tab-mark))

;; set to dark by default, same as the actual themes
(set-face-attribute 'whitespace-tab nil
                    :background "#272727"
                    :foreground "#383838"
                    :weight 'normal)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; and keep this ish in sync with the theme, at least for standard theme switching via 'spacemacs/cycle-spacemacs-theme
(defun amb/set-tab-color ()
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      (set-face-attribute 'whitespace-tab nil
                          :background "#272727"
                          :foreground "#383838"
                          :weight 'normal)
    (set-face-attribute 'whitespace-tab nil
                        :background "#d7b797"
                        :foreground "#fdffe3"
                        :weight 'normal)))

(advice-add 'spacemacs/cycle-spacemacs-theme :after #'amb/set-tab-color)

(progn ;; doom-modeline
  (require 'doom-modeline)
  (doom-modeline-init)
  (setq doom-modeline-height 23))

(provide 'amb-ui)
