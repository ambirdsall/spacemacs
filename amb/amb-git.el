;;; -*- lexical-binding: t -*-

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; Make magit-gh-pulls look at the correct URL for GHE
;; This is an incomplete solution; it appears the library hardcodes the github URL
(setq gh-profile-alist '(("github"
                          :url "https://git.sigfig.com/api/v3"
                          :remote-regexp "^\\(?:git@git\\.sigfig\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?git\\.sigfig\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")))

;; use emacs for command line git stuff (not working with tmux rn :/)
(global-git-commit-mode)

(provide 'amb-git)
