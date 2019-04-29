  ; Add projectile TODO.org files to agenda automatically
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (append (org-projectile-todo-files) org-agenda-files)
    (push "~/notes/agenda.org" org-agenda-files)
    (push "~/notes/ical-entries.org" org-agenda-files))

  (with-eval-after-load 'org
    (require 'ox-beamer)
    (require 'ox-confluence)
    (require 'ox-tufte)
    (require 'ob-dot)
    (require 'ob-js)
    (require 'ob-ruby)
    (require 'ob-shell)
    (require 'ob-typescript)
    (setq org-export-babel-evaluate nil)
    ;; Set sensible mode for editing dot files
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
    ;; Update images from babel code blocks automatically
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (js . t)
       (ruby . t)
       (shell . t)
       (typescript . t))))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-tufte
  :ensure t
  :after ox)

(provide 'amb-org)
