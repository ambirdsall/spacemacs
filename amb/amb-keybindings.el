;;; -*- lexical-binding: t -*-

(require 'amb-helper-functions)
(require 'core-keybindings)

(if (display-graphic-p) ;; org-mode heading keybindings
    (with-eval-after-load 'org
      (define-key org-mode-map (kbd "C-RET") #'amb/org-new-heading)
      (define-key org-mode-map (kbd "C-S-RET") #'amb/org-new-subheading)
      (define-key org-mode-map [remap evil-org-org-insert-todo-heading-respect-content-below] 'amb/org-new-subheading)
      ; (evil-define-key 'normal org-mode-map $
      ;   (kbd "C-RET") #'amb/org-new-heading $
      ;   (kbd "C-S-RET") #'amb/org-new-subheading) $
      )
  (with-eval-after-load 'org
    (let ((map (if (boundp 'input-decode-map)
                   input-decode-map
                 function-key-map)))
      (define-key map "\e[1;P9" (kbd "C-RET"))
      (define-key org-mode-map (kbd "C-RET") #'amb/org-new-heading)
      (define-key map "\e[1;P10" (kbd "C-S-RET"))
      (define-key org-mode-map (kbd "C-S-RET") #'amb/org-new-subheading)
      (define-key org-mode-map [remap evil-org-org-insert-todo-heading-respect-content-below] 'amb/org-new-subheading)
      ; (evil-define-key 'normal org-mode-map $
      ;   (kbd "C-RET") #'amb/org-new-heading $
      ;   (kbd "C-S-RET") #'amb/org-new-subheading) $
      )))

;; logical equivalent of vim's ~o~ and ~O~ in emacs
(progn
  (global-set-key (kbd "<C-return>") 'open-line-below)
  (global-set-key (kbd "<C-S-return>") 'open-line-above))

(progn ;; global application shortcuts to match OS keybindings
  (global-set-key (kbd "M-s-SPC") (lambda () (interactive) (shell-command "open '/Applications/Google Chrome.app'")))
  ;; s-SPC not always getting recognized tho
  (global-set-key (kbd "s-SPC") (lambda () (interactive) (shell-command "open '/Applications/iTerm.app"))))

(global-set-key [remap fill-paragraph]
                #'amb/fill-or-unfill)

(progn ;; limited global paredit
  (global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
  (global-set-key (kbd "C-(") 'paredit-backward-slurp-sexp)
  (global-set-key (kbd "C-}") 'paredit-forward-barf-sexp)
  (global-set-key (kbd "C-}") 'paredit-backward-barf-sexp))

(with-eval-after-load 'doc-view ;; fix clobbered keybinding in docview mode
  (define-key doc-view-mode-map (kbd "n") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "l") 'doc-view-next-page))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "g" #'amb/edit-org-mode-glossary-notes)

(spacemacs/declare-prefix "d" "dash")
(spacemacs/declare-prefix "o" "ᕙ(⇀‸↼‶)ᕗ")
(spacemacs/declare-prefix "oe" "edit note files")
(spacemacs/set-leader-keys
  "oea" #'amb/edit-aliases
  "oec" #'amb/edit-cli-primer
  "oee" #'amb/pick-a-note-why-dont-ya
  "oeE" #'amb/edit-elisp-notes
  "oed" #'amb/edit-helper-functions
  "oen" #'amb/edit-ngts-todos
  "oev" #'amb/edit-variables
  "fev" #'amb/edit-variables
  "fee" #'amb/pick-an-elisp-file-why-dont-ya
  "oet" #'amb/edit-text-objects
  "oes" #'amb/edit-sigfig-notes
  "oek" #'amb/edit-keybindings
  "fek" #'amb/edit-keybindings)

(spacemacs/declare-prefix "ot" "typescript")
(spacemacs/set-leader-keys
  "otc" #'tide-jsdoc-template
  "ott" #'amb/type-check-current-buffer-file
  "otp" #'amb/prettify-region
  "otP" #'amb/prettify-buffer
  "oty" #'amb/copy-file-path-relative-to-project-root)

(spacemacs/declare-prefix "oh" "help")
(spacemacs/set-leader-keys
  "ohk" #'which-key-show-major-mode
  "ohK" #'which-key-show-keymap)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxt" #'amb/tabify-buffer
  "oxT" #'amb/untabify-buffer)

(spacemacs/declare-prefix "zo" "origami")
(spacemacs/set-leader-keys
  "zoc" #'origami-close-all-nodes
  "zoo" #'origami-open-all-nodes)

(spacemacs/set-leader-keys
  ":"    #'helm-M-x
  "."    (lambda () (interactive) (dired "."))
  "/"    #'spacemacs/helm-project-do-rg
  "SPC"  #'amb/jump-around
  "aD"   #'dired-projectile-project-root
  "D"    #'docker
  "fa"   #'amb/find-alternate-file
  "fer"  #'source-dotspacemacs-user-config
  "G"    #'magit-status
  "l"    #'spacemacs/workspaces-transient-state/body
  "L"    #'spacemacs/layouts-transient-state/body
  "nk"   #'evil-numbers/inc-at-pt
  "nj"   #'evil-numbers/dec-at-pt
  "oa"   #'org-agenda
  "oA"   #'amb/open-agenda-file
  "oc"   #'amb/toggle-clipboard
  "od"   #'delete-trailing-whitespace
  "of"   #'evil-first-non-blank
  "oF"   #'font-lock-fontify-buffer
  ; "oh" #'spacemacs/evil-search-clear-highlight $
  "ol"   #'evil-buffer
  "oo"   #'evil-open-below-without-leaving-normal-state
  "oO"   #'evil-open-above-without-leaving-normal-state
  "op"   #'amb/paste-from-clipboard
  "oy"   #'amb/evil-yank-to-clipboard
  "oz"   #'evil-toggle-fold
  "po"   #'org-projectile/goto-todos
  "ps"   #'amb/helm-ag-in-projectile-root
  "hf"   #'describe-function
  "hh"   #'describe-key-briefly
  "hm"   #'woman
  "hk"   #'describe-key
  "hK"   #'which-key-show-top-level
  "hv"   #'describe-variable
  "J"    #'evil-avy-goto-char-timer
  "jj"   #'evil-avy-goto-char-2
  "jJ"   #'evil-avy-goto-char
  "si"   #'helm-imenu
  "V"    #'er/contract-region
  "ww"   #'ace-window
  "wW"   #'other-window
  "W"    #'subword-mode
  "xF"   #'unfill-paragraph
  "zO"   #'origami-mode
  "Z"    #'evil-toggle-fold)

(progn ;; jump between isearch results using arrow keys.
  ;; left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))

(with-eval-after-load 'dired
  (evilified-state-evilify dired-mode dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

(progn ;; insert current filename into minibuffer (e.g. for shell command)
  (define-key minibuffer-local-map
    [f3] (lambda () (interactive)
           (insert (buffer-file-name (current-buffer-not-mini)))))

  (defun current-buffer-not-mini ()
    "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
    (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
          (window-buffer (previous-window)) (window-buffer (next-window))))))

(progn ;; search navigation
 (define-key evil-normal-state-map (kbd "gf") #'helm-find-files)
 (define-key evil-normal-state-map (kbd "/") #'helm-swoop)
 (define-key evil-visual-state-map (kbd "/") #'helm-swoop))

(progn ;; Info-mode scrolling
  (define-key Info-mode-map (kbd "<up>") #'evil-scroll-line-up)
  (define-key Info-mode-map (kbd "<down>") #'evil-scroll-line-down)
  (define-key Info-mode-map (kbd "<left>") #'Info-backward-node)
  (define-key Info-mode-map (kbd "<right>") #'Info-forward-node))

(with-eval-after-load 'org
 (define-key org-mode-map (kbd "<up>") #'previous-line)
 (define-key org-mode-map (kbd "<down>") #'next-line)
 ;; (define-key org-mode-map (kbd "<") #'evil-shift-left)
 ;; (define-key org-mode-map (kbd ">") #'evil-shift-right)
 (define-key org-mode-map (kbd "<C-S-return>") #'amb/org-insert-subheading-respect-content)
 (spacemacs/set-leader-keys-for-major-mode 'org-mode
   "hs" #'amb/org-insert-subheading-respect-content
   "hi" #'org-insert-heading-after-current
   "p" #'amb/html2org-clipboard))

;; TODO: if on a mac and GUI, bind `(kbd "s-_")` (i.e. alt-shift-dash, the standard OS-level em-dash binding) to self-insert em-dash

(provide 'amb-keybindings)
