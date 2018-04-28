;;; -*- lexical-binding: t -*-

(unless (display-graphic-p)
    (with-eval-after-load 'org
    (let ((map (if (boundp 'input-decode-map)
                   input-decode-map
                 function-key-map)))
       (define-key map "\e[1;P9" (kbd "C-RET"))
       (define-key org-mode-map (kbd "C-RET") #'amb/org-new-heading)
       (define-key map "\e[1;P10" (kbd "C-S-RET"))
       (define-key org-mode-map (kbd "C-S-RET") #'amb/org-new-subheading))))

(if (display-graphic-p)
    (progn
     (global-set-key (kbd "M-<down>") #'move-current-line)
     (global-set-key (kbd "M-<up>") (lambda () (interactive) (move-current-line -1))))
  (progn
    (global-set-key (kbd "ESC <down>") #'move-current-line)
    (global-set-key (kbd "ESC <up>") (lambda () (interactive) (move-current-line -1)))))

;; (global-set-key (kbd "<C-return>") 'open-line-below)
;; (global-set-key (kbd "<C-S-return>") 'open-line-above)

(eval-after-load 'multiple-cursors-mode
  (lambda () (define-key mc/keymap (kbd "<return>") nil))) ; With multiple cursors, <return> inserts newline; C-g exits multiple-cursors-mode

(spacemacs/declare-prefix "oe" "edit note files")
(spacemacs/set-leader-keys
  "oea" #'amb/edit-aliases
  "oec" #'amb/edit-cli-primer
  "oee" #'amb/pick-a-note-why-dont-ya
  "oeE" #'amb/edit-elisp-notes
  "oed" #'amb/edit-definitions
  "oen" #'amb/edit-ngts-todos
  "oev" #'amb/edit-variables
  "fev" #'amb/edit-variables
  "oet" #'amb/edit-text-objects
  "oes" #'amb/edit-sigfig-notes
  "oek" #'amb/edit-keybindings
  "fek" #'amb/edit-keybindings)

(spacemacs/declare-prefix "ot" "typescript")
(spacemacs/set-leader-keys
  "otc" #'tide-jsdoc-template
  "ott" #'amb/type-check-current-buffer-file
  "otp" #'amb/prettify-region
  "otP" #'amb/prettify-buffer)

(spacemacs/set-leader-keys
  ":" #'helm-M-x
  "SPC" #'amb/jump-around
  "fer" #'source-dotspacemacs-user-config
  "nk" #'evil-numbers/inc-at-pt
  "nj" #'evil-numbers/dec-at-pt
  "oa" #'amb/open-agenda-file
  "oc" #'amb/toggle-clipboard
  "od" #'delete-trailing-whitespace
  "of" #'evil-first-non-blank
  "oF" #'font-lock-fontify-buffer
  "oh" #'spacemacs/evil-search-clear-highlight
  "ol" #'evil-buffer
  "oo" #'evil-open-below-without-leaving-normal-state
  "oO" #'evil-open-above-without-leaving-normal-state
  "op" #'amb/paste-from-clipboard
  "oy" #'amb/evil-yank-to-clipboard
  "oz" #'evil-toggle-fold
  "po" #'org-projectile/goto-todos
  "ps" #'amb/helm-ag-in-projectile-root
  "hh" #'describe-key-briefly
  "hm" #'woman
  "hs" #'helm-imenu
  "jj" #'evil-avy-goto-char-2
  "jJ" #'evil-avy-goto-char)

(with-eval-after-load 'dired
  (evilified-state-evilify dired-mode dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

(define-key evil-normal-state-map (kbd "gf") #'helm-find-files)

(setq evil-replace-with-register-key (kbd "gr"))
(evil-replace-with-register-install)

(define-key Info-mode-map (kbd "<up>") #'evil-scroll-line-up)
(define-key Info-mode-map (kbd "<down>") #'evil-scroll-line-down)
(define-key Info-mode-map (kbd "<left>") #'Info-backward-node)
(define-key Info-mode-map (kbd "<right>") #'Info-forward-node)

(with-eval-after-load 'org
 (define-key org-mode-map (kbd "<up>") #'previous-line)
 (define-key org-mode-map (kbd "<down>") #'next-line)
 ;; (define-key org-mode-map (kbd "<") #'evil-shift-left)
 ;; (define-key org-mode-map (kbd ">") #'evil-shift-right)
 (define-key org-mode-map (kbd "?") #'self-insert-command) ; what the fuck, discover.el
 (define-key org-mode-map (kbd "<C-S-return>") #'amb/org-insert-subheading-respect-content)
 (spacemacs/set-leader-keys-for-major-mode 'org-mode
   "hs" #'amb/org-insert-subheading-respect-content
   "hi" #'org-insert-heading-after-current))

;; MACRO DEFINITIONS
(fset 'import-definition-builder
   [?0 ?f ?\{ ?% ?d ?d ?\C-o ?0 ?C ?i ?d ?b ?\M-/ escape ?> ?>])

(fset 'build-directive-definition
   [?c ?t ?\( ?e ?x ?p ?o ?r ?t ?  ?c ?o ?n ?s ?t ?  ?n ?n ?n ?a ?m ?e ?  ?= ?  ?D ?e ?f ?i ?n ?i ?t ?i ?o ?n ?B ?u ?i ?l ?d ?e ?r ?. ?g ?e ?t ?D ?i ?r ?e ?c ?t ?i ?v ?e ?B ?u ?n ?d ?l ?e ?f ?d ?/ ?\" return ?l ?y ?i ?\" ?? ?n ?n ?n ?a ?m ?e return ?g ?r ?i ?w ?b ?~ ?E ?a ?D ?i ?r ?e ?c ?t ?i ?v ?e ?f ?d])

(fset 'convert-directive-template
   [?^ ?f ?U ?d ?e ?$ ?X ?F ?/ ?c ?T ?: ?  ?. escape ?y ?s ?t ?, ?\" ?y ?s ?t ?, ?\) ?i ?r ?e ?q ?u ?i ?r ?e escape ?$ ?F ?. ?a ?d ?i ?r ?e ?c ?t ?i ?v ?e ?. escape])

(fset 'build-component-definition
      [?c ?t ?\( ?e ?x ?p ?o ?r ?t ?  ?c ?o ?n ?s ?t ?  ?n ?n ?n ?a ?m ?e ?  ?= ?  ?D ?e ?f ?i ?n ?i ?t ?i ?o ?n ?B ?u ?i ?l ?d ?e ?r ?. ?g ?e ?t ?C ?o ?m ?p ?o ?n ?e ?n ?t ?B ?u ?n ?d ?l ?e ?f ?d ?/ ?\" return ?l ?y ?i ?\" ?? ?n ?n ?n ?a ?m ?e return ?g ?r ?i ?w ?b ?~ ?E ?a ?C ?o ?m ?p ?o ?n ?e ?n ?t ?f ?d])

(fset 'convert-component-template
      [?^ ?f ?U ?d ?e ?$ ?X ?F ?/ ?c ?T ?: ?  ?. escape ?y ?s ?t ?, ?\" ?y ?s ?t ?, ?\) ?i ?r ?e ?q ?u ?i ?r ?e escape ?$ ?F ?. ?a ?c ?o ?m ?p ?o ?n ?e ?n ?t ?. escape])

(fset 'build-service-definition
      [?c ?t ?\( ?e ?x ?p ?o ?r ?t ?  ?c ?o ?n ?s ?t ?  ?n ?n ?n ?a ?m ?e ?  ?= ?  ?D ?e ?f ?i ?n ?i ?t ?i ?o ?n ?B ?u ?i ?l ?d ?e ?r ?. ?g ?e ?t ?S ?e ?r ?v ?i ?c ?e ?B ?u ?n ?d ?l ?e ?f ?d ?/ ?\" return ?l ?y ?i ?\" ?? ?n ?n ?n ?a ?m ?e return ?g ?r ?i ?w ?b ?~ ?: ?s ?/ ?S ?e ?r ?v ?i ?c ?e ?/ ?S ?v ?c return])

(fset 'build-filter-definition
      [?c ?t ?\( ?e ?x ?p ?o ?r ?t ?  ?c ?o ?n ?s ?t ?  ?n ?n ?n ?a ?m ?e ?  ?= ?  ?D ?e ?f ?i ?n ?i ?t ?i ?o ?n ?B ?u ?i ?l ?d ?e ?r ?. ?g ?e ?t ?F ?i ?l ?t ?e ?r ?B ?u ?n ?d ?l ?e ?f ?d ?/ ?\" return ?l ?y ?i ?\" ?? ?n ?n ?n ?a ?m ?e return ?g ?r ?i ?w ?b ?~ ?E ?a ?F ?i ?l ?t ?e ?r ?f ?d])
