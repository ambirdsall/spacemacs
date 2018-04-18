(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; create "il"/"al" (inside/around) line text objects:
(define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
;; create "ie"/"ae" (inside/around) entire buffer text objects:
(define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")
