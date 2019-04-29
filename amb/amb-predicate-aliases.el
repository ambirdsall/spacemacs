;;; -*- lexical-binding: t -*-

;; I just really vastly prefer "use question mark as suffix" as an idiom for
;; type predicates to "use 'p' as a suffix except when it looks really weird or
;; would be misleading because there exists some other word"
(defalias 'atom? 'atom)
(defalias 'buffer? 'bufferp)
(defalias 'cons? 'consp)
(defalias 'display-graphic? 'display-graphic-p)
(defalias 'frame? 'framep)
(defalias 'list? 'listp)
(defalias 'null? 'null)
(defalias 'number? 'numberp)
(defalias 'process? 'processp)
(defalias 'string? 'stringp)
(defalias 'subr? 'subrp)
(defalias 'symbol? 'symbolp)
(defalias 'vector? 'vectorp)
(defalias 'window? 'windowp)

;; Hell, it's just better for predicates generally
(defalias 'bound? 'boundp)
(defalias 'fbound? 'fboundp)
(defalias 'use-region? 'use-region-p)

(provide 'amb-predicate-aliases)
