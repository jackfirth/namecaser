#lang racket/base


(provide
 lower-camel-case-in
 upper-camel-case-in
 lower-snake-case-in
 screaming-snake-case-in
 upper-snake-case-in
 lower-kebab-case-in
 screaming-snake-case-in
 upper-kebab-case-in)


(require (for-syntax namecaser/base
                     racket/base
                     racket/require-transform
                     syntax/parse))


;@----------------------------------------------------------------------------------------------------


(begin-for-syntax
  (define (import-rename imp new-symbol)
    (define id (import-local-id imp))
    (define new-id (datum->syntax id new-symbol id id))
    (struct-copy import imp [local-id new-id]))

  (define (make-case-name-require-transformer case-changer)
    (make-require-transformer
     (λ (stx)
       (syntax-parse stx
         [(_ require-spec ...)
          (define-values (imports sources) (expand-import #'(combine-in require-spec ...)))
          (values
           (for/list ([i (in-list imports)]
                      #:do [(define import-name (symbol->string (syntax-e (import-local-id i))))]
                      #:when (or (camel-case-ascii-string? import-name)
                                 (snake-case-ascii-string? import-name)
                                 (kebab-case-ascii-string? import-name)))
             (define new-name (string->symbol (case-changer (name-words import-name))))
             (import-rename i new-name))
           sources)])))))


(define-syntax lower-camel-case-in (make-case-name-require-transformer lower-camel-case))
(define-syntax upper-camel-case-in (make-case-name-require-transformer upper-camel-case))
(define-syntax lower-snake-case-in (make-case-name-require-transformer lower-snake-case))
(define-syntax upper-snake-case-in (make-case-name-require-transformer upper-snake-case))
(define-syntax screaming-snake-case-in (make-case-name-require-transformer screaming-snake-case))
(define-syntax lower-kebab-case-in (make-case-name-require-transformer lower-kebab-case))
(define-syntax upper-kebab-case-in (make-case-name-require-transformer upper-kebab-case))
(define-syntax screaming-kebab-case-in (make-case-name-require-transformer screaming-kebab-case))
