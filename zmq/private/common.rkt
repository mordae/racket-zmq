#lang racket/base
;
; Common Utilities
;

(require racket/contract
         (only-in ffi/unsafe register-finalizer))

(provide (all-defined-out))


(define/contract (retain-parent parent object)
                 (-> any/c any/c any/c)
  (when parent
    (let ((boxee (box parent)))
      (register-finalizer object (lambda (object)
                                   (set-box! boxee #f)))))
  object)


; vim:set ts=2 sw=2 et:
