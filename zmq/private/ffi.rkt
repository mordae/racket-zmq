#lang racket/base
;
; ZeroMQ FFI Bindings
;

(require (rename-in ffi/unsafe (-> -->))
         (for-syntax racket/base)
         (for-syntax racket/syntax)
         racket/contract
         racket/provide
         throw)

(require "common.rkt")

(provide (filtered-out (lambda (name)
                         (and (regexp-match? #rx"^[^_]" name)
                              (not (regexp-match? #rx"^lib" name))
                              (not (regexp-match? #rx"-tag$" name))
                              (regexp-replace #rx"-pointer\\?$" name "?")))
           (except-out (all-defined-out)
                       zmq-strerror
                       zmq-ctx-destroy
                       zmq-msg-close)))


(define libzmq (ffi-lib "libzmq" '("3" "")))

(define-struct/contract (exn:fail:zmq exn:fail) ())
(define-struct/contract (exn:fail:zmq:again exn:fail:zmq) ())


(define-syntax (define-zmq stx)
  (syntax-case stx ()
    ((_ name type)
     (with-syntax ((symbol (regexp-replace* #rx"[?!]"
                             (regexp-replace* #rx"[-/]"
                               (symbol->string (syntax-e #'name)) "_") "")))
       #'(define name (get-ffi-obj symbol libzmq type))))))


(define (with-finalizer result finalizer)
  (when result
    (register-finalizer result finalizer))
  result)


(define (check-result result)
  (when (or (and (integer? result)
                 (not (>= result 0)))
            (not result))
    (cond
      ((= (saved-errno) 11)
       (throw exn:fail:zmq:again 'zmq (zmq-strerror (saved-errno))))

      (else
       (throw exn:fail:zmq 'zmq (zmq-strerror (saved-errno))))))
  result)


(define socket->ports
  (get-ffi-obj 'scheme_socket_to_ports #f
               (_fun _long
                     _string/utf-8
                     (_int = 0)
                     (inp : (_ptr o _scheme))
                     (outp : (_ptr o _scheme))
                     --> _void
                     --> (begin
                           (register-finalizer inp close-input-port)
                           (register-finalizer outp close-output-port)
                           (values inp outp)))))


(define-cpointer-type _zmq-ctx-pointer)
(define-cpointer-type _zmq-socket-pointer)

(define-cstruct _zmq-msg
  ((dummy (_array _byte 32))))


(define _zmq-ctx-option-name
  (_enum '(io-threads = 1
           max-sockets = 2)))


(define _zmq-bstr-option-name
  (_enum '(identity = 5
           subscribe = 6
           unsubscribe = 7)))

(define _zmq-int-option-name
  (_enum '(fd = 14
           events = 15
           type = 16
           linger = 17)))


(define _zmq-socket-type
  (_enum '(pair pub sub req rep dealer router pull push xpub xsub)))


;; Error Handling
(define-zmq zmq-strerror
            (_fun #:save-errno 'posix
                  _int --> _string/locale))


;; Context
(define-zmq zmq-ctx-new
            (_fun #:save-errno 'posix
                  --> (result : _zmq-ctx-pointer/null)
                  --> (with-finalizer (check-result result) zmq-ctx-destroy)))

(define-zmq zmq-ctx-destroy
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer
                  --> (result : _int)
                  --> (void (check-result result))))

(define-zmq zmq-ctx-set
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer
                  _zmq-ctx-option-name
                  _int
                  --> (result : _int)
                  --> (void (check-result result))))

(define-zmq zmq-ctx-get
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer
                  _zmq-ctx-option-name
                  --> (result : _int)
                  --> (check-result result)))


;; Messages
(define-zmq zmq-msg-init
            (_fun #:save-errno 'posix
                  (msg : (_ptr o _zmq-msg))
                  --> (result : _int)
                  --> (begin
                        (check-result result)
                        (with-finalizer msg zmq-msg-close))))

(define-zmq zmq-msg-close
            (_fun #:save-errno 'posix
                  (_ptr i _zmq-msg)
                  --> (result : _int)
                  --> (void (check-result result))))

(define-zmq zmq-msg-size
            (_fun (_ptr i _zmq-msg) --> _size))

(define-zmq zmq-msg-data
            (_fun (_ptr i _zmq-msg) --> _pointer))

(define-zmq zmq-msg-more?
            (_fun (_ptr i _zmq-msg) --> _bool))

(define-zmq zmq-msg-recv
            (_fun #:save-errno 'posix
                  (msg : (_ptr io _zmq-msg) = (zmq-msg-init))
                  _zmq-socket-pointer
                  (_bitmask '(dontwait = 1))
                  --> (result : _int)
                  --> (begin
                        (check-result result)
                        msg)))

(define (zmq-msg->bytes msg)
  (bytes-copy (make-sized-byte-string (zmq-msg-data msg) (zmq-msg-size msg))))


;; Socket
(define-zmq zmq-socket
            (_fun #:save-errno 'posix
                  (ctx : _zmq-ctx-pointer)
                  _zmq-socket-type
                  --> (result : _zmq-socket-pointer/null)
                  --> (retain-parent ctx
                        (with-finalizer (check-result result) zmq-close))))

(define-zmq zmq-close
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  --> (result : _int)
                  --> (void (check-result result))))


(define zmq-getsockopt/int
  (get-ffi-obj 'zmq_getsockopt libzmq
               (_fun #:save-errno 'posix
                     _zmq-socket-pointer
                     (name : _zmq-int-option-name)
                     (value : (_ptr o _int))
                     (size : (_ptr io _size) = (ctype-sizeof _int))
                     --> (result : _int)
                     --> (begin
                           (check-result result)
                           value))))

(define zmq-setsockopt/int
  (get-ffi-obj 'zmq_setsockopt libzmq
               (_fun #:save-errno 'posix
                     _zmq-socket-pointer
                     (name : _zmq-int-option-name)
                     (value : (_ptr i _int))
                     (size : _size = (ctype-sizeof _int))
                     --> (result : _int)
                     --> (void (check-result result)))))


(define zmq-getsockopt/bstr
  (get-ffi-obj 'zmq_getsockopt libzmq
               (_fun #:save-errno 'posix
                     _zmq-socket-pointer
                     (name : _zmq-bstr-option-name)
                     (value : _pointer = (make-bytes 256))
                     (size : (_ptr io _size) = 256)
                     --> (result : _int)
                     --> (begin
                           (check-result result)
                           (bytes-copy (make-sized-byte-string value size))))))

(define zmq-setsockopt/bstr
  (get-ffi-obj 'zmq_setsockopt libzmq
               (_fun #:save-errno 'posix
                     _zmq-socket-pointer
                     (name : _zmq-bstr-option-name)
                     (value : _pointer)
                     (size : _size = (bytes-length value))
                     --> (result : _int)
                     --> (void (check-result result)))))


(define-zmq zmq-bind
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  _string/utf-8
                  --> (result : _int)
                  --> (void (check-result result))))

(define-zmq zmq-connect
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  _string/utf-8
                  --> (result : _int)
                  --> (void (check-result result))))

(define-zmq zmq-send
            (_fun _zmq-socket-pointer
                  (buf : _bytes)
                  (_size = (bytes-length buf))
                  (_bitmask '(dontwait = 1
                              sndmore  = 2))
                  --> (result : _int)
                  --> (void (check-result result))))


; vim:set ts=2 sw=2 et:
