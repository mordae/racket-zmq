#lang racket/base
;
; ZeroMQ FFI Bindings
;

(require racket/contract
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/list)

(require misc1/syntax
         misc1/throw)

(require
  (rename-in ffi/unsafe (-> -->)))

(provide
  (all-defined-out))


;; Private exceptions.
(struct exn:fail:zmq exn:fail ())
(struct exn:fail:zmq:again exn:fail:zmq ())


;; Symbol importer.
(define-ffi-definer define-zmq
                    (ffi-lib "libzmq" '("3" "")))


;; Utility that makes sure parent argument is only collected after the
;; define resulting object does.
(define (retain-parent get-arg)
  (λ (fn)
    (λ args
      (let ((parent (get-arg args))
            (result (apply fn args)))
        (when parent
          (let ((box-with-parent (box parent)))
            (register-finalizer result (λ (result)
                                         (set-box! box-with-parent #f)))))
        (values result)))))


;; Checks result and raise an exception if it corresponds to a failure.
(define (check-result result)
  (when (or (and (integer? result)
                 (< result 0))
            (not result))
    (if (= (saved-errno) 11)
        (throw exn:fail:zmq:again 'zmq (zmq-strerror (saved-errno)))
        (throw exn:fail:zmq       'zmq (zmq-strerror (saved-errno))))))


;; Importer wrapper than checks result of the function and converts
;; failures into exceptions.
(define (checked-result fn)
  (λ args
    (let ((result (apply fn args)))
      (checked-result result)
      (values result))))


;; Convert unix file descriptor to Racket ports.
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
                  _int --> _string/locale)
            #:c-id zmq_strerror)


;; Context
(define-zmq zmq-ctx-destroy
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer --> _int)
            #:c-id zmq_ctx_new
            #:wrap (compose checked-result (deallocator first)))

(define-zmq zmq-ctx-new
            (_fun #:save-errno 'posix
                  --> _zmq-ctx-pointer/null)
            #:c-id zmq_ctx_new
            #:wrap (compose (allocator zmq-ctx-destroy) checked-result))

(define-zmq zmq-ctx-set
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer _zmq-ctx-option-name _int --> _int)
            #:c-id zmq_ctx_set
            #:wrap checked-result)

(define-zmq zmq-ctx-get
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer _zmq-ctx-option-name --> _int)
            #:c-id zmq_ctx_get
            #:wrap checked-result)


;; Messages
(define-zmq zmq-msg-close
            (_fun #:save-errno 'posix
                  (_ptr i _zmq-msg) --> _int)
            #:c-id zmq_msg_close
            #:wrap (compose checked-result (deallocator first)))

(define-zmq zmq-msg-init
            (_fun #:save-errno 'posix
                  (msg : (_ptr o _zmq-msg))
                  --> _int
                  --> msg)
            #:c-id zmq_msg_init
            #:wrap (allocator zmq-msg-close))

(define-zmq zmq-msg-size
            (_fun (_ptr i _zmq-msg) --> _size)
            #:c-id zmq_msg_size)

(define-zmq zmq-msg-data
            (_fun (_ptr i _zmq-msg) --> _pointer)
            #:c-id zmq_msg_data)

(define-zmq zmq-msg-more?
            (_fun (_ptr i _zmq-msg) --> _bool)
            #:c-id zmq_msg_more)

(define-zmq zmq-msg-recv
            (_fun #:save-errno 'posix
                  (msg : (_ptr io _zmq-msg) = (zmq-msg-init))
                  _zmq-socket-pointer
                  (_bitmask '(dontwait = 1))
                  --> (result : _int)
                  --> (begin
                        (check-result result)
                        msg))
            #:c-id zmq_msg_recv)

(define (zmq-msg->bytes msg)
  (bytes-copy (make-sized-byte-string (zmq-msg-data msg) (zmq-msg-size msg))))


;; Socket
(define-zmq zmq-close
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer --> _int)
            #:c-id zmq_close
            #:wrap (compose checked-result (deallocator first)))

(define-zmq zmq-socket
            (_fun #:save-errno 'posix
                  _zmq-ctx-pointer
                  _zmq-socket-type
                  --> _zmq-socket-pointer/null)
            #:c-id zmq_socket
            #:wrap (compose (retain-parent first)
                            (allocator zmq-close)
                            checked-result))

(define-zmq zmq-getsockopt/int
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  (name : _zmq-int-option-name)
                  (value : (_ptr o _int))
                  (size : (_ptr io _size) = (ctype-sizeof _int))
                  --> (result : _int)
                  --> (begin
                        (check-result result)
                        value))
            #:c-id zmq_getsockopt)

(define-zmq zmq-setsockopt/int
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  (name : _zmq-int-option-name)
                  (value : (_ptr i _int))
                  (size : _size = (ctype-sizeof _int))
                  --> _int)
            #:c-id zmq_setsockopt
            #:wrap checked-result)

(define-zmq zmq-getsockopt/bstr
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  (name : _zmq-bstr-option-name)
                  (value : _pointer = (make-bytes 256))
                  (size : (_ptr io _size) = 256)
                  --> (result : _int)
                  --> (begin
                        (check-result result)
                        (bytes-copy (make-sized-byte-string value size))))
            #:c-id zmq_getsockopt)

(define-zmq zmq-setsockopt/bstr
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer
                  (name : _zmq-bstr-option-name)
                  (value : _pointer)
                  (size : _size = (bytes-length value))
                  --> _int)
            #:c-id zmq_setsockopt
            #:wrap checked-result)

(define-zmq zmq-bind
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer _string/utf-8 --> _int)
            #:c-id zmq_bind
            #:wrap checked-result)

(define-zmq zmq-connect
            (_fun #:save-errno 'posix
                  _zmq-socket-pointer _string/utf-8 --> _int)
            #:c-id zmq_connect
            #:wrap checked-result)

(define-zmq zmq-send
            (_fun _zmq-socket-pointer
                  (buf : _bytes)
                  (_size = (bytes-length buf))
                  (_bitmask '(dontwait = 1
                              sndmore  = 2))
                  --> _int)
            #:c-id zmq_send
            #:wrap checked-result)


; vim:set ts=2 sw=2 et:
