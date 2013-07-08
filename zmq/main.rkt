#lang racket/base
;
; ZeroMQ Bindings
;

(require racket/contract
         racket/function
         (only-in ffi/unsafe register-finalizer))

(require "private/ffi.rkt")

(provide (except-out
           (all-defined-out) zmq-context socket drain string->bytes/safe))


;; Single context with only one I/O thread.
(define zmq-context (zmq-ctx-new))


(define-struct socket
  (type  ; Socket mode.
   sock  ; The actual C object.
   inch  ; Channel for message receiving.
   ping) ; Semaphore to ping input pump.
  #:constructor-name new-socket)


(define (socket-type? v)
  ;; We only support non-blocking socket types.
  (list? (member v '(pub sub router))))


(define/contract (make-socket type #:identity (identity #f)
                                   #:subscribe (subscribe null)
                                   #:bind (bind null)
                                   #:connect (connect null))
                 (->* (socket-type?)
                      (#:identity (or/c #f string? bytes?)
                       #:subscribe (listof (or/c string? bytes?))
                       #:bind (listof string?)
                       #:connect (listof string?))
                      socket?)
  ;; Create the underlying C object.
  (let ((s    (zmq-socket zmq-context type))
        (inch (make-channel))
        (ping (make-semaphore)))
    ;; Extract notification port.
    (let-values (((in out) (socket->ports (zmq-getsockopt/int s 'fd) "zmq")))
      ;; Create socket structure.
      (let ((socket (new-socket type s inch ping)))
        ;; Set socket identity, if specified.
        (when identity
          (set-socket-identity! socket identity))

        ;; Bind to given endpoints.
        (for ((endpoint (in-list bind)))
          (socket-bind socket endpoint))

        ;; Connect to given endpoints.
        (for ((endpoint (in-list connect)))
          (socket-connect socket endpoint))

        ;; Subscribe to given prefixes.
        (for ((prefix (in-list subscribe)))
          (socket-subscribe! socket prefix))

        ;; Pump channels from socket to the channel.
        (unless (eq? type 'pub)
          (define pump
            (thread
              (thunk
                (for ((msg (in-producer drain #f s (choice-evt in ping))))
                  (channel-put inch msg)))))

          ;; Kill the pump once our socket gets forgotten.
          (register-finalizer socket
            (lambda (socket)
              (thread-suspend pump))))

        ;; Return the new socket.
        socket))))


;; Wait for and read a message from socket.
(define (drain sock evt)
  (define (poll)
    (let ((flags (zmq-getsockopt/int sock 'events)))
      (unless (bitwise-bit-set? flags 0)
        (sync evt)
        (poll))))

  (let loop ()
    (with-handlers ((exn:fail:zmq:again? (lambda (exn) (poll) (loop))))
      (let ((msg (zmq-msg-recv sock '(dontwait))))
        (if (zmq-msg-more? msg)
          (cons (zmq-msg->bytes msg) (loop))
          (list (zmq-msg->bytes msg)))))))


(define/contract (set-socket-identity! socket name)
                 (-> socket? (or/c #f string? bytes?) void?)
  (let ((name (if (string? name) (string->bytes/utf-8 name) name)))
    (zmq-setsockopt/bstr (socket-sock socket) 'identity name)))


(define/contract (socket-identity socket)
                 (-> socket? bytes?)
  (zmq-getsockopt/bstr (socket-sock socket) 'identity))


(define/contract (string->bytes/safe str)
                 (-> (or/c string? bytes?) bytes?)
  (if (bytes? str) str (string->bytes/utf-8 str)))


(define/contract (socket-send socket . parts)
                 (->* (socket?) () #:rest (listof (or/c bytes? string?)) void?)
  (if (null? parts)
    (unless (eq? 'pub (socket-type socket))
      ;; All parts have been sent.  Ping receiver to catch up.
      (semaphore-post (socket-ping socket)))

    ;; Send one more part.
    (let ((value (string->bytes/safe (car parts)))
          (flags (if (null? (cdr parts)) '() '(sndmore))))
      (zmq-send (socket-sock socket) value flags)

      ;; Recurse and do the same with other parts.
      (apply socket-send socket (cdr parts)))))


(define/contract (socket-receive/list socket)
                 (-> socket? (listof bytes?))
  (channel-get (socket-inch socket)))


(define/contract (socket-receive socket)
                 (-> socket? any)
  (apply values (socket-receive/list socket)))


(define/contract (socket-bind socket endpoint)
                 (-> socket? string? void?)
  (zmq-bind (socket-sock socket) endpoint))


(define/contract (socket-connect socket endpoint)
                 (-> socket? string? void?)
  (zmq-connect (socket-sock socket) endpoint))


(define/contract (socket-subscribe! socket prefix)
                 (-> socket? (or/c bytes? string?) void?)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-sock socket) 'subscribe prefix)))


(define/contract (socket-unsubscribe! socket prefix)
                 (-> socket? (or/c bytes? string?) void?)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-sock socket) 'unsubscribe prefix)))


(define/contract (socket-receive-evt socket)
                 (-> socket? evt?)
  (guard-evt (thunk (socket-inch socket))))


; vim:set ts=2 sw=2 et:
