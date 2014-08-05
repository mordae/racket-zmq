#lang racket/base
;
; ZeroMQ Bindings
;

(require racket/contract
         racket/function
         racket/undefined)

(require misc1/async)

(require "private/ffi.rkt")

(provide
  (contract-out
    (socket? predicate/c)
    (socket-type? predicate/c)

    (rename make-socket socket
            (->* (socket-type?)
                 (#:identity (or/c #f string? bytes?)
                  #:subscribe (listof (or/c string? bytes?))
                  #:bind (listof string?)
                  #:connect (listof string?))
                 socket?))

    (socket-identity
      (->* (socket?) ((or/c string? bytes?)) (or/c void? bytes?)))

    (socket-send
      (->* (socket?) () #:rest (listof (or/c bytes? string?)) void?))

    (socket-receive/list
      (-> socket? (listof bytes?)))

    (socket-receive
      (-> socket? any))

    (socket-bind
      (-> socket? string? void?))

    (socket-connect
      (-> socket? string? void?))

    (socket-subscribe
      (-> socket? (or/c bytes? string?) void?))

    (socket-unsubscribe
      (-> socket? (or/c bytes? string?) void?))))


;; Single context with only one I/O thread.
(define zmq-context (zmq-ctx-new))


;; Structure representing a socket, using a background racket thread
;; with a channel to receive messages.
(struct socket
  (type sock recv-evt ping)
  #:property prop:evt (struct-field-index recv-evt))


(define (socket-type? v)
  ;; We only support non-blocking socket types.
  (list? (member v '(pub sub router))))


(define (make-socket type #:identity (identity #f)
                          #:subscribe (subscribe null)
                          #:bind (bind null)
                          #:connect (connect null))
  ;; Create the underlying C object.
  (let*-values (((s) (zmq-socket zmq-context type))
                ((in out) (socket->ports (zmq-getsockopt/int s 'fd) "zmq"))
                ((ping) (make-semaphore))
                ((recv-evt) (async/loop
                              (drain s (choice-evt in ping)))))
    ;; Create socket structure.
    (let ((socket (socket type s recv-evt ping)))
      ;; Set socket identity, if specified.
      (when identity
        (socket-identity socket identity))

      ;; Bind to given endpoints.
      (for ((endpoint (in-list bind)))
        (socket-bind socket endpoint))

      ;; Connect to given endpoints.
      (for ((endpoint (in-list connect)))
        (socket-connect socket endpoint))

      ;; Subscribe to given prefixes.
      (for ((prefix (in-list subscribe)))
        (socket-subscribe socket prefix))

      ;; Return the new socket.
      socket)))


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


(define (socket-identity socket (name undefined))
  (cond
    ((eq? name undefined)
     (zmq-getsockopt/bstr (socket-sock socket) 'identity))

    (else
     (let ((name (string->bytes/safe name)))
       (void (zmq-setsockopt/bstr (socket-sock socket) 'identity name))))))


(define (string->bytes/safe str)
  (if (bytes? str) str (string->bytes/utf-8 str)))


(define (socket-send socket . parts)
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


(define (socket-receive/list socket)
  (sync socket))


(define (socket-receive socket)
  (apply values (socket-receive/list socket)))


(define (socket-bind socket endpoint)
  (zmq-bind (socket-sock socket) endpoint))


(define (socket-connect socket endpoint)
  (zmq-connect (socket-sock socket) endpoint))


(define (socket-subscribe socket prefix)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-sock socket) 'subscribe prefix)))


(define (socket-unsubscribe socket prefix)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-sock socket) 'unsubscribe prefix)))


; vim:set ts=2 sw=2 et:
