#lang racket/base
;
; ZeroMQ Bindings
;

(require racket/contract)

(require "private/ffi.rkt")

(provide (except-out
           (all-defined-out) zmq-context socket))


;; Single context with only one I/O thread.
(define zmq-context (zmq-ctx-new))


(define-struct socket
  (s     ; The actual C object.
   evt)  ; Input port for event notifications (sync on it).
  #:constructor-name new-socket)


(define (socket-type? v)
  ;; We only support non-blocking socket types.
  (list? (member v '(pub sub router))))


(define/contract (make-socket type #:identity (identity #f)
                                   #:subscribe (subscribe null))
                 (->* (socket-type?)
                      (#:identity (or/c #f string? bytes?)
                       #:subscribe (listof (or/c string? bytes?)))
                      socket?)
  ;; Create the underlying C object.
  (let ((s (zmq-socket zmq-context type)))
    ;; Extract notification port.
    (let-values (((in out) (socket->ports (zmq-getsockopt/int s 'fd) "zmq")))
      ;; Create socket structure.
      (let ((socket (new-socket s in)))
        ;; Set socket identity, if specified.
        (when identity
          (set-socket-identity! socket identity))

        ;; Subscribe to given prefixes.
        (for ((prefix (in-list subscribe)))
          (socket-subscribe! socket prefix))

        ;; Return the new socket.
        socket))))


(define/contract (set-socket-identity! socket name)
                 (-> socket? (or/c #f string? bytes?) void?)
  (let ((name (if (string? name) (string->bytes/utf-8 name) name)))
    (zmq-setsockopt/bstr (socket-s socket) 'identity name)))


(define/contract (socket-identity socket)
                 (-> socket? bytes?)
  (zmq-getsockopt/bstr (socket-s socket) 'identity))


(define/contract (socket-send socket . parts)
                 (->* (socket?) () #:rest (listof (or/c bytes? string?)) void?)
  (unless (null? parts)
    (let ((value (if (string? (car parts))
                   (string->bytes/utf-8 (car parts))
                   (car parts))))
      (zmq-send (socket-s socket) value
                (if (null? (cdr parts)) '() '(sndmore))))
    (apply socket-send socket (cdr parts))))


(define/contract (socket-receive/list socket)
                 (-> socket? (listof bytes?))
  (let poll ()
    (let ((flags (zmq-getsockopt/int (socket-s socket) 'events)))
      (unless (bitwise-bit-set? flags 0)
        (sync (socket-evt socket))
        (poll))))

  (let loop ()
    (let ((msg (zmq-msg-recv (socket-s socket) '(dontwait))))
      (if (zmq-msg-more? msg)
        (cons (zmq-msg->bytes msg) (loop))
        (list (zmq-msg->bytes msg))))))


(define/contract (socket-receive socket)
                 (-> socket? any)
  (apply values (socket-receive/list socket)))


(define/contract (socket-bind socket endpoint)
                 (-> socket? string? void?)
  (zmq-bind (socket-s socket) endpoint))


(define/contract (socket-connect socket endpoint)
                 (-> socket? string? void?)
  (zmq-connect (socket-s socket) endpoint))


(define/contract (socket-subscribe! socket prefix)
                 (-> socket? (or/c bytes? string?) void?)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-s socket) 'subscribe prefix)))


(define/contract (socket-unsubscribe! socket prefix)
                 (-> socket? (or/c bytes? string?) void?)
  (let ((prefix (if (string? prefix) (string->bytes/utf-8 prefix) prefix)))
    (zmq-setsockopt/bstr (socket-s socket) 'unsubscribe prefix)))


; vim:set ts=2 sw=2 et:
