#lang typed/racket/base
;
; ZeroMQ Bindings
;

(require racket/function)

(require mordae/async)

(require/typed zmq/private/ffi
  (#:opaque Context-Pointer zmq-ctx-pointer?)
  (#:opaque Socket-Pointer zmq-socket-pointer?)
  (#:opaque Message zmq-msg?)

  (socket->read-port
    (-> Integer String Input-Port))

  (zmq-ctx-new
    (-> Context-Pointer))
  (zmq-socket
    (-> Context-Pointer Socket-Kind Socket-Pointer))
  (zmq-getsockopt/int
    (-> Socket-Pointer Symbol Integer))
  (zmq-setsockopt/int
    (-> Socket-Pointer Symbol Integer Integer))
  (zmq-getsockopt/bstr
    (-> Socket-Pointer Symbol Bytes))
  (zmq-setsockopt/bstr
    (-> Socket-Pointer Symbol Bytes Integer))
  (zmq-msg-recv
    (-> Socket-Pointer (Listof (U 'dontwait)) Message))
  (zmq-msg-more?
    (-> Message Boolean))
  (zmq-msg->bytes
    (-> Message Bytes))
  (zmq-send
    (-> Socket-Pointer Bytes (Listof (U 'dontwait 'sndmore)) Integer))
  (zmq-bind
    (-> Socket-Pointer String Integer))
  (zmq-connect
    (-> Socket-Pointer String Integer))

  (#:struct (exn:fail:zmq exn:fail) ())
  (#:struct (exn:fail:zmq:again exn:fail:zmq) ()))

(provide socket?
         socket-kind?
         socket-identity
         socket-send
         socket-receive
         socket-bind
         socket-connect
         socket-subscribe
         socket-unsubscribe)

(provide
  (rename-out (new-socket socket)))

(provide Socket
         Socket-Kind)


;; Single context with only one I/O thread.
(define zmq-context (zmq-ctx-new))


;; Special type for socket variants.
(define-type Socket-Kind (U 'pub 'sub 'router))
(define-predicate socket-kind? Socket-Kind)

;; Alias for our primary structure as `socket` itself is already
;; occupied by the constructor name in our provides.
(define-type Socket socket)


;; Structure representing a socket, using a background racket thread
;; with a channel to receive messages.
(struct socket
  ((kind : Socket-Kind)
   (sock : Socket-Pointer)
   (recv-evt : (Evtof (Listof Bytes)))
   (ping : Semaphore))
  #:property prop:evt (struct-field-index recv-evt))


(: new-socket (->* (Socket-Kind)
                   (#:identity (U String Bytes)
                    #:subscribe (Listof (U String Bytes))
                    #:bind (Listof String)
                    #:connect (Listof String)
                    #:send-queue Natural
                    #:receive-queue Natural)
                   Socket))
(define (new-socket kind #:identity (identity #f)
                    #:subscribe (subscribe null)
                    #:bind (bind null)
                    #:connect (connect null)
                    #:send-queue (send-queue #f)
                    #:receive-queue (receive-queue #f))
  ;; Create the underlying C object.
  (let* ((s (zmq-socket zmq-context kind))
         (in (socket->read-port (zmq-getsockopt/int s 'fd) "zmq"))
         (ping (make-semaphore))
         (recv-evt (async-task-evt
                     (async/loop
                       (drain s (choice-evt in ping))))))
    ;; Create socket structure.
    (let ((socket (socket kind s recv-evt ping)))
      ;; Set socket identity, if specified.
      (when identity
        (socket-identity socket identity))

      ;; Send outgoing queue maximum length.
      (when send-queue
        (zmq-setsockopt/int s 'sndhwm send-queue))

      ;; Send incoming queue maximum length.
      (when receive-queue
        (zmq-setsockopt/int s 'rcvhwm receive-queue))

      ;; Bind to given endpoints.
      (for ((endpoint : String bind))
        (socket-bind socket endpoint))

      ;; Connect to given endpoints.
      (for ((endpoint : String connect))
        (socket-connect socket endpoint))

      ;; Subscribe to given prefixes.
      (for ((prefix : (U String Bytes) subscribe))
        (socket-subscribe socket prefix))

      ;; Return the new socket.
      socket)))


;; Wait for and read a message from socket.
(: drain (-> Socket-Pointer (Evtof Any) (Listof Bytes)))
(define (drain sock evt)
  (define (poll) : Void
    (let ((flags (zmq-getsockopt/int sock 'events)))
      (unless (bitwise-bit-set? flags 0)
        (sync evt)
        (poll))))

  (let loop : (Listof Bytes) ()
    (with-handlers ((exn:fail:zmq:again? (lambda (exn) (poll) (loop))))
      (let ((msg (zmq-msg-recv sock '(dontwait))))
        (if (zmq-msg-more? msg)
          (cons (zmq-msg->bytes msg) (loop))
          (list (zmq-msg->bytes msg)))))))


(define socket-identity
  (case-lambda
    (((s : Socket))
     (zmq-getsockopt/bstr (socket-sock s) 'identity))

    (((s : Socket) (name : (U String Bytes)))
     (let ((name (string->bytes/safe name)))
       (void (zmq-setsockopt/bstr (socket-sock s) 'identity name))))))


(: string->bytes/safe (-> (U String Bytes) Bytes))
(define (string->bytes/safe str)
  (if (bytes? str) str (string->bytes/utf-8 str)))


(: socket-send (-> Socket (U String Bytes) * Void))
(define (socket-send socket . parts)
  (if (null? parts)
    (unless (eq? 'pub (socket-kind socket))
      ;; All parts have been sent.  Ping receiver to catch up.
      (semaphore-post (socket-ping socket)))

    ;; Send one more part.
    (let ((value (string->bytes/safe (car parts)))
          (flags (if (null? (cdr parts)) '() '(sndmore))))
      (zmq-send (socket-sock socket) value flags)

      ;; Recurse and do the same with other parts.
      (apply socket-send socket (cdr parts)))))


(: socket-receive (-> Socket (Listof Bytes)))
(define (socket-receive socket)
  (sync (socket-recv-evt socket)))


(: socket-bind (-> Socket String Void))
(define (socket-bind socket endpoint)
  (void (zmq-bind (socket-sock socket) endpoint)))


(: socket-connect (-> Socket String Void))
(define (socket-connect socket endpoint)
  (void (zmq-connect (socket-sock socket) endpoint)))


(: socket-subscribe (-> Socket (U String Bytes) Void))
(define (socket-subscribe socket prefix)
  (let ((prefix (string->bytes/safe prefix)))
    (void
      (zmq-setsockopt/bstr (socket-sock socket) 'subscribe prefix))))


(: socket-unsubscribe (-> Socket (U String Bytes) Void))
(define (socket-unsubscribe socket prefix)
  (let ((prefix (string->bytes/safe prefix)))
    (void
      (zmq-setsockopt/bstr (socket-sock socket) 'unsubscribe prefix))))


; vim:set ts=2 sw=2 et:
