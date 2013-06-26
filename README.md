# Minimal Racket ZeroMQ Bindings

## Subscriber

```racket
(require zmq)

(define socket (make-socket 'sub
                            #:subscribe '("foo" "bar")))

(socket-connect socket "tcp://127.0.0.1:1234")

(for ((parts (in-producer socket-receive/list #f socket)))
  (printf "received ~s\n" parts))
```

## Producer

```racket
(require zmq)

(define socket (make-socket 'pub))
(socket-bind socket "tcp://127.0.0.1:1234")

(let loop ()
  (socket-send socket "foo" "Hello World!")
  (sleep 1)
  (loop))
```

## Router

```racket
(require zmq)

(define socket (make-socket 'router #:identity "hub"))
(socket-bind socket "tcp://127.0.0.1:4321")

(for ((parts (in-producer socket-receive/list #f socket)))
  (printf "received ~s\n" parts)
  ;; Echo the message back.
  (apply socket-send socket parts))
```
