# Minimal Racket ZeroMQ Bindings

## Subscriber

```racket
(require zmq)

(define sub
  (socket 'sub
          #:subscribe '("foo" "bar")
          #:connect '("tcp://127.0.0.1:1234")))

(for ((parts (in-producer socket-receive/list #f sub)))
  (printf "received ~s\n" parts))
```

## Publisher

```racket
(require zmq)

(define pub
  (socket 'pub #:bind '("tcp://127.0.0.1:1234")))

(for ((i (in-producer sleep #f 1)))
  (socket-send pub "foo" "Hello World!"))
```

## Router

```racket
(require zmq)

(define echo
  (socket 'router
          #:identity "hub"
          #:bind '("tcp://127.0.0.1:4321")))

(for ((parts (in-producer socket-receive/list #f echo)))
  (printf "received ~s\n" parts)
  (apply socket-send echo parts))
```
