#lang scribble/manual

@require[(for-label racket)]
@require[(for-label "main.rkt")]

@title{Minimal ZeroMQ Bindings}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

@defmodule[zmq]

@defproc[(socket? (v any/c)) boolean?]{
  Predicate identifying ZeroMQ sockets.
}

@defproc[(socket-type? (v any/c)) boolean?]{
  Predicate for valid socket types.
  Currently recognizes @racket['(pub sub router)].
}

@defproc[(socket (type socket-type?)
                 (#:identity identity (or/c #f string? bytes?))
                 (#:subscribe subscribe (listof (or/c string? bytes?)))
                 (#:bind bind (listof string?))
                 (#:connect connect (listof string?))
                 (#:send-queue send-queue (or/c #f exact-positive-integer?))
                 (#:receive-queue receive-queue (or/c #f exact-positive-integer?)))
           socket?]{
  Create new socket of specified @racket[type] and optionally immediately
  @racket[bind] and/or @racket[connect] it, using defined @racket[identity].

  The @racket[send-queue] and @racket[receive-queue] options correspond to
  ZeroMQ high-water marks. That is, number of packets to buffer when the
  peer is not available.
}

@defproc*[(((socket-identity (s socket?)) bytes?)
           ((socket-identity (s socket?) (identity (or/c string? bytes?))) void?))]{
  Determine or change socket identity.

  Changing the identity after connections have been established
  (via binding or connecting) will most likely not work as desired.
}

@defproc[(socket-send (s socket?) (part (or/c bytes? string?)) ...) void?]{
  Send a multi-part message.

  When operating in the @racket['router] mode, first part is reserved for
  target peer's identity.
}

@defproc[(socket-receive/list (s socket?)) (listof bytes?)]{
  Receive a multi-part message as a list of it's parts.

  When operating in the @racket['router] mode, first part is reserved for
  target peer's identity.
}

@defproc[(socket-receive (s socket?)) any]{
  Same as @racket[socket-receive/list], but returns multiple values.
}

@defproc[(socket-bind (s socket?) (addr string?)) void?]{
  Bind to specified ZeroMQ endpoint address such as
  @racket["tcp://127.0.0.1:1234"].
}

@defproc[(socket-connect (s socket?) (addr string?)) void?]{
  Connect to specified ZeroMQ endpoint address such as
  @racket["tcp://127.0.0.1:1234"].
}

@defproc[(socket-subscribe (s socket?) (prefix (or/c bytes? string?))) void?]{
  Used with @racket['sub] sockets to define prefix of messages to
  receive from a peer @racket['pub] socket.
}

@defproc[(socket-unsubscribe (s socket?) (prefix (or/c bytes? string?))) void?]{
  Cancel subscription made with @racket[socket-subscribe].
}

@; vim:set ft=scribble sw=2 ts=2 et:
