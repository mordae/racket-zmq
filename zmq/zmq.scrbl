#lang scribble/manual

@require[(for-label (except-in typed/racket/base)
                    (only-in racket/base))]
@require[(for-label zmq)]

@title{Minimal ZeroMQ Bindings}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

@defmodule[zmq]

This module is typed and can be used from both normal and typed code.


@defidform[Socket]{
  Type of a ZeroMQ socket of any kind.
}

@defidform[Socket-Kind]{
  Type of a ZeroMQ socket kind, equivalent to @racket[(U 'router 'pub 'sub)].

  The @racket['router] socket can send messages to any of it's peers,
  always prefixed with that particular peer's identity part. The other two
  kinds @racket['pub] and @racket['sub] can connect to each other and allow
  one-way distribution of messages without the need to identify individual
  recipients on the sender's side.
}

@defproc[(socket? (v Any)) Boolean]{
  Predicate for @racket[Socket].
}

@defproc[(socket-kind? (v Any)) Boolean]{
  Predicate for @racket[Socket-Kind].
}

@defproc[(socket (kind Socket-Kind)
                 (#:identity identity (U String Bytes))
                 (#:subscribe subscribe (Listof (U String Bytes)))
                 (#:bind bind (Listof String))
                 (#:connect connect (Listof String))
                 (#:send-queue send-queue Natural)
                 (#:receive-queue receive-queue Natural))
           Socket]{
  Create new socket of specified @racket[kind] and optionally immediately
  @racket[bind] and/or @racket[connect] it, using defined @racket[identity].

  The @racket[send-queue] and @racket[receive-queue] options correspond to
  ZeroMQ high-water marks. That is, number of packets to buffer when the
  peer is not available.
}

@defproc*[(((socket-identity (s Socket)) Bytes)
           ((socket-identity (s Socket) (identity (U String Bytes))) Void))]{
  Determine or change socket identity.

  Changing the identity after connections have been established
  (via binding or connecting) will most likely not work as desired.
}

@defproc[(socket-send (s Socket) (part (U Bytes String)) ...) Void]{
  Send a multi-part message.

  When operating in the @racket['router] mode, first part is reserved for
  target peer's identity.
}

@defproc[(socket-receive (s Socket)) (Listof Bytes)]{
  Receive a multi-part message as a list of it's parts.

  When operating in the @racket['router] mode, first part is reserved for
  target peer's identity.
}

@defproc[(socket-bind (s Socket) (addr String)) Void]{
  Bind to specified ZeroMQ endpoint address such as
  @racket["tcp://127.0.0.1:1234"].
}

@defproc[(socket-connect (s Socket) (addr String)) Void]{
  Connect to specified ZeroMQ endpoint address such as
  @racket["tcp://127.0.0.1:1234"].
}

@defproc[(socket-subscribe (s Socket) (prefix (U String Bytes))) Void]{
  Used with @racket['sub] sockets to define prefix of messages to
  receive from a peer @racket['pub] socket.
}

@defproc[(socket-unsubscribe (s Socket) (prefix (U String Bytes))) Void]{
  Cancel subscription made with @racket[socket-subscribe].
}

@; vim:set ft=scribble sw=2 ts=2 et:
