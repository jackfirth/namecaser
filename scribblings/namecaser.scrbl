#lang scribble/manual

@(require (for-label namecaser
                     racket/base))

@title{Namecaser}

@defmodule[namecaser/base]

Namecaser is a library for converting names between different programming language naming styles.


@defproc[(upper-camel-case [name (or/c ascii-string? (sequence/c word?))])
         (and/c camel-case-ascii-string? immutable?)]{
 Conerts @racket[name] to upper camel case.}
