#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/yknow.scrbl
@;
@; Yknow objects, a representation for pending computations that can
@; turn out to allow user-specified results.

@;   Copyright 2024 The Lathe Authors
@;
@;   Licensed under the Apache License, Version 2.0 (the "License");
@;   you may not use this file except in compliance with the License.
@;   You may obtain a copy of the License at
@;
@;       http://www.apache.org/licenses/LICENSE-2.0
@;
@;   Unless required by applicable law or agreed to in writing,
@;   software distributed under the License is distributed on an
@;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
@;   either express or implied. See the License for the specific
@;   language governing permissions and limitations under the License.


@(require lathe-comforts/scribblings/private/shim)
@(init-shim)


@title[#:tag "yknow"]{Yknow Values, for Sometimes-User-Specified Computations}

@defmodule[lathe-comforts/yknow]

@deftech{Yknow values} are a way to represent @racket[promise?]-like computations that may turn out to have not-yet-specified or user-specifiable results. The not-yet-specified results may be missing for some versions of a program but may eventually be present once the program has been sufficiently upgraded, as with @tech{knowable values}. The user-specifiable results represent a stable absence of information that the user can handle in the way they see fit, as with @tech{maybe values}. Specifically, a yknow value is equivalent to a @racket[promise?] containing a @tech{knowable value} containing a possible @tech{maybe value} containing a possible @racket[promise?].


@defproc[(yknow? [v any/c]) boolean?]{
  Returns whether the given value is a @tech{yknow value}.
}

@; TODO YKNOW DOCS: Document these.
@;{@[
(provide /own-contract-out
  yknow?
  yknow-value-promise-maybe-knowable-promise
  yknow/c
  make-yknow-from-value-promise-maybe-knowable-promise)
(provide
  ; TODO: Stop exporting this.
  yknow-augment)
(provide /own-contract-out
  make-yknow-from-value-knowable-promise
  make-yknow-from-value
  uninformative-yknow
  yknow-value-promise-knowable
  yknow-value-knowable
  yknow-known-specified?
  yknow-value
  yknow-value-promise-maybe-knowable-promise-map
  yknow-value-promise-maybe-knowable-map
  yknow-map
  yknow-map/knowable
  yknow-joininfo*
  yknow-maybe-yknow-joininfo*)
]}
