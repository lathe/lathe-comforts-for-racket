#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/promise.scrbl
@;
@; Utilities for promises.

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


@title[#:tag "promise"]{Utilities for Promises}

@defmodule[lathe-comforts/promise]


@defproc[
  (promise-map
    [promise (promise/c any/c)]
    [on-value (-> any/c any/c)])
  (promise/c any/c)
]{
  Returns a promise that first forces the given one and then transforms its result using @racket[on-value].
}
