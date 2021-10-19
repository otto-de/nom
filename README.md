# nom

Utilities for working with the happy path in the face of anomalies.

(The name »nom« is a bad pun on the word »anomaly«.  »Anomaly« is actually
composed from »an-« and »homalos«, so the antonym of »anomal« should be »homal«.
However, a funny name is obviously more important than etymological accuracy.)

## Rationale

One popular way of signalling errors in Clojure is to return _anomalies_,
i. e. data structures that carry information about the error and can be
identified as such.

This library supports the handling of potential anomalies by providing tools to
transparently propagate anomalies without obscuring the logic flow.

The convention we propose hereby is: functions don't need to expect anomalies in
their formal parameters.  Instead, whenever we have an anomaly at hand, we
immediately propagate it to any dependent values.  The actual handling of
whatever anomalies may have occurred is deferred to the outside, e. g. at the
level of an HTTP handler.

## Usage

Dependency: `[de.otto/nom "1.0.0"]`

Require: `[de.otto.nom.core :as nom]`

### Creation

You can create a nom anomaly using `fail`:

    (nom/fail ::my-category {:some data})

Our own anomaly format is a bit different from the `cognitect.anomalies` format,
but those are recognized too and automatically adapted.  In order to extend this
behaviour to your own anomaly formats, extend the `abominable?` and `adapt`
multimethods using a proper predicate.

### Propagation

The basic principle is maybe best illustrated by showing `nom`.  If you have a
form like this:

    (foo arg0 arg1)

where both `arg0` or `arg1` might be anomalies, then you can prepend `nom`:

    (nom/nom foo arg0 arg1)

which does the same as the form above, but if `arg0` or `arg1` is an anomaly,
then it will immediately return that anomaly and not call `foo` at all.

You can do this in a threading manner very similar to `some->` and `some->>`,
with `nom->` and `nom->>`:

    (nom/nom-> foo
               (bar baz)
               quux
               (wizzle wozzle))

which would be equivalent to:

    (nom/nom wizzle
             (nom/nom quux
                      (nom/nom bar foo baz))
             wozzle)

i. e. thread the values just as in `->`, but if any of the intermediate values
is an anomaly, short-circuit and return that anomaly.

Another pattern is checking a number of values, then executing a body, but if
any of the values is an anomaly, short-circuit and return the anomaly instead.
That's what `with-nom` is for:

    (nom/with-nom [something (something else) (and another thing)]
      (frob something)
      (wozzle another thing))

Of course, this begs for binding those values; we have `let-nom>` for that:

    (nom/let-nom> [a something
                   b (something else)
                   c (and another thing)]
      (frob a b c))

This short-ciruits as soon as an anomaly is encountered in the bindings; in that
case, no further binding nor the body is even evaluated.  You might want to have
anomalies propagate through the bindings instead and then do something about it
in the body.  That's what `let-nom` (without the `>`) does.

## License

Copyright © 2021 OTTO GmbH & Co. KG

This program and the accompanying materials are made available under the terms
of the Apache License 2.0, see https://www.apache.org/licenses/LICENSE-2.0.html
