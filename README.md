# twenty-forty-eight

A Clojure implementation of the game 2048, currently playable only in the REPL.

## Usage

```clojure
(def b (atom (init-board)))
(swap! b move :u)
(swap! b move :d)
(swap! b move :l)
(swap! b move :r)
```

## License

Copyright © 2014 Matt Oquist <moquist@majen.net>

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
