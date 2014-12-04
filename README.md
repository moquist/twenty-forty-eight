# twenty-forty-eight

A Clojure implementation of the game 2048, currently playable only in the REPL.

## Usage

```clojure
(require 'twenty-forty-eight.core)
(in-ns 'twenty-forty-eight.core)
(def b (atom (init-board)))
(swap! b move :u)
(swap! b move :d)
(swap! b move :l)
(swap! b move :r)
```

I've been working on simple heuristic "AI" players, to see how different simple
heuristics perform. If you want to try them out (in order of goodness):
```clojure
(time (play-ai->stats a-stubborn 1000)) ; Worse than random!
(time (play-ai->stats a-not-i 1000)) ; random null model
(time (play-ai->stats ai-pref-dir 1000)) ; better
(time (play-ai->stats ai-pref-dir-watch-blanks 1000)) ; best heuristic so far
(play-ai (human-intelligence))
```


## License

Copyright © 2014 Matt Oquist <moquist@majen.net>

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
