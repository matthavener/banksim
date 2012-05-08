# banksim

Very similar to ants.clj by Rich Hickey. Some trucks (agents) move money on a grid (refs) from a central bank to division banks. The division banks are decayed and central bank is refilled with another actor. The entire system is drawn with JPanel.

## Usage

If you don't have `lein`, just [download the script](https://raw.github.com/technomancy/leiningen/stable/bin/lein)

Then run, `lein repl`

Once the repl is loaded, call the start function with `(start)`

## License

Copyright (C) 2012 Matt Havener

Distributed under the Eclipse Public License, the same as Clojure.
