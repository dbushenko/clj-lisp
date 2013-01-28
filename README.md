# clj-lisp

This is a metacyclic interpretor as in SICP but written with Clojure. Also it is designed to be functional-style since it doesn't have the 'set!' or 'define' operator. If you need to define a variable or a function -- use 'let'.

The body of 'let' and 'lambda' must consist of the 'do' block like this:

    (let [a 1
          b 2]
       (do
          ((primitive +) 1 2)))

Since this is not a production-ready lisp, it contains NO predefined functions. It doesn't have even arythmetics -- everything is up to you. But you may use host language (Clojure) and call its functions using the word 'primitive' like this:

    ((primitive println) "Hello world!")

You may predefine some functions and use them further like this:

    (let [+ (lambda [a b] (do ((primitive +) a b)))]
       (+ 1 2))

A larger example see in 'example.lisp'.
  
## Usage

    $ lein uberjar
    $ run.sh

## License

Copyright © 2013 by Dmitry Bushenko

Distributed under the Eclipse Public License, the same as Clojure.
