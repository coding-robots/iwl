I Write Like
============

Running
-------

To launch the web server:

	racket app.rkt

and open `http://localhost:8080`.

Make sure you have [Racket 5.1](http://racket-lang.org) or later installed.


Training
--------

The original train data is not included (while the results of training are
available in `data` directory).

First, put plain-text formatted books into `train-data` directory. Edit
`authors.rkt` to add authors and file names (remove the ones you don't
have). Finally, run `racket train.rkt`.
