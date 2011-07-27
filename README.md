I Write Like
============

Website: <http://iwl.me>
Wikipedia: <http://en.wikipedia.org/wiki/I_Write_Like>

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

History
-------

The original version was written in July 2010 in Python using web.py
framework.  Then I rewrote it in Racket in November 2010. This is the latest
Racket version.
