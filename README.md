# scheme-httpd

This is a simple HTTP daemon for scheme. It's pretty
rough-and-ready. I have it running with
[Racket](http://racket-lang.org/) and [Scheme48](http://s48.org/) at
the moment.

## Dependencies

It currently depends on:

 - [SRFI](http://srfi.schemers.org/)s
   [1](http://srfi.schemers.org/srfi-1/),
   [2](http://srfi.schemers.org/srfi-2/),
   [9](http://srfi.schemers.org/srfi-9/),
   [13](http://srfi.schemers.org/srfi-13/),
   [23](http://srfi.schemers.org/srfi-23/),
   [34](http://srfi.schemers.org/srfi-34/) and
   [39](http://srfi.schemers.org/srfi-39/)
 - Byte vector support
 - A means of converting between strings and UTF-8 encoded byte vectors
 - TCP server socket support
 - Some notion of preemptively-scheduled threads
 - A pretty-printer
 - Block I/O of byte vectors
 - and last but not least, my [xxexpr library](http://github.com/tonyg/xxexpr/).

## Running the example

Check the source code out into some directory. Check out
[xxexpr](http://github.com/tonyg/xxexpr/) next to it.

    git clone git://github.com/tonyg/scheme-httpd
    git clone git://github.com/tonyg/xxexpr

If you're using Racket,

    racket ./scheme-httpd/racket/example.rkt

If you're using Scheme48,

    ./scheme-httpd/s48/example.scm

or equivalently

    cd scheme-httpd/s48
    tail -f -n +3 example.scm | scheme48

Once the program is loaded and running, you should be able to see the
example application at http://localhost:8000/.

## Copyright and Licence

Copyright (c) 2008, 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>  
Copyright (c) 2008 LShift Ltd. <query@lshift.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
