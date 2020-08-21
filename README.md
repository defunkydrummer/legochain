# legochain

## What is this?

This simple program implements a working blockchain class (*based on block mining with a difficulty level*) plus a very simple "peer to peer" protocol that enables two or more servers to talk to each other and send/receive their blockchains, based on TCP sockets.  

This is a simple project with the following goals, either:

### Goal A

To show newcomers to Common Lisp, an example of how to implement something not so simple but not too complex, like a blockchain, that would involve the use of:

- Cryptography, using [ironclad](https://github.com/sharplispers/ironclad)
- Regular expressions, using [cl-ppcre](https://edicl.github.io/cl-ppcre/)
- Sockets, using [usocket](https://github.com/usocket/usocket)
- Objects with [the Common Lisp Object System](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html)

The code is written with as many comments as possible, and is as clear as I can manage to. The aim was not performance, nor production-quality, but educational value.

You can start by taking a look at [legochain.lisp](legochain.lisp) and then to [peertopeer.lisp](peertopeer.lisp)

### Goal B

- To show seasoned CL developers who still haven't yet found out what is a blockchain, block mining, and the "nonce" values, a simple answer by looking at the code.

## How to install (for newcomers to CL)

- Install [Portacle](https://portacle.github.io/), the portable Common Lisp environent.
- Clone this project into the *projects* subdirectory under the portacle root directory, so you have it into `/portacle/projects/legochain`.
- Start portacle
- Write `(ql:quickload "legochain")` to load the program.
- All the functions are in package "legochain".
- Try `(legochain::servers-test 9000 9001)` to make two blockchain servers talk to each other (a simple demo.)

## Author

Flavio Egoavil aka D e f u n k y d r u m m e r

License MIT

# Hacking

I'm always open to pull requests that can make the code more readable or more educational!
