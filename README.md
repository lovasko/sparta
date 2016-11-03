# Sparta
`Text.Sparta` is a Haskell module that implements a sparse table that
provides a reverse approach to querying: the actual matching patterns are
stored within the database and the input for the search functionality is a
plain text.


## Components
The project consists of three components: a `Text.Sparta` module that
contains the core functionality, a `sparta-cli` command-line interface
client that is able to load data from a CSV file and finally a robust UNIX
server application that can be queried via TCP sockets.


## Author
Daniel Lovasko <daniel.lovasko@gmail.com>

