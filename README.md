# Zaisu

Zaisu is an in-memory CouchDB mock replication endpoint intended for testing
purposes.

It is built with [Cowboy](https://github.com/ninenines/cowboy), an HTTP server
written in Erlang.

## Running

To generate the files needed to build a release, run

    $ make bootstrap-rel

From now on, you can simply generate a new release and start the
application with

    $ make run

Tests can be run with

    $ make tests


tbc.
