Icinga2 Check Aggregation Check
===============================

Check the haproxy status URL and alert based on certain conditions.

Use cases:
- alert if fewer than X servers on backend Y are up, for a given frontend
- alert if more than X servers on backend Y are down, for a given frontend


Usage
-----
You must have an erlang 18 executable available in your path. Depending on OS:
```shell
apt-get install erlang
```
```shell
brew install erlang
```

For descriptions of all available options, run `./aggcheck` without any parameters or flags.

Known Limitations
-----------------

Build Instructions
------------------

This project is written in erlang, and is built using the Mix build tool from
the elixir ecosystem. The easiest way to get a build environment is to follow
the install steps from http://elixir-lang.org/install.html.

The build output is an erlang escript file named "aggcheck", which is a single
self-contained file with all dependencies packaged inside, similar to a jar.

To build the escript executable:
```shell
mix deps.get
mix escript.build
```

To build a debian package, a Makefile is provided that uses FPM:
```shell
gem install fpm
Make
```


Contributing
------------

This is my first useful erlang code, so there are probably a lot of
opportunities for improvement. All such improvements / feedback about how I'm
doing it wrong are welcome.
