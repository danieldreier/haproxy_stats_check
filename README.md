HAProxy Server Count Nagios Check
=================================
[![Build Status](https://travis-ci.org/danieldreier/haproxy_stats_check.svg?branch=master)](https://travis-ci.org/danieldreier/haproxy_stats_check)

Check the haproxy status URL and alert if there are too many unhealthy backends.

The idea is to be able to monitor a load balancer and alert before all backends
are down.

Usage
-----

```shell
Usage: /usr/local/bin/haproxy_check [-n [<check_name>]] [-u [<url>]]
                                    [-w [<warn_threshold>]]
                                    [-c [<crit_threshold>]]
                                    [-b <backend>]

  -n, --name            Check name to display in output [default: haproxy
                        backend count]
  -u, --url             URL for haproxy stats CSV [default:
                        http://localhost:7070/haproxy?stats;csv]
  -w, --warn-threshold  warning threshold for matching servers [default: 1]
  -c, --crit-threshold  critical threshold for matching servers [default:
                        2]
  -b, --backend         haproxy backend to count servers for
```

Examples:

For the "forgeapi" haproxy backend, warn if 1 or more servers are in the down
state, and go critical if 2 or more are in the down state.
```shell
./haproxy_check -w 1 -c 2 --backend forgeapi
```

For the "web" haproxy backend, warn if 1 or more servers are in the down
state, and go critical if 2 or more are in the down state.
```shell
./haproxy_check -w 1 -c 2 --backend web
```

For descriptions of all available options, run `./haproxy_check` without any parameters or flags.

Requirements
-----------------
You must have an erlang 18 executable available in your path. Depending on OS:
```shell
apt-get install erlang
```
```shell
brew install erlang
```

Testing Instructions
--------------------

This tool includes some basic eunit test coverage, which you can run with:

```shell
mix eunit
```

To run eunit tests from within the erl shell, you'll need to compile with
the debug flag enabled so the macro preprocessor includes the tests:
```shell
8> c(haproxy_check_cli, {d,'TEST'}).
{ok,haproxy_check_cli}
9> eunit:test(haproxy_check_cli).
  Test passed.
ok
10> c(haproxy, {d,'TEST'}).
{ok,haproxy}
11> eunit:test(haproxy).
  All 4 tests passed.
ok
12>
```

Build Instructions
------------------

This project is written in erlang, and is built using the Mix build tool from
the elixir ecosystem. The easiest way to get a build environment is to follow
the install steps from http://elixir-lang.org/install.html.

The build output is an erlang escript file named "haproxy_check", which is a single
self-contained file with all dependencies packaged inside, similar to a jar.

To build the escript executable:
```shell
mix deps.get
mix escript.build
```

To build a debian package, a Makefile is provided that uses FPM:
```shell
gem install fpm
make
```

You will end up with a .deb file in the current directory, which should
then be put in an apt repository.

To build an RPM package, you must have the rpmbuild tool available. On OS X:
```shell
brew install rpm
gem install fpm
make
```


Contributing
------------

This is my second useful erlang project, so there are probably a lot of
opportunities for improvement. All such improvements / feedback about how I'm
doing it wrong are welcome.
