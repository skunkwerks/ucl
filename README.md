## UCL: BEAM-friendly bindings for `libUCL`

[LibUCL] is a human-friendly, automation-oriented parser and generator
for `Universal Configuration Language` or `UCL`. UCL's main benefits
are:

- human readable and editable with comments
- machine parsable
- conversion to/from YAML, JSON
- macro and include-file support
- lightning-fast parser

`UCL` is a NIF-based binding to the reference implementation [LibUCL],
and currently requires that library to be pre-installed on your system.

Version 1.0 onwards uses dirty schedulers, but does not yield nor
time-slice NIF calls, as this would make the code significantly more
complex for a function that is unlikely to be run frequently within the
BEAM.

UCL comes with friendly functions for *both* Elixir and Erlang, we are
all friends here.

## Installation & Compilation

### Dependencies

`libucl` is obviously a dependency, and the usual UNIX build chain is
required.

- FreeBSD:  `textproc/libucl devel/rebar3`
- OSX:      `brew install libucl`
- Debian:   `build-essential` from base
            `erlang-dev erlang-nox` from erlang-solutions
            `rebar3` from https://rebar3.org/
- OpenBSD:  `erlang-21 elixir` from base
            `rebar3` from https://rebar3.org/

On some systems, LibUCL is missing and you'll need to build it.
On Debian, there is a conflicting compression library of the same name.

## Building LibUCL from source

### OpenBSD

```
$ doas pkg_add pkgconf autogen autoconf-archive autoconf-2.69 \
    automake-1.11 libtool
$ tar xzf libucl-*.gz
$ cd libucl*
$ ./autogen.sh
$ ./configure
$ make
$ doas make install
```

### Debian

```
$ sudo apt install autoconf libcurl4-openssl-dev automake libtool \
    autoconf-archive pkg-config
$ tar xzf libucl-*.gz
$ cd libucl*
$ ./autogen.sh
$ ./configure --prefix=/usr
$ make
$ sudo make install
```

### Installation

Via git clone:

```
$ export PATH=$PATH:/usr/local/lib/erlang25/bin:$HOME/.mix
$ mix local.rebar
$ rebar3 do clean, compile, ct, ex_doc
```

Via [hex] in the usual fashion:

- Elixir: add `{:ucl, "~> 1.0"}` to `mix.exs`
- Erlang: add `{ucl, "1.0.0"}` to `rebar.config`

Compilation should work on all UNIX-like OS out of the box. There is
o Windows support planned, but if you can compile `libucl` on Windows,
get in touch.

Tests are available as usual via `rebar3 ct`, and long-running property
tests via `rebar3 proper`. The property tests are effectively fuzzing
the NIF by injecting random `binary()` data and expecting it to return
a classic `{ok | error, any()}` tuple, and take a considerable amount of
time (several hours):

```
$ time rebar3 proper
... 3 hours later ...
00% {60000,66000}
10% {54000,60000}
09% {48000,54000}
10% {42000,48000}
10% {36000,42000}
09% {30000,36000}
09% {24000,30000}
09% {18000,24000}
09% {12000,18000}
10% {6000,12000}
9% {0,6000}
===> 1/1 properties passed
OK: Passed 500000 test(s).
11005.99 real     11039.08 user        86.36 sys
```

Although `UCL` is written in Erlang, and uses `rebar3` ,it should
compile cleanly as a dependency on any BEAM language.

## Usage

```elixir
iex> UCL.to_json(":foo")
     {:error, :ucl_invalid}
iex> "foo: true" |> UCL.to_json! |> Jason.decode!
     %{"foo" => true}
```

```erlang
1> ucl:to_json(<<"foo: true">>).
    {ok,<<"{\n    \"foo\": true\n}">>}
```

The implementation is intended for functional transformation of config
data held in memory, and thus a large portion of the LibUCL API has been
summarily ignored. If you need something please let us know.

## Universal Configuration Language Syntax

UCL is heavily infused by `nginx` configuration as the example of a
convenient configuration system. However, UCL is fully compatible with
`JSON` format and is able to parse json files. For example, you can
write the same configuration in the following ways:

* in nginx like:

```nginx
param = value;
section {
    param = value;
    param1 = value1;
    flag = true;
    number = 10k;
    time = 0.2s;
    string = "something";
    subsection {
        host = {
            host = "hostname";
            port = 900;
        }
        host = {
            host = "hostname";
            port = 901;
        }
    }
}
```

* or in JSON:

```json
{
    "param": "value",
    "param1": "value1",
    "flag": true,
    "subsection": {
        "host": [
        {
            "host": "hostname",
            "port": 900
        },
        {
            "host": "hostname",
            "port": 901
        }
        ]
    }
}
```

## Improvements to the json notation.

There are various things that make ucl configuration more convenient for
editing than strict json:

### General syntax sugar

* Braces are not necessary to enclose a top object: it is automatically
  treated as an object:

```json
"key": "value"
```
is equal to:
```json
{"key": "value"}
```

* There is no requirement of quotes for strings and keys, moreover, `:`
  may be replaced `=` or even be skipped for objects:

```nginx
key = value;
section {
    key = value;
}
```
is equal to:
```json
{
    "key": "value",
    "section": {
        "key": "value"
    }
}
```

* No commas mess: you can safely place a comma or semicolon for the last
  element in an array or an object:

```json
{
    "key1": "value",
    "key2": "value",
}
```
### Automatic arrays creation

* Non-unique keys in an object are allowed and are automatically
  converted to the arrays internally:

```json
{
    "key": "value1",
    "key": "value2"
}
```
is converted to:
```json
{
    "key": ["value1", "value2"]
}
```

### Named keys hierarchy

UCL accepts named keys and organize them into objects hierarchy
internally. Here is an example of this process:

```nginx
section "blah" {
	key = value;
}
section foo {
	key = value;
}
```

is converted to the following object:

```nginx
section {
	blah {
		key = value;
	}
	foo {
		key = value;
	}
}
```

Plain definitions may be more complex and contain more than a single
level of nested objects:

```nginx
section "blah" "foo" {
	key = value;
}
```

is presented as:

```nginx
section {
	blah {
		foo {
			key = value;
		}
	}
}
```

### Convenient numbers and booleans

* Numbers can have suffixes to specify standard multipliers:
    + `[kKmMgG]` - standard 10 base multipliers (so `1k` is translated to 1000)
    + `[kKmMgG]b` - 2 power multipliers (so `1kb` is translated to 1024)
    + `[s|min|d|w|y]` - time multipliers, all time values are translated to float number of seconds, for example `10min` is translated to 600.0 and `10ms` is translated to 0.01
* Hexadecimal integers can be used by `0x` prefix, for example `key = 0xff`. However, floating point values can use decimal base only.
* Booleans can be specified as `true` or `yes` or `on` and `false` or `no` or `off`.
* It is still possible to treat numbers and booleans as strings by enclosing them in double quotes.

## General improvements

### Comments

UCL supports different style of comments:

* single line: `#`
* multiline: `/* ... */`

Multiline comments may be nested:
```c
# Sample single line comment
/*
 some comment
 /* nested comment */
 end of comment
*/
```

## Thanks

- Paul Davis, whose well documented [NIF examples] made writing this almost cake

[NIF examples]: https://github.com/davisp/nif-examples
[hex]: https://hex.pm/packages/ucl
[LibUCL]: https://github.com/vstakhov/libucl
