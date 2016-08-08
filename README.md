# Valgrind with tracegen

This repository contains a new Valgrind tool called "tracegen."
tracegen generates full data and instruction traces, that is a trace of all 
memory reads or writes and all executed instructions, in the CTF format.

Note: This tool is an EXPERIMENTAL PROTOTYPE used for research purposes.
It's neither fast nor safe to use. You have been warned!

## Compiling
See the original Valgrind [README](README) for full instructions.

~~~
./autogen.sh
# adjust the prefix to your preferred location
./configure --prefix=$PWD/inst
make -j8
make install
~~~

## Running
To generate a data and program trace from `YOUR_APPLICATION`, run

~~~
mkdir cts-out # you need to create the output directory first
inst/bin/valgrind --tool=tracegen --trace-mem=yes --output-dir=cts-out YOUR_APPLICATION
~~~

## Viewing the trace
You can use any CTS-enabled trace viewer, the most basic one is babeltrace.

It's packaged on Ubuntu, install with

~~~
sudo apt-get install babeltrace
~~~

To get a text log of all traced events, run babeltrace with the directory
containing the stream and metadata files as parameter.

~~~
babeltrace cts-out
~~~

You can also use the 
[Babeltrace Python bindings](http://diamon.org/babeltrace/docs/python/)
to process the trace further in Python 3.


## Contact
Author: Philipp Wagner <philipp.wagner@tum.de>

