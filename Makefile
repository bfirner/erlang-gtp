#Clear suffixes
.SUFFIXES:

#Define erl to beam suffix
.SUFFIXES: .erl .beam

FLAGS=+debug_info -Wall +native

#Use erlc to compile .erl files to .beam files
.erl.beam:
	erlc $(FLAGS) $<

#Build all of the required beam files by default
all: twod_array.beam stones.beam gtp.beam

clean:
	-rm *.beam
	-rm erl_crash.dump

run: twod_array.beam stones.beam gtp.beam
	erl -noshell -run gtp main

