all: 
	@erl -make
	@escript release/build_rel.escript boot botnet `pwd`/ebin

clean:
	rm -f ebin/*.beam