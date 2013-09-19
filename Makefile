compile:
	@./rebar compile

clean:
	@./rebar clean
run:
	erl -pa ebin

.PHONY: compile clean deps
