REBAR := $(shell which rebar3 2>/dev/null || echo ./rebar3)
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: clean compile tests

all: compile

compile: $(REBAR)
	$(REBAR) compile

xref: $(REBAR)
	$(REBAR) as test xref

test: $(REBAR)
	$(REBAR) as test eunit
	$(REBAR) as test proper

dialyzer: $(REBAR)
	$(REBAR) as test dialyzer

edoc:
	mkdir -p doc && cp -fR doc_src/* doc
	$(REBAR) edoc
	
edoc_private:
	mkdir -p doc && cp -fR doc_src/* doc
	$(REBAR) as edoc_private edoc

clean: $(REBAR)
	$(REBAR) clean

./rebar3:
	erl -noshell -s inets start -s ssl start \
		-eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar3
