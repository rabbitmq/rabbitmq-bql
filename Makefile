SHELL=/bin/bash

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
TARGETS=ebin/leex.beam src/command_lexer.erl src/command_parser.erl ebin/command_lexer.beam ebin/command_parser.beam $(BEAM_TARGETS)

ERLAMQP_BIN=$(CURDIR)/../rabbitmq-erlang-client
RABBIT_BIN=$(CURDIR)/../rabbitmq-server

INCLUDE_OPTS=-I $(INCLUDE_DIR) -I $(ERLAMQP_BIN)/include -I $(RABBIT_BIN)/include

ERLC_OPTS=$(INCLUDE_OPTS) -o $(EBIN_DIR) -Wall -v +debug_info

ERLC=erlc
ERL=erl

ERL_PATH_OPTS=-pa $(EBIN_DIR) -pa $(ERLAMQP_BIN)/ebin -pa $(RABBIT_BIN)/ebin -pa $(IBROWSE_BIN)/ebin
LEXER_NAME=command_lexer
PARSER_NAME=command_parser

all: $(TARGETS)

src/command_lexer.erl: src/command_lexer.xrl
	$(ERL) -I -pa ebin -noshell -eval 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME).xrl",[{outdir,"$(SOURCE_DIR)"}]), halt().'

src/command_parser.erl: src/command_parser.yrl
	$(ERL) -I -pa ebin -noshell -eval 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	$(ERLC) $(ERLC_OPTS) -pa $(EBIN_DIR) $<

test: all
	$(ERL) -I -pa ebin -noshell -eval 'command_parser_test:test(), halt().'

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f erl_crash.dump
	rm -f src/command_lexer.erl
	rm -f src/command_parser.erl
