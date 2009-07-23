PACKAGE=mod_bql
DEPS=rabbitmq-server rabbitmq-erlang-client
GENERATED_SOURCES=command_lexer command_parser

include ../include.mk

LEXER_NAME=command_lexer
PARSER_NAME=command_parser

src/command_lexer.erl: ebin/leex.beam src/command_lexer.xrl
	$(ERL) -I -pa ebin -noshell -eval 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME).xrl",[{outdir,"$(SOURCE_DIR)"}]), halt().'

src/command_parser.erl: ebin/leex.beam src/command_parser.yrl
	$(ERL) -I -pa ebin -noshell -eval 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'

test: all
	$(ERL) -I -pa ebin -noshell -eval 'command_parser_test:test(), halt().'
