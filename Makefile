PACKAGE=rabbitmq-bql
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627
GENERATED_SOURCES=command_lexer command_parser
EXTRA_PACKAGE_DIRS=scripts
TEST_APPS=amqp_client rabbitmq_bql
TEST_COMMANDS=command_parser_test:test() bql_test:test() amq_interface_test:test()
START_RABBIT_IN_TESTS=true

include ../include.mk

LEXER_NAME=command_lexer
PARSER_NAME=command_parser

src/command_lexer.erl: ebin/leex.beam src/command_lexer.xrl
	$(ERL) -I -pa ebin -noshell -eval 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME).xrl",[{outdir,"$(SOURCE_DIR)"}]), halt().'

src/command_parser.erl: ebin/leex.beam src/command_parser.yrl
	$(ERL) -I -pa ebin -noshell -eval 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'
