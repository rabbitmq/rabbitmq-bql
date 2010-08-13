PACKAGE=rabbitmq-bql
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627
CLIENT_DEPS=rabbit_common amqp_client
GENERATED_SOURCES=command_lexer command_parser
SCRIPTS_DIR=scripts
EXTRA_PACKAGE_DIRS=$(SCRIPTS_DIR)
TEST_APPS=amqp_client rabbitmq_bql
TEST_COMMANDS=command_parser_test:test() bql_test:test() amq_interface_test:test() bql_client_test:test()
START_RABBIT_IN_TESTS=true
CLIENT_PACKAGE=rabbitmq-bql-client.zip
EXTRA_PACKAGE_ARTIFACTS=$(CLIENT_PACKAGE)

include ../include.mk

LEXER_NAME=command_lexer
PARSER_NAME=command_parser

src/command_lexer.erl: ebin/leex.beam src/command_lexer.xrl
	$(ERL) -I -pa ebin -noshell -eval 'ok = leex:file("$(SOURCE_DIR)/$(LEXER_NAME).xrl",[{outdir,"$(SOURCE_DIR)"}]), halt().'

src/command_parser.erl: ebin/leex.beam src/command_parser.yrl
	$(ERL) -I -pa ebin -noshell -eval '{ok, _} = yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'

CLIENT_PACKAGE_DIR=build/client
$(DIST_DIR)/$(CLIENT_PACKAGE): $(TARGETS) $(wildcard $(SCRIPTS_DIR))
	rm -rf $(CLIENT_PACKAGE_DIR)
	mkdir -p $(DIST_DIR)
	mkdir -p $(CLIENT_PACKAGE_DIR)/ebin
	cp $(EBIN_DIR)/* $(foreach DEP_NAME, $(CLIENT_DEPS), $(PRIV_DEPS_DIR)/$(DEP_NAME)*/ebin/*) $(CLIENT_PACKAGE_DIR)/ebin
	cp $(SCRIPTS_DIR)/* $(CLIENT_PACKAGE_DIR)
	
	(cd $(CLIENT_PACKAGE_DIR); zip -r ../../$@ *)
	
run_client: $(DIST_DIR)/$(CLIENT_PACKAGE)
	(cd $(CLIENT_PACKAGE_DIR); ./bql $(CLIENT_ARGS))
	
run_dump: $(DIST_DIR)/$(CLIENT_PACKAGE)
	(cd $(CLIENT_PACKAGE_DIR); ./bql_dump)
