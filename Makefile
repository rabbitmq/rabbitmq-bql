SOURCE_ERLS:=$$(filter-out %/leex.erl,$$(wildcard $$($(PACKAGE_DIR)_SOURCE_DIR)/*.erl))
APP_NAME:=rabbitmq_bql
DEPS:=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627-wrapper
CLIENT_DEPS:=rabbit_common amqp_client
GENERATED_ERLS:=$$($(PACKAGE_DIR)_SOURCE_DIR)/command_lexer.erl $$($(PACKAGE_DIR)_SOURCE_DIR)/command_parser.erl
SCRIPTS_DIR:=$(PACKAGE_DIR)/scripts
EXTRA_PACKAGE_DIRS:=$(SCRIPTS_DIR)
TEST_COMMANDS:=command_parser_test:test() bql_test:test() amq_interface_test:test() bql_client_test:test()
CLIENT_PACKAGE:=$(PACKAGE_DIR)/$(DIST_DIR)/rabbitmq-bql-client
EXTRA_TARGETS:=$(CLIENT_PACKAGE).zip

LEXER_NAME:=command_lexer
PARSER_NAME:=command_parser

%/src/command_lexer.erl: %/ebin/leex.beam %/src/command_lexer.xrl
	$(ERL) -I -pa ebin -noshell -eval 'ok = leex:file("$(@D)/$(LEXER_NAME).xrl",[{outdir,"$(@D)/"}]), halt().'

%/src/command_parser.erl: %/ebin/leex.beam %/src/command_parser.yrl
	$(ERL) -I -pa ebin -noshell -eval '{ok, _} = yecc:file("$(@D)/$(PARSER_NAME)"), halt().'

$(CLIENT_PACKAGE).zip_INPUTS:=$(PACKAGE_DIR)/ebin/* $(foreach DEP_NAME,$(CLIENT_DEPS),$(PACKAGE_DIR)/$(DIST_DIR)/$(DEP_NAME)*/ebin/*)
$(CLIENT_PACKAGE).zip_DIR:=$(CLIENT_PACKAGE)
$(CLIENT_PACKAGE).zip: $(wildcard $(SCRIPTS_DIR)) | $(PACKAGE_DIR)/$(DIST_DIR)
	rm -rf $($@_DIR)
	mkdir -p $($@_DIR)/ebin
	cp $($@_INPUTS) $($@_DIR)/ebin
	cp $(SCRIPTS_DIR)/* $($@_DIR)
	(cd $(@D); zip -r $@ $(notdir $($@_DIR)))

run_client: $(DIST_DIR)/$(CLIENT_PACKAGE)
	(cd $(CLIENT_PACKAGE_DIR); ./bql $(CLIENT_ARGS))

run_dump: $(DIST_DIR)/$(CLIENT_PACKAGE)
	(cd $(CLIENT_PACKAGE_DIR); ./bql_dump)

include ../include.mk
