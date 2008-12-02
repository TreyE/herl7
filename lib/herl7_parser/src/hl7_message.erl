-module(hl7_message).

-export([parse_message/1]).

parse_message(String) ->
    hl7_message_parser:parse(hl7_message_tokenizer:tokenize(String)).
