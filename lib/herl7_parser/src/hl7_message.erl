-module(hl7_message).

-export([parse_message/1, segments/1]).

-include("hl7_structures.hrl").

-ifdef(testing).
-include("eunit.hrl").
-endif.

parse_message(String) ->
    hl7_message_parser:parse(hl7_message_tokenizer:tokenize(String)).

segments(Message) ->
    Segs = Message#hl7r_message.segments,
    {Segs, length(Segs)}.

-ifdef(testing).
-define(EX_MSG, "MSH|^~\\&|AccMgr|1|||20050110045504||ADT^A01|599102|P|2.3|||\rEVN|A01|20050110045502|||||\r").

parse_example_message() ->
    Msg = ?EX_MSG,
    {ok, ParsedMessage} = parse_message(Msg),
    ParsedMessage.    

message_record_test() ->
    ParsedMessage = parse_example_message(),
    true = is_record(ParsedMessage, hl7r_message).

message_segments_test() ->
        ParsedMessage = parse_example_message(),
        ExpectedSegments = ParsedMessage#hl7r_message.segments,
        {ExpectedSegments, 2} = segments(ParsedMessage).
-endif.
