-module(hl7_message).

-export([parse_message/1, segments/1, serialize/2, new/1]).

-include("hl7_structures.hrl").

-ifdef(testing).
-include("eunit.hrl").
-endif.

new(Lst) ->
    #hl7r_message{
      segments = Lst
    }.

parse_message(String) ->
    hl7_message_parser:parse(hl7_message_tokenizer:tokenize(String)).

segments(Message) when is_record(Message, hl7r_message) ->
    Segs = Message#hl7r_message.segments,
    Segs.

serialize(Message, MessageProperties) when is_record(Message, hl7r_message) ->
    SegSerializer = fun(X, Acc) ->
      lists:append(
        Acc,
        binary_to_list(hl7_segment:serialize(X, MessageProperties))
      )
    end,
    list_to_binary(lists:foldl(SegSerializer, [], Message#hl7r_message.segments)).

-ifdef(testing).
-define(EX_MSG, "MSH|^~\\&|AccMgr|1|||20050110045504||ADT^A01|599102|P|2.3|||\rEVN|A01|20050110045502|||||\r").

parse_example_message() ->
    Msg = ?EX_MSG,
    {ok, ParsedMessage} = parse_message(Msg),
    ParsedMessage.

segment_field(S, F, Msg) ->
      Segment = lists:nth(S, segments(Msg)),
      lists:nth(F, Segment#hl7r_segment.fields).

segment_field_component(S, F, Comp, Msg) ->
      Segment = lists:nth(S, segments(Msg)),
      Field = lists:nth(F, Segment#hl7r_segment.fields),
      lists:nth(Comp, Field#hl7r_component_field.components).

message_record_test() ->
    ParsedMessage = parse_example_message(),
    true = is_record(ParsedMessage, hl7r_message).

message_segments_test() ->
        ParsedMessage = parse_example_message(),
        ExpectedSegments = ParsedMessage#hl7r_message.segments,
        ExpectedSegments = segments(ParsedMessage).

message_header_tag_test() ->
        ParsedMessage = parse_example_message(),
        FirstField = segment_field(1, 1, ParsedMessage),
        <<"MSH">> = FirstField#hl7r_content_field.content.

message_component_test() ->
        ParsedMessage = parse_example_message(),
        ExpectedComponent = segment_field_component(1, 9, 2, ParsedMessage),
        <<"A01">> = ExpectedComponent#hl7r_content_component.content.

serialize_test() ->
        ParsedMessage = parse_example_message(),
        SerializeOutput = binary_to_list(serialize(ParsedMessage, hl7_message_properties:new())),
        SerializeOutput = ?EX_MSG.

create_message_test() ->
  ExpectedValue = "MSH|^~\\&|AccMgr|1|||20050110045504||ADT^A01|599102|P|2.3|||\r",
    MessageProps = hl7_message_properties:new(),
    Fields = [
      hl7_field:new_content(<<"MSH">>),
      hl7_field:new_content(<<"^~\\&">>),
      hl7_field:new_content(<<"AccMgr">>),
      hl7_field:new_content(<<"1">>),
      hl7_field:new_empty(),
      hl7_field:new_empty(),
      hl7_field:new_content(<<"20050110045504">>),
      hl7_field:new_empty(),
      hl7_field:new_components(
       [
         hl7_component:new_content(<<"ADT">>),
         hl7_component:new_content(<<"A01">>)
      ]),
      hl7_field:new_content(<<"599102">>),
      hl7_field:new_content(<<"P">>),
      hl7_field:new_content(<<"2.3">>),
      hl7_field:new_empty(),
      hl7_field:new_empty(),
      hl7_field:new_empty()
    ],
    ParsedMessage = hl7_message:new([ hl7_segment:new(<<"MSH">>, Fields) ] ),
    SMsg = binary_to_list(serialize(ParsedMessage, MessageProps)),
    ExpectedValue = SMsg.

-endif.
