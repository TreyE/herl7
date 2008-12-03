-module(hl7_segment).

-export([serialize/2]).

-include("hl7_structures.hrl").

-define(S_DELIMITER, $\r).

serialize(Segment, MProps) when is_record(Segment, hl7r_segment) ->
     FDelim = hl7_message_properties:field_separator(MProps),
     Fields = lists:map(field_serializer(MProps), Segment#hl7r_segment.fields),
     add_segment_delimiter(hl7_serializer_utils:join_with_delimiter(Fields, FDelim)).

add_segment_delimiter(SegBin) ->
    list_to_binary(lists:append(binary_to_list(SegBin), [?S_DELIMITER])).

field_serializer(MProps) ->
    fun(Field) ->
	    serialize_field(Field, MProps)
    end.

serialize_field(Field, MProps) ->
    FRaw = hl7_field:serialize(Field, MProps),
    EscChar = ?E_CHAR(MProps),
    SDel = ?S_DELIMITER,
    hl7_serializer_utils:escape_only(FRaw, SDel, EscChar).
