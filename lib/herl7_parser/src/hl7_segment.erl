-module(hl7_segment).

-export([serialize/2, new/2, field_value/2]).

-include("hl7_structures.hrl").

-define(S_DELIMITER, $\r).

new(SID, Lst) ->
    #hl7r_segment{
      segment_id = SID,
      fields = Lst
    }.

field_value(N, Segment) when is_record(Segment, hl7r_segment) ->
  Fields = Segment#hl7r_segment.fields,
  case (N > length(Fields)) of
    false -> hl7_field:value(lists:nth(N, Fields));
    true -> nil
  end.

serialize(Segment, MProps) when is_record(Segment, hl7r_segment) ->
     FDelim = hl7_message_properties:field_separator(MProps),
     Fields = lists:map(field_serializer(MProps), Segment#hl7r_segment.fields),
     finish_segment(
       Segment#hl7r_segment.segment_id,
       hl7_serializer_utils:join_with_delimiter(Fields, FDelim)
     ).

finish_segment(<<"MSH">>, SegBin) ->
  Msg = lists:append(binary_to_list(SegBin), [?S_DELIMITER]),
  {Hd, Tail} = lists:split(6, Msg),
  list_to_binary(lists:append(Hd, tl(Tail)));
finish_segment(_, SegBin) ->
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
