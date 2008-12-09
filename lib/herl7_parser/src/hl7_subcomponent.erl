-module(hl7_subcomponent).

-export([serialize/2, new_content/1, new_empty/0]).

-include("hl7_structures.hrl").

new_empty() ->
    new_content(nil).

new_content(Lst) ->
    #hl7r_subcomponent{
      content = Lst
    }.


serialize(SubC, MProps) when is_record(SubC, hl7r_subcomponent) ->
    EscChar = ?E_CHAR(MProps),
    CSep = hl7_message_properties:subcomponent_separator(MProps),
    hl7_serializer_utils:escape_for_serialization(
      SubC#hl7r_subcomponent.content,
      CSep,
      EscChar
    ).
