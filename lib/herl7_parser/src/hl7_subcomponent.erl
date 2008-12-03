-module(hl7_subcomponent).

-export([serialize/2]).

-include("hl7_structures.hrl").

serialize(SubC, MProps) when is_record(SubC, hl7r_subcomponent) ->
    EscChar = ?E_CHAR(MProps),
    CSep = hl7_message_properties:subcomponent_separator(MProps),
    hl7_serializer_utils:escape_for_serialization(
      SubC#hl7r_subcomponent.content,
      CSep,
      EscChar
    ).
