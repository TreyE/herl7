-module(hl7_subcomponent).

-export([serialize/3]).

-include("hl7_structures.hrl").

serialize(SubC, CSep, EscChar) when is_record(SubC, hl7r_subcomponent) ->
    hl7_serializer_utils:escape_for_serialization(
      SubC#hl7r_subcomponent.content,
      CSep,
      EscChar
    ).
