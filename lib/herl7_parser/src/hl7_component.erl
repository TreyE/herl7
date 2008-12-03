-module(hl7_component).

-export([serialize/3]).

-include("hl7_structures.hrl").

serialize(Component, CSep, EscChar) when is_record(Component, hl7r_content_component) ->
    hl7_serializer_utils:escape_for_serialization(Component#hl7r_content_component.content, CSep, EscChar).
