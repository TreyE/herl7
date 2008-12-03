-module(hl7_component).

-export([serialize/2]).

-include("hl7_structures.hrl").

serialize(Component, MProps) when is_record(Component, hl7r_content_component) ->
    EscChar = ?E_CHAR(MProps),
    CSep = hl7_message_properties:component_separator(MProps),
    hl7_serializer_utils:escape_for_serialization(Component#hl7r_content_component.content, CSep, EscChar).
