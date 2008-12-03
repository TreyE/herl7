-module(hl7_component).

-export([serialize/2]).

-include("hl7_structures.hrl").

serialize(Component, MProps) when is_record(Component, hl7r_content_component) ->
    EscChar = ?E_CHAR(MProps),
    CSep = hl7_message_properties:component_separator(MProps),
    hl7_serializer_utils:escape_for_serialization(Component#hl7r_content_component.content, CSep, EscChar);
serialize(Component, MProps) when is_record(Component, hl7r_subcomponent_component) ->
  SCDelim = hl7_message_properties:subcomponent_separator(MProps),
  SubComps = lists:map(subcomponent_serializer(MProps), Component#hl7r_subcomponent_component.subcomponents),
  hl7_serializer_utils:join_with_delimiter(SubComps, SCDelim).

subcomponent_serializer(MProps) ->
    fun(SC) ->
	    serialize_subcomponent(SC, MProps)
    end.

serialize_subcomponent(SubComponent, MProps) ->
    SCRaw = hl7_subcomponent:serialize(SubComponent, MProps),
    EscChar = ?E_CHAR(MProps),
    CSep = hl7_message_properties:component_separator(MProps),
    hl7_serializer_utils:escape_only(SCRaw, CSep, EscChar).

