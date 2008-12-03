-module(hl7_field).

-export([serialize/2]).

-include("hl7_structures.hrl").

serialize(Field, MProps) when is_record(Field, hl7r_content_field) ->
  FSep = hl7_message_properties:field_separator(MProps),
  EscChar = ?E_CHAR(MProps),
  hl7_serializer_utils:escape_for_serialization(Field#hl7r_content_field.content, FSep, EscChar);
serialize(Field, MProps) when is_record(Field, hl7r_component_field) ->
  CDelim = hl7_message_properties:component_separator(MProps),
  Comps = lists:map(component_serializer(MProps), Field#hl7r_component_field.components),
  hl7_serializer_utils:join_with_delimiter(Comps, CDelim).

component_serializer(MProps) ->
    fun(SC) ->
	    serialize_component(SC, MProps)
    end.

serialize_component(Component, MProps) ->
    CRaw = hl7_component:serialize(Component, MProps),
    EscChar = ?E_CHAR(MProps),
    FSep = hl7_message_properties:field_separator(MProps),
    hl7_serializer_utils:escape_only(CRaw, FSep, EscChar).


