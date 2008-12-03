-module(hl7_field).

-export([serialize/2]).

-include("hl7_structures.hrl").

serialize(Field, MProps) when is_record(Field, hl7r_content_field) ->
  FSep = hl7_message_properties:field_separator(MProps),
  EscChar = ?E_CHAR(MProps),
  hl7_serializer_utils:escape_for_serialization(Field#hl7r_content_field.content, FSep, EscChar).


