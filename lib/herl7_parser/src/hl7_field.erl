-module(hl7_field).

-export([serialize/3]).

-include("hl7_structures.hrl").

serialize(Field, FSep, EscChar) when is_record(Field, hl7r_content_field) ->
  hl7_serializer_utils:escape_for_serialization(Field#hl7r_content_field.content, FSep, EscChar).


