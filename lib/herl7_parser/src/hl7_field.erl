-module(hl7_field).

-export([serialize/3]).

-ifdef(testing).
-include("eunit.hrl").
-endif.

-include("hl7_structures.hrl").

serialize(Field, FSep, EscChar) when is_record(Field, hl7r_content_field) ->
  escape_content(de_binarize(Field#hl7r_content_field.content), [], FSep, EscChar).

escape_content([T|Rest], Acc, FSep, EscChar) ->
  case T of
    FSep -> escape_content(Rest, lists:append(Acc, [EscChar, T]), FSep, EscChar);

    EscChar -> escape_content(Rest, lists:append(Acc, [EscChar,T]), FSep, EscChar);
    _ -> escape_content(Rest, lists:append(Acc, [T]), FSep, EscChar)
  end;
escape_content([], Acc, _FSep, _EscChar) -> list_to_binary(Acc).
    
    

de_binarize(Val) when is_binary(Val) -> binary_to_list(Val);
de_binarize(Val) when is_list(Val) -> Val.

-ifdef(testing).
content_escape_test() ->
    OriginalContent = <<"AB\\D|E">>,
    Field = #hl7r_content_field{
      content = OriginalContent
    },
    <<"AB\\\\D\\|E">> = serialize(Field, $|, $\\).
    
-endif.
