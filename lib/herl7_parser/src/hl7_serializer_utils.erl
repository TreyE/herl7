-module(hl7_serializer_utils).

-export([escape_for_serialization/3]).

-ifdef(testing).
-include("eunit.hrl").
-endif.


escape_for_serialization(Content, Delim, EscChar) ->
    escape_content(de_binarize(Content), [], Delim, EscChar).

escape_content([T|Rest], Acc, Delim, EscChar) ->
  case T of
    Delim -> escape_content(Rest, lists:append(Acc, [EscChar, T]), Delim, EscChar);

    EscChar -> escape_content(Rest, lists:append(Acc, [EscChar,T]), Delim, EscChar);
    _ -> escape_content(Rest, lists:append(Acc, [T]), Delim, EscChar)
  end;
escape_content([], Acc, _Delim, _EscChar) -> list_to_binary(Acc).
    
    

de_binarize(Val) when is_binary(Val) -> binary_to_list(Val);
de_binarize(Val) when is_list(Val) -> Val.

-ifdef(testing).
content_escape_test() ->
    OriginalContent = <<"AB\\D|E">>,
    <<"AB\\\\D\\|E">> = escape_for_serialization(OriginalContent, $|, $\\).
-endif.
