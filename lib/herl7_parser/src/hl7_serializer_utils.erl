-module(hl7_serializer_utils).

-export([escape_for_serialization/3, join_with_delimiter/2, escape_only/3]).

-ifdef(testing).
-include("eunit.hrl").
-endif.


escape_for_serialization(Content, Delim, EscChar) ->
    escape_content(de_binarize(Content), [], Delim, EscChar).

escape_only(Content, Delim, EscChar) ->
    escape_only(de_binarize(Content), [], Delim, EscChar).

escape_only([T|Rest], Acc, Delim, EscChar) ->
  case T of
    Delim -> escape_only(Rest, lists:append(Acc, [EscChar, T]), Delim, EscChar);
    _ -> escape_only(Rest, lists:append(Acc, [T]), Delim, EscChar)
  end;
escape_only([], Acc, _Delim, _EscChar) -> list_to_binary(Acc).

escape_content([T|Rest], Acc, Delim, EscChar) ->
  case T of
    Delim -> escape_content(Rest, lists:append(Acc, [EscChar, T]), Delim, EscChar);

    EscChar -> escape_content(Rest, lists:append(Acc, [EscChar,T]), Delim, EscChar);
    _ -> escape_content(Rest, lists:append(Acc, [T]), Delim, EscChar)
  end;
escape_content([], Acc, _Delim, _EscChar) -> list_to_binary(Acc).

join_with_delimiter(Items, DChar) ->
  delimiter_join(lists:map(fun de_binarize/1, Items), [], DChar).
     
delimiter_join([T], Acc, _DChar) ->
    list_to_binary(lists:append(Acc, T));
delimiter_join([T|Rest], Acc, DChar) ->    
    delimiter_join(Rest, lists:append(Acc, lists:append(T, [DChar])), DChar).
    

de_binarize(nil) -> [];
de_binarize(Val) when is_binary(Val) -> binary_to_list(Val);
de_binarize(Val) when is_list(Val) -> Val.

-ifdef(testing).
content_escape_test() ->
    OriginalContent = <<"AB\\D|E">>,
    <<"AB\\\\D\\|E">> = escape_for_serialization(OriginalContent, $|, $\\).

delimited_join_test() ->
        OriginalContent = [<<"ABCDE">>, <<"ABCD">>, <<"A&BD^E">>],
        <<"ABCDE|ABCD|A&BD^E">> = join_with_delimiter(OriginalContent, $|).
-endif.
