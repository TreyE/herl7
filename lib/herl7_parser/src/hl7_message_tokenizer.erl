-module(hl7_message_tokenizer).

-export([tokenize/1]).

-include("hl7_records.hrl").

-define(FS_TOK, {field_separator, 1}).
-define(CS_TOK, {component_separator, 1}).
-define(FR_TOK, {field_repeat_separator, 1}).
-define(SCS_TOK, {subcomponent_separator, 1}).
-define(ST_TOK, {segment_terminator, 1}).
-define(B_TOK(A), {byte, 1, A}).

tokenize(String) -> 
  {MProps, Tokens, Rest} = parse_msh(String),   
  tokenize_it(Rest, MProps, Tokens).


parse_msh([$M, $S, $H, FieldSep, CompSep, FRepeat, EscChar, SCSep|Rest]) ->
  Tokens = [
    ?B_TOK($M),
    ?B_TOK($S),
    ?B_TOK($H),
    ?FS_TOK,
    ?B_TOK(CompSep),
    ?B_TOK(FRepeat),
    ?B_TOK(EscChar),
    ?B_TOK(SCSep)
  ],
  MProps = #hl7r_message_properties{
    field_separator = FieldSep,
    component_separator = CompSep,
    subcomponent_separator = SCSep,
    escape_character = EscChar,
    field_repeat_separator = FRepeat
  },
  {MProps, Tokens, Rest}.

tokenize_it([$\r], _MsgPropts, Tokens) ->
  lists:append(Tokens, [{segment_terminator, 1}]);
tokenize_it([A, B|Rest], MsgPropts, Tokens) ->
  EChar = hl7_message_properties:escape_character(MsgPropts),
  FChar = hl7_message_properties:field_separator(MsgPropts),
  CChar = hl7_message_properties:component_separator(MsgPropts),
  SCChar = hl7_message_properties:subcomponent_separator(MsgPropts),
  FRChar = hl7_message_properties:field_repeat_separator(MsgPropts),
  case ({A, B}) of
  {EChar,Char} -> tokenize_it(Rest, MsgPropts, lists:append(Tokens, [?B_TOK(Char)]));
  {$\r,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [?ST_TOK]));
  {FChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [?FS_TOK]));
  {CChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [?CS_TOK]));
  {SCChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [?SCS_TOK]));
  {FRChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [?FR_TOK]));
  {CharA,CharB} -> tokenize_it([CharB|Rest], MsgPropts, lists:append(Tokens, [?B_TOK(CharA)]))
  end.
