-module(hl7_message_parser).

-export([parse/1]).

-include("hl7_records.hrl").

parse(String) -> tokenize_it(String, parse_msh(String), []).


parse_msh([$M, $S, $H, FieldSep, CompSep, FRepeat, EscChar, SCSep|_Rest]) ->
  #hl7_message_properties{
    field_separator = FieldSep,
    component_separator = CompSep,
    subcomponent_separator = SCSep,
    escape_character = EscChar,
    field_repeat_separator = FRepeat
  }.

tokenize_it([$\r], _MsgPropts, Tokens) ->
  lists:append(Tokens, [{segment_terminator}]);
tokenize_it([A, B|Rest], MsgPropts, Tokens) ->
  EChar = MsgPropts#hl7_message_properties.escape_character,
  FChar = MsgPropts#hl7_message_properties.field_separator,
  CChar = MsgPropts#hl7_message_properties.component_separator,
  SCChar = MsgPropts#hl7_message_properties.subcomponent_separator,
  FRChar = MsgPropts#hl7_message_properties.field_repeat_separator,
  case ({A, B}) of
  {EChar,Char} -> tokenize_it(Rest, MsgPropts, lists:append(Tokens, [{byte, Char}]));
  {FChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [{field_separator}]));
  {CChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [{component_separator}]));
  {SCChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [{subcomponent_separator}]));
  {FRChar,Char} -> tokenize_it([Char|Rest], MsgPropts, lists:append(Tokens, [{field_repeat_separator}]));
  {CharA,CharB} -> tokenize_it([CharB|Rest], MsgPropts, lists:append(Tokens, [{byte, CharA}]))
  end.
