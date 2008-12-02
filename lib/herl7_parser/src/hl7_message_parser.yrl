Nonterminals message segment part_content vals val segments segment_content fields.
Terminals byte field_separator component_separator subcomponent_separator field_repeat_separator segment_terminator.

Rootsymbol message.
message -> segments : {message, '$1'}.

% Segments
segments -> segment : ['$1'].
segments -> segment segments : {segments, lists:append(['$1'], '$2')}.
segment -> segment_content segment_terminator : {segment, '$1'}.
segment_content -> fields : {fields, '$1'}.

% Fields
fields -> part_content field_separator part_content : lists:append([{field, '$1'}], [{field, '$3'}]).
fields -> part_content field_separator fields : lists:append([{field, '$1'}], '$3').


% Vanilla Content
part_content -> vals : '$1'.

vals -> val : ['$1'].
vals -> val vals : lists:append(['$1'], '$2').
val -> byte : byte_value('$1').
val -> component_separator : component_separator.
val -> subcomponent_separator : subcomponent_separator.
val -> field_repeat_separator : field_repeat_separator.
Erlang code.
byte_value(Token) ->
  {byte, element(3, Token)}.
