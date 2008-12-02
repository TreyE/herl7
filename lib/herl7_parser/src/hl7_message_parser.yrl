Nonterminals message segment part_content vals val segments segment_content fields field components component subcomponents.
Terminals byte field_separator component_separator subcomponent_separator field_repeat_separator segment_terminator.

Rootsymbol message.
message -> segments : {message, '$1'}.

% Segments
segments -> segment : ['$1'].
segments -> segment segments : {segments, lists:append(['$1'], '$2')}.
segment -> segment_content segment_terminator : {segment, '$1'}.
segment_content -> fields : {fields, '$1'}.

% Fields
fields -> field field_separator field : lists:append([{field, '$1'}], [{field, '$3'}]).
fields -> field field_separator fields : lists:append([{field, '$1'}], '$3').

field -> part_content : '$1'.
field -> components : '$1'.


% Components
components -> component component_separator component : lists:append([{component, '$1'}], [{field, '$3'}]).
components -> component component_separator components : lists:append([{component, '$1'}], '$3').

component -> part_content : '$1'.
component -> subcomponents : '$1'.

% Subcomponents
subcomponents -> part_content subcomponent_separator part_content : lists:append([{subcomponent, '$1'}], [{field, '$3'}]).
subcomponents -> part_content subcomponent_separator subcomponents : lists:append([{subcomponent, '$1'}], '$3').

% Vanilla Content
part_content -> '$empty' : nil.
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
