Nonterminals message segment part_content vals val segments segment_content fields field components component subcomponents.
Terminals byte field_separator component_separator subcomponent_separator field_repeat_separator segment_terminator.

Rootsymbol message.
message -> segments : create_message('$1').

% Segments
segments -> segment : ['$1'].
segments -> segment segments : lists:append(['$1'], '$2').
segment -> segment_content segment_terminator : '$1'.
segment_content -> fields : create_segment('$1').

% Fields
fields -> field : ['$1'].
fields -> field field_separator fields : lists:append(['$1'], '$3').

field -> part_content : create_content_field('$1').
field -> components : create_component_field('$1').


% Components
components -> component component_separator component : ['$1', '$3'].
components -> component component_separator components : lists:append(['$1'], '$3').

component -> part_content : create_content_component('$1').
component -> subcomponents : create_subcomponent_component('$1').

% Subcomponents
subcomponents -> part_content subcomponent_separator part_content : create_subcomponents('$1', '$3').
subcomponents -> part_content subcomponent_separator subcomponents : append_subcomponents('$1', '$3').

% Vanilla Content
part_content -> '$empty' : nil.
part_content -> vals : create_part_content('$1').

vals -> val : ['$1'].
vals -> val vals : lists:append(['$1'], '$2').
val -> byte : byte_value('$1').

Erlang code.
-include("hl7_structures.hrl").

byte_value(Token) ->
  {byte, element(3, Token)}.

create_subcomponents(A, B) ->
  [
  #hl7r_subcomponent{
    content = A
  },
  #hl7r_subcomponent{
    content = B
  }
  ].

append_subcomponents(A, B) ->
  [#hl7r_subcomponent{
    content = A
  }, B].

create_content_component(A) ->
  #hl7r_content_component{
    content = A
  }.

create_subcomponent_component(A) ->
  #hl7r_subcomponent_component{
    subcomponents = A
  }.

create_content_field(A) ->
  #hl7r_content_field{
    content = A
  }.

create_component_field(A) ->
  #hl7r_component_field{
    components = A
  }.

create_segment(A) ->
  SIDField = lists:nth(1, A),
  #hl7r_segment{
    segment_id = SIDField#hl7r_content_field.content,
    fields = A
  }.

create_message(A) ->
  #hl7r_message{
    segments = A
  }.

create_part_content(A) ->
  MFun = fun(X) ->
    element(2, X)
  end,
  list_to_binary(lists:map(MFun, A)).
