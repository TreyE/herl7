-module(hl7_segment_msh).

-export([process_segment/2]).

-ifdef(testing).
-include("eunit.hrl").
-endif.

-define(MSH_SEG_PROPLIST, [
  {encoding_characters, 2},
  {sending_application, 3},
  {sending_facility, 4},
  {receiving_application, 5},
  {receiving_facility, 6},
  {dt_of_message, 7},
  {security, 8},
  {msg_type, 9},
  {msg_control_id, 10},
  {processing_id, 11},
  {version_id, 12},
  {sequence_number, 13},
  {continuation_pointer, 14},
  {accept_acknowledgement_type, 15},
  {application_acknowledgement_type, 16},
  {country_code, 17},
  {char_set, 18},
  {msg_language, 17}
]).

process_segment(RawSeg, MsgProps) ->
  GVFun = get_field_value(RawSeg),
  MSHSeg = lists:map(GVFun, ?MSH_SEG_PROPLIST),
  lists:append(
    lists:append([{field_separator, hl7_message_properties:field_separator(MsgProps)}], MSHSeg),
    [{raw_segment, RawSeg}]
  ).

get_field_value(RawSeg) ->
    fun(X) ->
      {Name, Idx} = X,
      {Name, hl7_segment:field_value(Idx, RawSeg)}
    end.

-ifdef(testing).
create_test_segment() ->
    MessageProps = hl7_message_properties:new(),
    Fields = [
      hl7_field:new_content(<<"MSH">>),
      hl7_field:new_content(<<"^~\\&">>),
      hl7_field:new_content(<<"AccMgr">>),
      hl7_field:new_content(<<"1">>),
      hl7_field:new_empty(),
      hl7_field:new_empty(),
      hl7_field:new_content(<<"20050110045504">>),
      hl7_field:new_empty(),
      hl7_field:new_components(
       [
         hl7_component:new_content(<<"ADT">>),
         hl7_component:new_content(<<"A01">>)
      ]),
      hl7_field:new_content(<<"599102">>),
      hl7_field:new_content(<<"P">>),
      hl7_field:new_content(<<"2.3">>),
      hl7_field:new_empty(),
      hl7_field:new_empty(),
      hl7_field:new_empty()
    ],
    {MessageProps, hl7_segment:new(<<"MSH">>, Fields)}.

create_processed_msh_seg() ->
    {MProp, Seg} = create_test_segment(),
    process_segment(Seg, MProp).

-define(TEST_VAL(TName, Field, Expected), TName() -> PSeg = create_processed_msh_seg(), Expected = proplists:get_value(Field, PSeg)).

-define(TEST_MSH_VAL(TestName, Expected), TestName() ->
  FName = list_to_atom(lists:sublist(atom_to_list(TestName), length(atom_to_list(TestName)) - 5)),
  PSeg = create_processed_msh_seg(),
  Expected = proplists:get_value(FName, PSeg)
).


field_sep_test() ->
    PSeg = create_processed_msh_seg(),
    $| = proplists:get_value(field_separator, PSeg).

enc_chars_test() ->
    PSeg = create_processed_msh_seg(),
    <<"^~\\&">> = proplists:get_value(encoding_characters, PSeg).

?TEST_MSH_VAL(sending_application_test, <<"AccMgr">>).

?TEST_MSH_VAL(sending_facility_test, <<"1">>).

?TEST_MSH_VAL(receiving_application_test, nil).

?TEST_MSH_VAL(receiving_facility_test, nil).

?TEST_MSH_VAL(dt_of_message_test, <<"20050110045504">>).

?TEST_MSH_VAL(security_test, nil).

msg_type_test() ->
    PSeg = create_processed_msh_seg(),
    Cs = [hl7_component:new_content(<<"ADT">>), hl7_component:new_content(<<"A01">>)],
    Cs = proplists:get_value(msg_type, PSeg).

?TEST_MSH_VAL(msg_control_id_test, <<"599102">>).

?TEST_MSH_VAL(processing_id_test, <<"P">>).

?TEST_MSH_VAL(version_id_test, <<"2.3">>).

?TEST_MSH_VAL(sequence_number_test, nil).

?TEST_MSH_VAL(continuation_pointer_test, nil).

?TEST_MSH_VAL(accept_acknowledgement_type_test, nil).

?TEST_MSH_VAL(application_acknowledgement_type_test, nil).

?TEST_MSH_VAL(country_code_test, nil).

?TEST_MSH_VAL(char_set_test, nil).

?TEST_MSH_VAL(msg_language_test, nil).

-endif.
