%%%-------------------------------------------------------------------
%%% File    : hl7_message_properties.erl
%%% Author  : Trey <trey@treysoldcomp>
%%% Description : Manages the hl7r_message_properties structure.
%%%
%%% Created :  3 Dec 2008 by Trey <trey@treysoldcomp>
%%%-------------------------------------------------------------------
-module(hl7_message_properties).

%% API
-export([escape_character/1, field_separator/1, component_separator/1, field_repeat_separator/1, subcomponent_separator/1, new/0, new/5]).

-include("hl7_records.hrl").

%%====================================================================
%% API
%%====================================================================

new(FSep, CSep, SCSep, FRSep, EChar) ->
    #hl7r_message_properties{
       field_separator = FSep,
       component_separator = CSep,
       subcomponent_separator = SCSep,
       field_repeat_separator = FRSep,
       escape_character = EChar
    }.


new() -> #hl7r_message_properties{}.

escape_character(MPRec) when is_record(MPRec, hl7r_message_properties) ->
    MPRec#hl7r_message_properties.escape_character.

field_separator(MPRec) when is_record(MPRec, hl7r_message_properties) ->
    MPRec#hl7r_message_properties.field_separator.

field_repeat_separator(MPRec) when is_record(MPRec, hl7r_message_properties) ->
    MPRec#hl7r_message_properties.field_repeat_separator.

component_separator(MPRec) when is_record(MPRec, hl7r_message_properties) ->
    MPRec#hl7r_message_properties.component_separator.

subcomponent_separator(MPRec) when is_record(MPRec, hl7r_message_properties) ->
    MPRec#hl7r_message_properties.subcomponent_separator.

%%====================================================================
%% Internal functions
%%====================================================================
