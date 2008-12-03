%% This is the application resource file (.app file) for the herl7_parser,
%% application.
{application, herl7_parser, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [herl7_parser_app,
              herl7_parser_sup,
              hl7_message_parser,
              hl7_message_tokenizer,
              hl7_message,
              hl7_field,
              hl7_component,
              hl7_subcomponent,
              hl7_serializer_utils,
              hl7_message_properties
   ]},
   {registered,[herl7_parser_sup]},
   {applications, [kernel, stdlib]},
   {mod, {herl7_parser_app,[]}},
   {start_phases, []}]}.

