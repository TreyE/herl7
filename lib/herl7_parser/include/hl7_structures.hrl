-record(hl7r_subcomponent, {
          content = []
         }).

-record(hl7r_subcomponent_component, {
          subcomponents = []
         }).

-record(hl7r_content_component, {
          content = []
         }).

-record(hl7r_content_field, {
          content = []
         }).

-record(hl7r_component_field, {
          components = []
         }).

-record(hl7r_segment, {
          segment_id = "",
          fields = []
         }).

-record(hl7r_message, {
          segments = []
         }).

-define(E_CHAR(A), hl7_message_properties:escape_character(A)).
