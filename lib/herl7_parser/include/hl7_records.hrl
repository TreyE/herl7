-record(hl7_message_properties, {
  field_separator,
  component_separator,
  subcomponent_separator,
  field_repeat_separator,
  escape_character
}).

-record(hl7_message, {
  segments = []
}).

-record(hl7_segment, {
  fields = [],
  raw_tokens = []
}).

-record(hl7_field, {
  value = [],
  components = []
}).

-record(hl7_component, {
  value = [],
  subcomponents = []
}).
