# Redfield File
### EBNF
This file documents the structure of a redfield schema file using EBNF syntax. 
|Syntax Item|Meaning|
|-----------|-------|
|`X \| Y`          | either X or Y|
|`(X Y)`         |"X then Y" counts as a single item|
|`[ X ]`         |option: X zero or one time|
|`{ X }`         |repetition: X zero or more times|

```
redfield_schema = [ "base_url" "=" HTTP_URL ";" ] { top_level_definition }
top_level_definition = message | service | enum | oneof
base_type = "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "f32" | "f64" | "bool" | "string" | "bytes" | "void"

ascii_alphanumeric = ascii_digit | ascii_alpha_upper | ascii_alpha_lower
ascii_alpha_lower = "a"..."z"
ascii_alpha_upper = "A"..."Z"
ascii_digit = "0"..."9"

one_or_more_digits = ascii_digit { ascii_digit }
decimal_number = one_or_more_digits
hex_digit = ascii_digit | "A"..."F" | "a"..."f"
hex_number = "0x" hex_digit { hex_digit }
binary_digit = "0" | "1"
binary_number = "0b" binary_digit { binary_digit }

ident = ascii_character { ascii_character | ascii_digit | "_" } 
type = ( list type ) | ( base_type | ident )
list = "[" [ decimal_number ] "]"

message = "message" ident message_body
message_body = "{" { enum | oneof | message | message_field } "}"
message_field = ident "@" field_number ( "?" ) ":" type ","
field_number = "0" | ("1"..."9" { ascii_digit })

enum = "enum" ident enum_body
enum_body = "{" { ident "=" enum_variant_value ","} [ "UNKNOWN" "," ] "}"
enum_variant_value = decimal_number | hex_number | binary_number

oneof = "oneof" ident oneof_body
oneof_body = "{" { enum | oneof | message | oneof_field } "}"
oneof_field = ident "@" field_number ":" type ","

service = "service" ident "{" { procedure } "}"
procedure = verb ident "(" [ ident ] ")" [ "->" ident ] ";"
verb = "GET" | "POST"
```
