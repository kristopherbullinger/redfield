# Redfield Encoding Specification
Like protobuf, redfield messages are encoded as Tag-Length(?)-Value. A 16-bit integer encodes the data type and field number. The least-significant four bits encode the value type, and the most significant bits, after being shifted right four bytes, indicates the field number.

`let field_number = tag >> 4;`

`let wire_type = tag & 0bF;`

|Wire Type|Description|
|---|---------|
|0|bool or u8 or i8|
|1|u16 or i16 or enum|
|2|u32 or i32 or f32|
|3|u64 or i64 or f64|
|4|string or byte array where length <= 0xFF|
|5|string or byte array where 0xFF < length <= 0xFFFF|
|6|string or byte array where 0xFF < length <= 0xFFFFFFFF|
|7|list where number of items <= 0xFF|
|8|list where number of items <= 0xFFFF|
|9|list where number of items <= 0xFFFFFFFF|
|10|message with 0xFF or fewer tag-value pairs|
|11|message with 0xFFFF or fewer tag-value pairs|
|12|oneof|
|13|null|

Therefore, the maximum number of fields in a single message definition is `u16::MAX >> 4 = 4095`.

### Booleans
Booleans represent `true` or `false`.
#### Redfield
Booleans are represented as one-byte value, just like a `u8`. `true` is represented by the value `1` and `false` by the value `0`. A value other than `1` or `0` is considered invalid.
#### JSON
Booleans are encoded as the values `true` and `false`.

### Signed and Unsigned Integer Types
|Bytes|Signed|Unsigned|
|-----|------|--------|
|1    |u8    |i8      |
|2    |u16   |i16     |
|4    |u32   |i32     |
|8    |u64   |i64     |
#### Redfield
Integers are encoded in little-endian form.
#### JSON
8-, 16-, and 32-bit integers are encoded as JSON numbers. 64-bit integers are encoded as strings.

### Floats
f32

f64
#### Redfield
Floats are encoded bitwise in little-endian form.
#### JSON
Floats are encoded as JSON numbers, or as the special values "Infinity", "NaN", or "-Infinity".

### Enums
Enums are C-like, meaning that they are essentially a discrete set of integers.  Each variant must be assigned a value, unless it has the special variant name `UNKNOWN`. If the `UNKNOWN` variant is present, then any value encountered during decoding which does not match any known variant will be preserved as an unknown value, and it is not considered an error. If the `UNKNOWN` variant is not present, it is considered an error to encounter an unknown value. Therefore, it is an API commitment to omit `UNKNOWN`.
```
enum Status {
  Pending = 1,
  Canceled = 99,
  Fulfilled = 100,
  UNKNOWN,
}
```
#### Redfield
Enums are represented on the wire as a `u16`.
#### JSON
Enums are represented as a JSON number.

### Byte Arrays
Byte arrays can be thought of as a special interpretation of `List<u8>`. They can be considered as raw, possibly opaque data blobs.
```
message Avatar {
  username: string,
  png: bytes,
}
```
#### Redfield
Byte arrays are represented as an unsigned integer indicating the length in bytes of the data, followed by the bytes. The length is indicated with a 1-, 2-, or 4-byte integer depending on the wire type for the field. Refer to the table at the top of this document.
#### JSON
Byte arrays are encoded as URL-safe[^1], percent-padded base64 strings.

### Strings
#### Redfield
Strings are represented just like byte arrays, but they must be valid utf8.
#### JSON
Strings are represented as JSON strings.

### Messages
A message is a group of key- or tag-value pairs.
#### Redfield
Messages are encoded with an unsigned integer indicating the number of tag-value pairs in the entire message, followed by a zero or more key-value pairs. The number of tag-value pairs is indicated with a 1- or 2-byte integer depending on the wire type. Refer to the table at the top of this document.
#### JSON
Messages are represented as JSON objects. The message field names as strings are used as the object keys.

### OneOfs
A OneOf is a type which can be exactly one of the types in the definition.
```
// assume existence of other items named Cat, Dog, and Fish...
oneof Pet {
  cat @0: Cat,
  dog @1: Dog,
  fish @2: Fish,
}
oneof Identifier {
  name @0: string,
  badge_number @1: u32,
  passport_id @2: u32,
}
````
#### Redfield 
OneOfs are encoded as a single tag-value pair; unlike messages, the length is implicitly 1 and therefore excluded from the encoded form.
#### JSON
OneOfs are encoded as an object with a single field.

### Lists
A list is a series of zero or more items. A list's definition may include a size ie `List<u8; 3>`, which indicates that a specific number of items will be present. If a size is specified, it is an error during decoding for the list to have fewer or more values.
```
message Pixel {
  position_xyz @0: List<f64; 3>,
  color_rgb @1: List<u8; 3>,
}
```
#### Redfield
Lists are length-prefixed with the number of items in the list. The length is encoded as a 1-, 2-, or 4-byte unsigned integer depending on the field type. Refer to the table at the top of this document.
#### JSON
Lists are encoded as JSON arrays.

### Null
`null` is a special type that represents the absence of a value, just like JSON's `null`. It's not very interesting by itself, but may be used as a member of a `oneof`.
#### Redfield
`null` contains no value; it is comprised only of a tag.
#### JSON
`null` is encoded as JSON `null`.

### Optional Message Fields
An optional message field may be omitted from the encoded form.
```
message PaginatedResult {
  data@0: List<DataItem>,
  next_token@1?: string, // final page reached when next_token is not present
}
```
#### Redfield
If the value is not present, it will not be included in the encoded form. If it is present, it will be encoded as normal.
#### JSON
If the value if not present, the key and value will not be present in the JSON object. If it is, it will appear as normal.

[^1](https://datatracker.ietf.org/doc/html/rfc4648#section-5)[https://datatracker.ietf.org/doc/html/rfc4648#section-5]
