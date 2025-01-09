#  Encoding Specification
Like protobuf, redfield messages are encoded as Tag-Length(?)-Value. A 16-bit integer encodes the data type and field number. The least-significant three bits encode the value type, and the most significant bits, after being shifted right three bytes, indicates the field number.

`let field_number = tag >> 3;`

`let data_type = tag & 0b111;`

|Tag|Wire Type|
|---|---------|
|0|bool|
|1|u8 or i8|
|2|u16 or i16 or enum|
|3|u32 or i32 or f32|
|4|u64 or i64 or f64|
|5|string, byte array, message, or list|
|6|oneof|
|7|null|

Therefore, the maximum number of fields in a single message definition is `u16::MAX >> 3 = 8191`.

### Booleans
Booleans represent `true` or `false`.
#### Redfield
Booleans are represented as one-byte value, just like a `u8`. `true` is represented by the value `1` and `false` by the value `0`. A value other than `1` or `0` is not considered compliant.
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
Eight-, sixteen-, and 32-bit integers are encoded as JSON numbers. 64-bit integers are encoded as strings.

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
Byte arrays can be thought of as a special interpretation of `[]u8`. They can be considered as raw, possibly opaque data blobs.
```
message Avatar {
  username: string,
  png: bytes,
}
```
#### Redfield
Byte arrays are represented as a `u32` indicating the length in bytes of the data, followed by the bytes.
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
Messages are encoded with a u32 indicating the length of the entire message, followed by a zero or more key-value pairs.
#### JSON
Messages are represented as JSON objects. The message field names as strings are used as the object keys.

### OneOfs
A OneOf is a type which can be exactly one of the types in the definition. It is encoded like a message with one field. It is considered an error if zero or more than one tag-value pairs are present during decoding.
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
OneOfs are encoded just like messages.
#### JSON
OneOfs are encoded just like messages.

### Lists
A list is a series of zero or more items. A list's definition may include a size ie `[3]u8`, which indicates that a specific number of items will be present. If a size is specified, it is an error during decoding for the list to have fewer or more values.
```
message Pixel {
  position_xyz @0: [3]f64,
  color_rgb @1: [3]u8,
}
```
#### Redfield
Lists are length-prefixed with their length in bytes, NOT the number of items present. A `[]u32` with 10 items will be prefixed with length 40: 10 (number of items) * 4 (byte width of each item). The encoded form is not affected by the presence of a size in the definition.
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
  data@0: []DataItem,
  next_token@1?: string, // final page reached when next_token is not present
}
```
#### Redfield
If the value is not present, it will not be included in the encoded form. If it is present, it will be encoded as normal.
#### JSON
If the value if not present, the key and value will not be present in the JSON object. If it is, it will appear as normal.

[^1](https://datatracker.ietf.org/doc/html/rfc4648#section-5)[https://datatracker.ietf.org/doc/html/rfc4648#section-5]
