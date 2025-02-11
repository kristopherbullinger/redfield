# Redfield: Schema-First HTTP APIs

```
// optionally specify where the service is hosted
base_url = "https://api.myweatherwebsite.com/api/v1";

// represents all the endpoints of our weather service
service Weather {
  // sends an HTTP GET request for current conditions
  GET current_weather(GetCurrentWeather) -> PointConditions;
  // sends a HTTP POST request for a user's preferences
  POST submit_user_preferences(UserPreferences);
}

message GetCurrentWeather {
  // nested definitions are possible in messages and oneofs
  oneof Location {
    latlong@0: [2]f64,
    zipcode@1: string,
    city_name@2: string,
  }
  location@0: Location,
}

enum PrecipType {
  Rain = 0,
  Snow = 1,
  // special enum variant that enables forward compatibility
  UNKNOWN,
}

// this enum does not contain the special UNKNOWN variant and is thus closed for modification
enum Units {
  // temperature C, precip mm
  Metric = 0,
  // temperature F, precip inch
  Imperial = 1,
}

message PointConditions {
  precip_type@0?: PrecipType,
  units@1: Units,  
  temperature@2: i16,
  is_night@3: bool,
  uv_index@4: i16,
}

message UserPreferences {
  userid@0: string,
  preferred_units@1: Units,
  home_location@2?: [2]f64,
}
```

## Motivation

Redfield aims to streamline the documentation, scaffolding, and consumption of simple request-response services.

Redfield is an interface definition language (IDL) and code generation tool that aims to make it simple to provide structured access to and documentation for HTTP APIs, with specific considerations for including browser-based javascript clients. It aims to replace manually-constructed documentation and to be simpler and more concise than existing choices, supporting the most common use-cases.

Redfield's schema language should be easy to read and write and should cover the majority of use cases in the most straightfoward way possible. The code generator should generate functions that are idiomatic and easy to read. The generated data structures should be without any extra cruft -- only what the schema specifies, without any protocol- or format-specific bookkeping metadata required.

A common method of exposing a service's data and functionality to consumers is via HTTP, where each discrete unit of a service is associated with a specific URL path and HTTP method. To communicate the available services and their paths, methods, the structure of the input and output data (if any) is left to documentation and experimentation. Because there is no widely-accepted standardized form of documenting these kinds of services, it can be a chore for clients to include support for the services, and the quality and availability of documentation is very inconsistent. Furthermore, ensuring compatibility and correctness of the documentation with the actual service is time consuming and error-prone -- if an update is made to a service but it is not reflected in documentation, it can lead to confusion and frustration by consumers.

## Prior Art

### Protobufs and gRPC
Protobufs and protoc are perhaps the most widely-used and supported schema and code generation tools available. The IDL is easy to read and the wire format is simple to understand. Almost every major programming language is supported. However, it has undergone major revisions and changes are still occurring all the time. Therefore, writing a new tool that supports protobufs can be a big challenge. Some syntax is only supported in some versions of protobufs. The interpretation and generated code for a given data structure can differ between versions. There exist some deprecated features such as groups. There is room for adding custom metadata to data and service definitions, which can introduce complexity that isn't needed in most cases.

The most commonly associated remote procedure protocol, gRPC, includes many features that are specific to high-performance service-to-service communication and thus relies on support for HTTP/2, which is not widely supported by browser-based javascript. There exist proxy services that can proxy HTTP/1.1 requests into gRPC requests, but this requires additional infrastructure, code tools, and external libraries. Further, gRPC and protobufs include support for half- and full-duplex streaming, which is not a part of redfield's scope.

Due to the high complexity of the IDL and RPC, many tools only support a subset of features, meaning it can be difficult to be confident that all clients, services, and middlemen comply sufficiently.

### Twirp
Twirp is very close to redfield's ideal. It has a very simple RPC protocol, has a limited feature set that supports most use cases, and eliminates much of the complexity of gRPC. It relies only on features available in HTTP/1.1, which makes it easy to support browser clients. It makes the pragmatic choice of using protobufs as the IDL, automatically gaining the benefits of existing protobuf-related tools. Twirp also supports both JSON and protobuf wire format for any service and endpoint, which makes debugging much simpler. However, twirp's protocol uses a POST request for every HTTP call, which simplifies the protocol but can make caching responses more challenging; by default, POST requests will go to origin every time. Further, while piggybacking on protobufs is a good choice for many reasons, redfield prefers to use a simpler IDL to go along with a simpler protocol.

### ConnectRPC
ConnectRPC and the associated `buf` tools are probably the best tools available for schema-first services right now. It is a gRPC-compatible protocol which degrades gracefully according to the capabilities of the client. By setting a specific option in a service endpoint, you can enable HTTP GET requests, which enables caching of the response. Services currently using gRPC can interoperate with connect services, and simpler clients such as web browsers are supported easily. However, if you don't need gRPC compatibility, then the inclusion of gRPC compatibility only brings a large and complicated dependency into your project. Furthermore, because it is a combination of a new protocol and a very complicated one, language support is increasing slowly.

### OpenAPI (FKA Swagger)
OpenAPI is a common json-based HTTP API description format. Because the schema language is "just json", it automatically gains basic support from just about every programming language and every developer tool. However, the schema language specification is very large, and because it is not its own language but rather more like a format built on top of json, it can be difficult to know what the expected shape of a bit of schema is supposed to be. Full support from developer tools is more challenging: references to other user-defined types are called out as a json $ref which can be difficult to follow, types are specified as strings adjacent to the rest of the data definition, and the OpenAPI format itself is described in a lengthy document rather than in a structured grammar format. Potentially due to the complexity and lower-quality tooling support, the quality and availabilty of OpenAPI-based code generator tools is lesser.

Describing a service endpoint includes specifying the shape of each response per status code and content type. These are details better abstracted away. For the most part, users only care about the input and output data, and whether or not some kind of error occurred. Specifying this extra information in the schema document, especially one as verbose as json, can take a lot of extra time compared to more concise IDLs such as redfield.
 
