val connect : string -> DBCaml.Driver.t

val deserialize : 'a Serde.De.t -> bytes -> ('a, Serde.error) result
