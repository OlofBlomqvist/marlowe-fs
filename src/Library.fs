module marlowe_fs
open FromJson
open Newtonsoft.Json.Linq

// marlowe-fs

let public deserializeContractFromJson = JObject.Parse >> fromJsonContract
let public serializeContractToDsl = ToJson.toJsonContract >> string >> MarloweRS.json_to_dsl
let public serializeContractToJson = ToJson.toJsonContract >> string


// marlowe-rs
let public dsl_to_json = MarloweRS.dsl_to_json
let public json_to_dsl = MarloweRS.json_to_dsl
let public datum_json_from_cbor_hex = MarloweRS.datum_from_cbor_hex_to_json

// combo
let public contract_dsl_to_fsharp = dsl_to_json >> deserializeContractFromJson
// todo: support dsl with variables

