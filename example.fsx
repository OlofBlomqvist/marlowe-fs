#r "nuget:Wasmtime"
#r "nuget:Newtonsoft.Json"
#load "src/Types.fs"
#load "src/FromJson.fs"
#load "src/ToJson.fs"
#load "src/MarloweRS.fs"
#load "src/Library.fs"

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open marlowe_fs

// deserialize json using the f# impl
let deserialized_from_json_to_fsharp = 
    """
        {
            "token":{
                "token_name":"abc",
                "currency_symbol":"def"
            },
            "to":{
                "account":{
                    "role_token":"role1"
                }
            },
            "then":"close",
            "pay":111111,
            "from_account":{
                "role_token":"role2"
            }
        }
    """ |> deserializeContractFromJson

open Types
let contract = Pay(
    Party.Role("nisse"),
    Payee.Party(Role("Kalle")),
    Token.Custom("symbol","name"), // Token.ADA
    Value.Constant(10),
    Contract.Close
)

// call marlowe-rs to deserialize cbor-hex encoded datum in to json
let datum = marlowe_fs.datum_json_from_cbor_hex "d8799fd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338dffd8799fa1d8799fd8799fd87980d8799fd8799f581ce0dc70fa9698727e439df458436797d057cdfea5e7c2844d12b6af8affd87a80ffffd8799f4040ffff1a002dc6c0a0a000ffd87a9fd8799fd87980d8799fd8799f581ce0dc70fa9698727e439df458436797d057cdfea5e7c2844d12b6af8affd87a80ffffd87a9fd87a9f4f5769746864726177616c5465737431ffffd8799f4040ffd87a9f1a00989680ffd87980ffff"


// serialize f# types using f# json serializer impl
let contract_json = contract |> serializeContractToJson


printfn "Marlowe Core JSON -> F#:\n%A\n" deserialized_from_json_to_fsharp
printfn "F# -> Marlowe Core JSON:\n%s\n" contract_json
printfn "CBOR-HEX -> Marlowe Core JSON:\n%A\n" datum
