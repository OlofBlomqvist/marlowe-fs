module Tests

open ToJson
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member x.CanParseBigJson() =
        System.IO.File.ReadAllText "../../../marlowe_samples/big.json"
        |> marlowe_fs.deserializeContractFromJson
        |> ignore
         
    [<TestMethod>]
     member x.CanReSerializeBigJson() = 
        let sample_json = 
            System.IO.File.ReadAllText "../../../marlowe_samples/big.json"
        
        let deserialized = 
            marlowe_fs.deserializeContractFromJson sample_json
         
        deserialized |> toJsonContract |> string |> ignore
