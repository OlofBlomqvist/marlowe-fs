module MarloweRS
open Wasmtime    

module wasi_internal =
    // Note: we must make a thread-safe version of this.
    // Note: we must make a good api for the most common features we want to expose to dotnet.
    let engcfg = new Config()
    let engine = new Engine(engcfg)
    let modulex = Module.FromFile(engine,"marlowe_lang_cli.wasm")
    let linker = new Linker(engine)
    let store = new Store(engine)
    let cfg = 
        WasiConfiguration()
            .WithArg("wasi_snapshot_preview1")
            .WithInheritedStandardOutput()
            .WithInheritedStandardError()
    store.SetWasiConfiguration(cfg)
    linker.DefineWasi()
    let instance = linker.Instantiate(store, modulex);
    let mem = instance.GetMemory("memory")
    let memstart = 10;
    let validate_dsl = instance.GetFunction<int32,int32,int32>("validate_dsl");
    let main = instance.GetAction<int32,int32,int32>("main")
    let parse_contract_dsl_to_json = instance.GetFunction<int32,int32,int32>("parse_contract_dsl_to_json")
    let parse_contract_dsl_to_cborhex = instance.GetFunction<int32,int32,int32>("parse_contract_dsl_to_cborhex")
    let parse_contract_json_to_cborhex = instance.GetFunction<int32,int32,int32>("parse_contract_json_to_cborhex")
    let parse_redeemer_from_cbor_to_json = instance.GetFunction<int32,int32,int32>("parse_redeemer_from_cbor_to_json")
    let parse_datum_from_cbor_to_json = instance.GetFunction<int32,int32,int32>("parse_datum_from_cbor_to_json")
    let parse_contract_json_to_dsl = instance.GetFunction<int32,int32,int32>("parse_contract_json_to_dsl")

    if isNull validate_dsl then failwith "failed to locate validate_dsl in wasm file."
    if isNull parse_contract_dsl_to_json then failwith "failed to locate parse_contract_dsl_to_json in wasm file."
    if isNull parse_contract_dsl_to_cborhex then failwith "failed to locate parse_contract_dsl_to_cborhex in wasm file."
    if isNull parse_contract_json_to_cborhex then failwith "failed to locate parse_contract_json_to_cborhex in wasm file."
    if isNull parse_redeemer_from_cbor_to_json then failwith "failed to locate parse_redeemer_from_cbor_to_json in wasm file."
    if isNull parse_datum_from_cbor_to_json then failwith "failed to locate parse_datum_from_cbor_to_json in wasm file."
    if isNull parse_contract_json_to_dsl then failwith "failed to locate parse_contract_json_to_dsl in wasm file."

    let invoke (meth:System.Func<int32,int32,int32>) (arg:string) = 
        let lenOfInpString = mem.WriteString(memstart,arg)
        let address_where_we_can_find_the_resulting_string = meth.Invoke(10,lenOfInpString)
        mem.ReadNullTerminatedString(address_where_we_can_find_the_resulting_string)



let dsl_to_json = wasi_internal.invoke wasi_internal.parse_contract_dsl_to_json


let json_to_dsl = wasi_internal.invoke wasi_internal.parse_contract_json_to_dsl

let datum_from_cbor_hex_to_json = wasi_internal.invoke wasi_internal.parse_datum_from_cbor_to_json



