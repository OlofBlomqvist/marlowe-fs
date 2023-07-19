module FromJson

open Newtonsoft.Json.Linq
open Newtonsoft.Json
open Types

module helpers =

    let hasProperty (jobject:JObject) (propertyName : string) : bool =
        jobject.Properties() |> Seq.exists (fun prop -> prop.Name = propertyName)

    let getProperty (jobject:JObject) (propertyName : string) : JToken =
        jobject.Properties()
        |> Seq.find (fun prop -> prop.Name = propertyName)
        |> (fun prop -> prop.Value)

    let hasAllProperties (jobject:JObject) (propertyNames : string list) =
        propertyNames |> Seq.forall (hasProperty jobject)

    let rec fromJsonToken (json : JToken) : Token =
        match json with
        | :? JObject as jobject ->
            let currencySymbol = jobject.Item("currency_symbol") |> fromJsonString
            let tokenName = jobject.Item("token_name") |> fromJsonString
            { CurrencySymbol = currencySymbol; TokenName = tokenName }
        | _ -> failwith "Unsupported JSON value for Token"

    and fromJsonString (json : JToken) : string =
        match json with
        | :? JValue as jvalue ->
            match jvalue.Value with
            | :? string as strValue -> strValue
            | _ -> failwith "Unsupported JSON value type for string"
        | _ -> failwith "Unsupported JSON value for string"

open helpers

let rec fromJsonParty (json : JToken) : Party =
    match json with
    | :? JValue as jvalue ->
        match jvalue.Value with
        | :? string as strValue -> Role strValue
        | x -> failwith <| sprintf "Unsupported JSON value type for Party: %A" x
    | :? JObject as jo ->
        if jo.ContainsKey "role_token" then 
            let v = jo.Item "role_token"
            Role(v.ToString())
        elif jo.ContainsKey "address" then 
            let v = jo.Item "address"
            Address(v.ToString())
        else
            failwith <| sprintf "Unsupported JSON value type for Party: %A" jo
    | x -> 
        failwith <| sprintf "Unsupported JSON value for Party: %A --- %A" x (x.ToString())

// TODO : make sure we have all values types
and fromJsonValue (json : JToken) : Value =
    match json with
    | :? JObject as jobject ->

        if hasProperty jobject "amount_of_token" then
            let token = getProperty jobject "amount_of_token" |> fromJsonToken
            let party = getProperty jobject "in_account" |> fromJsonParty
            AvailableMoney(party, token)
        elif hasProperty jobject "constant" then
            let x = getProperty jobject "constant" |> fromJsonInt64
            Constant x
        elif hasProperty jobject "constant_param" then
            let x = getProperty jobject "constant_param" |> fromJsonString
            ConstantParam x
        elif hasProperty jobject "negate" then
            let v = getProperty jobject "negate" |> fromJsonValue
            NegValue v
        elif hasAllProperties jobject ["add";"and"] then
            let lhs = getProperty jobject "add" |> fromJsonValue
            let rhs = getProperty jobject "and" |> fromJsonValue
            AddValue(lhs, rhs)
        elif hasAllProperties jobject ["value";"minus"] then
            let lhs = getProperty jobject "value" |> fromJsonValue
            let rhs = getProperty jobject "minus" |> fromJsonValue
            SubValue(lhs, rhs)
        elif hasAllProperties jobject ["multiply";"times"] then
            let lhs = getProperty jobject "multiply" |> fromJsonValue
            let rhs = getProperty jobject "times" |> fromJsonValue
            MulValue(lhs, rhs)
        elif hasProperty jobject "divide" && hasProperty jobject "by" then
            let lhs = getProperty jobject "divide" |> fromJsonValue
            let rhs = getProperty jobject "by" |> fromJsonValue
            DivValue(lhs, rhs)
        elif hasProperty jobject "value_of_choice" then
            let choiceId = getProperty jobject "value_of_choice" |> fromJsonChoiceId
            ChoiceValue choiceId
        elif hasProperty jobject "time_interval_start" then
            TimeIntervalStart
        elif hasProperty jobject "time_interval_end" then
            TimeIntervalEnd
        elif hasProperty jobject "use_value" then
            let valueId = getProperty jobject "use_value" |> fromJsonString
            UseValue valueId
        elif hasAllProperties jobject ["if";"then";"else"] then
            let obs = getProperty jobject "if" |> fromJsonObservation
            let tv = getProperty jobject "then" |> fromJsonValue
            let ev = getProperty jobject "else" |> fromJsonValue
            Cond(obs, tv, ev)
        else
            failwith "Invalid JSON object for Value"
    | :? JValue as jvalue ->
        match jvalue.Value with
        | :? int64 as intValue -> Constant intValue
        | :? string as stringValue -> ConstantParam stringValue
        | x -> failwith  <| sprintf "Unsupported JSON value type for Value: %A" (x)
    | _ -> failwith "Unsupported JSON value for Value"

and fromJsonContract (json : JToken) : Contract =
    match json with
    | :? JValue as jvalue ->
    
        match jvalue.Value with
        | :? string as strValue ->
            match strValue with
            | "close" -> Close
            | _ -> failwithf "Unsupported contract type: %s" strValue
        | _ -> failwith "Unsupported JSON value type for Contract"
    | :? JObject as jobject ->

        if hasAllProperties jobject ["from_account";"to";"token";"pay"] then
            let party = getProperty jobject "from_account" |> fromJsonParty
            let payee = getProperty jobject "to" |> fromJsonPayee
            let token = getProperty jobject "token" |> fromJsonToken
            let value = getProperty jobject "pay" |> fromJsonValue
            let thenContract = getProperty jobject "then" |> fromJsonContract
            Pay(party, payee, token, value, thenContract)

        elif hasAllProperties jobject ["if";"then";"else"] then
            let obs = getProperty jobject "if" |> fromJsonObservation
            let thenContract = getProperty jobject "then" |> fromJsonContract
            let elseContract = getProperty jobject "else" |> fromJsonContract
            If(obs, thenContract, elseContract)

        elif hasAllProperties jobject ["when";"timeout";"timeout_continuation"] then
            let casesJson = getProperty jobject "when" |> fromJsonArray
            let cases = casesJson |> Seq.toList |> List.map (fun caseJson -> fromJsonCase caseJson)
            let timeout = getProperty jobject "timeout" |> fromJsonTimeout
            let timeoutContinuation = getProperty jobject "timeout_continuation" |> fromJsonContract
            When(cases, timeout, timeoutContinuation)

        elif hasAllProperties jobject ["let";"be";"then"] then
            let valueId = getProperty jobject "let" |> fromJsonString
            let value = getProperty jobject "be" |> fromJsonValue
            let thenContract = getProperty jobject "then" |> fromJsonContract
            Let(valueId, value, thenContract)

        elif hasAllProperties jobject ["assert";"then"] then
            let obs = getProperty jobject "assert" |> fromJsonObservation
            let thenContract = getProperty jobject "then" |> fromJsonContract
            Assert(obs, thenContract)

        else
            failwith "Invalid JSON object for Contract"
    | _ -> failwith "Unsupported JSON value for Contract"

and fromJsonPayee (json : JToken) : Payee = // (Party (Role "role")) vs (Account (Role "role"))
    match json with
    | :? JObject as jobject ->

        if jobject.ContainsKey "party" then
            let p = jobject.Item "party" |> fromJsonParty
            Payee.Party(p)
        elif 
            jobject.ContainsKey "account" then
            let p = jobject.Item "account" |> fromJsonParty
            Payee.Account(p)
        else
            failwith <| sprintf "Invalid JSON object for payee: %A" (jobject.ToString())

        
    | x ->  failwith <| sprintf "Unsupported JSON value for Payee : %A"  (x.ToString())

and fromJsonCase (json : JToken) : Case =
    match json with
    | :? JObject as jobject ->

        if hasAllProperties jobject ["case";"then"] then
            let action = getProperty jobject "case" |> fromJsonAction
            let thenContract = getProperty  jobject "then" |> fromJsonContract
            Case(action, thenContract)
        else
            failwith "Invalid JSON object for Case"
    | _ -> failwith "Unsupported JSON value for Case"


and fromJsonAction (json : JToken) : Action =
    match json with
    | :? JObject as jobject ->

        if hasAllProperties jobject ["into_account";"party" ;"of_token" ;"deposits" ] then
            
            let party = 
                getProperty jobject "party" 
                |> fromJsonParty
            
            let into_account  =
                getProperty jobject "into_account" 
                |> fromJsonParty
            
            let of_token = 
                getProperty jobject "of_token" 
                |> fromJsonToken
            
            let deposits = 
                getProperty jobject "deposits" 
                |> fromJsonValue
            
            Deposit(
                party, 
                into_account, 
                of_token, 
                deposits
            )

        elif hasAllProperties jobject ["for_choice";"choose_between"] then
            let choiceId = getProperty jobject "for_choice" |> fromJsonChoiceId
            let bounds = getProperty jobject "choose_between" |> fromJsonBounds
            Choice(choiceId, bounds)
        elif hasProperty jobject "notify_if" then
            let obs = getProperty jobject "notify_if" |> fromJsonObservation
            Notify(obs)
        else
            failwith "Invalid JSON object for Action"
    | _ -> failwith "Unsupported JSON value for Action"

and fromJsonChoiceId (json : JToken) : ChoiceId =
    match json with
    | :? JObject as jobject ->

        if hasAllProperties jobject ["choice_name";"choice_owner"] then
            let choiceName = getProperty jobject "choice_name" |> fromJsonString
            let choiceOwner = getProperty jobject "choice_owner" |> fromJsonParty
            { choice_name = choiceName; choice_owner = choiceOwner }
        else
            failwith "Invalid JSON object for ChoiceId"
    | _ -> failwith <| sprintf "Unsupported JSON value for ChoiceId: %A" (json.ToString())

and fromJsonBounds(json : JToken) : Bound list = 
    match json with
    | :? JArray as jarray ->
        jarray |> Seq.map (fun item -> 
            item |> fromJsonBound
        ) |> List.ofSeq
    | _ -> failwith <| sprintf "Unsupported JSON value for Bounds: %A" json

and fromJsonBound (json : JToken) : Bound =
    match json with
    | :? JObject as jobject ->
        if hasAllProperties jobject ["from";"to"] then
            let from = getProperty jobject "from" |> fromJsonInt64
            let to_ = getProperty jobject "to" |> fromJsonInt64
            {from = from ; _to = to_}
        else
            failwith "Invalid JSON object for Bound"
    | _ -> failwith <| sprintf "Unsupported JSON value for Bound: %A" (json.ToString())


// TODO: handle all observations
and fromJsonObservation (json : JToken) : Observation =
    match json with
    | :? JObject as jobject ->

        if hasAllProperties jobject ["and" ;"both"] then
            let lhs = getProperty jobject "both" |> fromJsonObservation
            let rhs = getProperty jobject "and" |> fromJsonObservation
            AndObs(lhs, rhs)
        elif hasAllProperties jobject ["either" ;"or"] then
            let lhs = getProperty jobject "either" |> fromJsonObservation
            let rhs = getProperty jobject "or" |> fromJsonObservation
            OrObs(lhs, rhs)
        elif hasProperty jobject "not" then
            let v = getProperty jobject "not" |> fromJsonObservation
            NotObs v
        elif hasProperty jobject "chose_something_for" then
            let choiceId = getProperty jobject "chose_something_for" |> fromJsonString
            ChoseSomething choiceId
        elif hasAllProperties jobject ["value" ;"lt"] then
            let lhs = getProperty jobject "value" |> fromJsonValue
            let rhs = getProperty jobject "lt" |> fromJsonValue
            ValueLT(lhs, rhs)
        elif hasAllProperties jobject ["value" ;"gt"] then
            let lhs = getProperty jobject "value" |> fromJsonValue
            let rhs = getProperty jobject "gt" |> fromJsonValue
            ValueGT(lhs, rhs)
        elif hasAllProperties jobject ["value" ;"ge"] then
            let lhs = getProperty jobject "value" |> fromJsonValue
            let rhs = getProperty jobject "ge" |> fromJsonValue
            ValueGE(lhs, rhs)
        else
            sprintf "Invalid JSON object for Observation: %A" (jobject.ToString()) |> failwith
    | _ -> failwith <| sprintf "Unsupported JSON value for Observation: %A" (json.ToString())

and fromJsonTimeout (json : JToken) : Timeout =
    match json with
    | :? JValue as jv -> 
        jv |> fromJsonInt64 |> fun n -> POSIXTime n
    | :? JObject as jobject ->
        if hasProperty jobject "time_param" then
            let timeParam = getProperty jobject "time_param" |> fromJsonString
            TimeParam timeParam
        elif hasProperty jobject "posix_time" then
            let posixTime = getProperty jobject "posix_time" |> fromJsonInt64
            POSIXTime posixTime
        else
            failwith "Invalid JSON object for Timeout"
    | _ -> failwith <| sprintf "Unsupported JSON value for Timeout: %A" (json.ToString())

and fromJsonInt64 (json : JToken) : int64 =
    match json with
    | :? JValue as jvalue ->
        match jvalue.Value with
        | :? int64 as int64Value -> int64Value
        | _ -> failwith "Unsupported JSON value type for int64"
    | _ -> failwith <| sprintf "Unsupported JSON value for int64: %A" (json.ToString())

and fromJsonArray (json : JToken) : JToken seq =
    match json with
    | :? JArray as jarray -> jarray.Children()
    | _ -> failwith "Unsupported JSON value for array"

let fromJson (jsonString : string) : Module =
    let json = JsonConvert.DeserializeObject(jsonString) :?> JObject
    let metadata = json.Item("metadata") |> fromJsonString
    let contract = json.Item("contract") |> fromJsonContract
    { Metadata = metadata; Contract = contract }


