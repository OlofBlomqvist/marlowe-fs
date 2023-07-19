module ToJson

open Newtonsoft.Json.Linq
open Types


let rec toJsonValue (value : Value) : JToken =
    match value with
    | AvailableMoney (party, token) ->
        let json = JObject()
        json.Add("amount_of_token", toJsonToken token)
        json.Add("in_account", toJsonParty party |> JValue)
        json :> JToken
    | Constant x ->
        JValue x
    | ConstantParam x ->
        // let json = JObject()
        // json.Add("constant_param", JValue x)
        // json :> JToken
        failwith <| sprintf "cannot serialize parameters to marlowe core json. you need to set this to a constant value: %A" x
    | NegValue v ->
        let json = JObject()
        json.Add("negate", toJsonValue v)
        json :> JToken
    | AddValue (lhs, rhs) ->
        let json = JObject()
        json.Add("add", toJsonValue lhs)
        json.Add("and", toJsonValue rhs)
        json :> JToken
    | SubValue (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("minus", toJsonValue rhs)
        json :> JToken
    | MulValue (lhs, rhs) ->
        let json = JObject()
        json.Add("multiply", toJsonValue lhs)
        json.Add("times", toJsonValue rhs)
        json :> JToken
    | DivValue (lhs, rhs) ->
        let json = JObject()
        json.Add("divide", toJsonValue lhs)
        json.Add("by", toJsonValue rhs)
        json :> JToken
    | ChoiceValue choiceId ->
        let json = JObject()
        json.Add("value_of_choice", toJsonChoiceId choiceId)
        json :> JToken
    | TimeIntervalStart ->
        JValue "time_interval_start"
    | TimeIntervalEnd ->
        JValue "time_interval_end"
    | UseValue valueId ->
        let json = JObject()
        json.Add("use_value", JValue valueId)
        json :> JToken
    | Cond (obs, tv, ev) ->
        let json = JObject()
        json.Add("if", toJsonObservation obs)
        json.Add("then", toJsonValue tv)
        json.Add("else", toJsonValue ev)
        json :> JToken

and toJsonObservation (observation : Observation) : JToken =
    match observation with
    | AndObs (lhs, rhs) ->
        let json = JObject()
        json.Add("both", toJsonObservation lhs)
        json.Add("and", toJsonObservation rhs)
        json :> JToken
    | OrObs (lhs, rhs) ->
        let json = JObject()
        json.Add("either", toJsonObservation lhs)
        json.Add("or", toJsonObservation rhs)
        json :> JToken
    | NotObs v ->
        let json = JObject()
        json.Add("not", toJsonObservation v)
        json :> JToken
    | ChoseSomething choiceId ->
        let json = JObject()
        json.Add("chose_something_for", JValue choiceId)
        json :> JToken
    | ValueGE (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("ge", toJsonValue rhs)
        json :> JToken
    | ValueGT (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("gt", toJsonValue rhs)
        json :> JToken
    | ValueLT (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("lt", toJsonValue rhs)
        json :> JToken
    | ValueLE (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("le", toJsonValue rhs)
        json :> JToken
    | ValueEQ (lhs, rhs) ->
        let json = JObject()
        json.Add("value", toJsonValue lhs)
        json.Add("equal_to", toJsonValue rhs)
        json :> JToken
    | TrueObs ->
        JValue true
    | FalseObs ->
        JValue false
and toJsonBounds (bounds : Bounds) : JToken = 
    let json_arr = JArray()
    for b in bounds do
        let json = JObject()
        json.Add("to",b._to)
        json.Add("from",b.from)
        json_arr.Add(json)
    json_arr :> JToken

and toJsonChoiceId (choideId : ChoiceId) : JToken = ""
and toJsonAction (action : Action) : JToken =
    match action with
    | Deposit (party, into_account, of_token, deposits) ->
        let json = JObject()
        
        json.Add("into_account", toJsonParty into_account)
        json.Add("party", toJsonParty party)
        json.Add("of_token", toJsonToken of_token)
        json.Add("deposits", toJsonValue deposits)
        json :> JToken
    | Choice (choiceId, bounds) ->
        let json = JObject()
        json.Add("for_choice", toJsonChoiceId choiceId)
        json.Add("choose_between", toJsonBounds bounds)
        json :> JToken
    | Notify obs ->
        let json = JObject()
        json.Add("notify_if", toJsonObservation obs)
        json :> JToken

and toJsonPayee (payee : Payee) : JObject =
    match payee with
    | Account acc -> 
        let json = JObject()
        let v = toJsonParty acc
        json.Add("account", v)
        json
    | Party party -> 
        let json = JObject()
        let v = toJsonParty party
        json.Add("party", v)
        json


and toJsonParty (party: Party) : JObject =
    match party with
    | Role role ->
        let json = JObject()
        json.Add("role_token", JValue role)
        json
    | Address addr ->
        let json = JObject()
        json.Add("address", JValue addr)
        json

and toJsonToken (token: Token) : JObject =
    let json = JObject()
    json.Add("currency_symbol", JValue token.CurrencySymbol)
    json.Add("token_name", JValue token.TokenName)
    json

and toJsonCase (case : Case) : JToken =
    match case with
    | Case (act, cont) ->
        let json = JObject()
        json.Add("case", toJsonAction act)
        json.Add("then", toJsonContract cont)
        json :> JToken

and toJsonContract (contract : Contract) : JToken =
    match contract with
    | Close ->
        JValue "close"
    | Pay (party, payee, token, value, cont) ->
        let json = JObject()
        json.Add("from_account", toJsonParty party)
        json.Add("to", toJsonPayee payee)
        json.Add("token", toJsonToken token)
        json.Add("pay", toJsonValue value)
        json.Add("then", toJsonContract cont)
        json :> JToken
    | If (obs, cont1, cont2) ->
        let json = JObject()
        json.Add("if", toJsonObservation obs)
        json.Add("then", toJsonContract cont1)
        json.Add("else", toJsonContract cont2)
        json :> JToken
    | When (cases, timeout, cont) ->
        let json = JObject()
        json.Add("when", JArray(cases |> List.map toJsonCase))
        json.Add("timeout", toJsonTimeout timeout |> JArray)
        json.Add("timeout_continuation", toJsonContract cont |> JArray)
        json :> JToken
    | Let (valId, value, cont) ->
        let json = JObject()
        json.Add("let", JValue valId)
        json.Add("be", toJsonValue value)
        json.Add("then", toJsonContract cont)
        json :> JToken
    | Assert (obs, cont) ->
        let json = JObject()
        json.Add("assert", toJsonObservation obs)
        json.Add("then", toJsonContract cont)
        json :> JToken

and toJsonTimeout (timeout : Timeout) : JToken =
    match timeout with
    | TimeParam p ->
        failwith <| sprintf "you need to initialize the time param %A before you can serialize this contract" p
    | POSIXTime t ->
        JValue t

let toJsonModule (moduleData : Module) : string =
    let metadata = moduleData.Metadata
    let contract = moduleData.Contract
    let json = JObject()
    json.Add("metadata", JValue metadata)
    json.Add("contract", toJsonContract contract)
    json.ToString()


