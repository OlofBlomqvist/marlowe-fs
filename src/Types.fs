module Types

type Timeout =
    | TimeParam of string
    | POSIXTime of int64

type Token = { 
    CurrencySymbol : string; 
    TokenName : string 
    
} with 
    static member ADA = { CurrencySymbol = "" ; TokenName = "" }
    static member Custom(symbol,name) = { CurrencySymbol = symbol ; TokenName = name }
type Value =
    | AvailableMoney of Party * Token
    | Constant of int64
    | ConstantParam of string
    | NegValue of Value
    | AddValue of Value * Value
    | SubValue of Value * Value
    | MulValue of Value * Value
    | DivValue of Value * Value
    | ChoiceValue of ChoiceId
    | TimeIntervalStart
    | TimeIntervalEnd
    | UseValue of ValueId
    | Cond of Observation * Value * Value

and Observation =
    | AndObs of Observation * Observation
    | OrObs of Observation * Observation
    | NotObs of Observation
    | ChoseSomething of string
    | ValueGE of Value * Value
    | ValueGT of Value * Value
    | ValueLT of Value * Value
    | ValueLE of Value * Value
    | ValueEQ of Value * Value
    | TrueObs
    | FalseObs

and Party =
    | Address of string
    | Role of string

and ChoiceId = { choice_name : string ; choice_owner: Party }
and AccountId = Party
and Bound = { from: int64; _to: int64}
and Bounds = Bound list
and Action =
    | Deposit of from_party : Party * into_account : AccountId * of_token: Token * deposits : Value
    | Choice of ChoiceId * Bounds
    | Notify of Observation

and Payee = Account of Party | Party of Party

and Case = Case of Action * Contract
and ValueId = string
and Contract =
    | Close
    | Pay of Party * Payee * Token * Value * Contract
    | If of Observation * Contract * Contract
    | When of Case list * Timeout * Contract
    | Let of ValueId * Value * Contract
    | Assert of Observation * Contract

and Module = { Metadata : string; Contract : Contract }