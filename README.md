# Marlowe-FS

F# Bindings for [Marlowe-RS] (https://github.com/OlofBlomqvist/marlowe-rs)

*Based on marlowe-rs v0.2.0 via WASI*

**This is an extremely unstable repository, don't try to use it for anything important**

Project goals:

- Provide all features that Marlowe_Lang ([Marlowe-RS](https://github.com/OlofBlomqvist/marlowe-rs)) does.
- Allow for designing Marlowe contracts in F#
- Provide base layer for equivalent C# Marlowe SDK


```
$ dotnet fsi .\example.fsx >

Marlowe Core JSON -> F#:
Pay
  (Role "role2", Account (Role "role1"), { CurrencySymbol = "def"
                                           TokenName = "abc" }, Constant 111111L,
   Close)

F# -> Marlowe Core JSON:
{
  "from_account": {
    "role_token": "nisse"
  },
  "to": {
    "party": {
      "role_token": "Kalle"
    }
  },
  "token": {
    "currency_symbol": "symbol",
    "token_name": "name"
  },
  "pay": 10,
  "then": "close"
}

CBOR-HEX datum -> Marlowe Core JSON:
"{
  "marlowe_params": "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d",
  "state": {
    "accounts": [
      [
        [
          {
            "address": "addr_test1vrsdcu86j6v8yljrnh69ssm8jlg90n075hnu9pzdz2m2lzsdlspjq"
          },
          {
            "token_name": "",
            "currency_symbol": ""
          }
        ],
        3000000
      ]
    ],
    "choices": [],
    "boundValues": [],
    "minTime": 0
  },
  "contract": {
    "token": {
      "token_name": "",
      "currency_symbol": ""
    },
    "to": {
      "party": {
        "role_token": "WithdrawalTest1"
      }
    },
    "then": "close",
    "pay": 10000000,
    "from_account": {
      "address": "addr_test1vrsdcu86j6v8yljrnh69ssm8jlg90n075hnu9pzdz2m2lzsdlspjq"
    }
  }
}"
```