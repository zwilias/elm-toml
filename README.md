# `elm-toml`
> Parsing TOML in Elm

[![Build Status](https://travis-ci.org/zwilias/elm-toml.svg?branch=master)](https://travis-ci.org/zwilias/elm-toml)

This library attempts to model [TOML](https://github.com/toml-lang/toml) in Elm,
providing a means to parse a TOML document into an Elm datastructure.

## Usage

Let's say we have a TOML document like so:

```toml
################################################################################
## This is my very important settings document                                ##
################################################################################

org.name = "Test org"
org.created = 2017-08-11

z-index = 12321

[[user]]
name = "Ilias"

[[user]]
name = "Alice"

[[user]]
name = "Fido"
```

We could model an equivalent Elm datastructure like so:

```elm
import Toml.Calendar as Calendar


type alias User =
    { name : String }
    

type alias Organisation =
    { name : String
    , created : Calendar.Date
    }


type Settings =
    { zIndex : Int
    , org : Organisation
    , users : List User
    }
```

Now, writing some decoder for this should be pretty straightforward, too:

```elm
import Toml.Decode as Decode exposing (Decoder)


user : Decoder e User
user =
    Decode.map User (Decode.field "name" Decode.string)
    
    
organisation : Decoder e Organisation
organisation =
    Decode.map2 Organisation
        (Decode.field "name" Decode.string)
        (Decode.field "created" Decode.localDate)
        
        
settings : Decoder e Settings
settings =
    Decode.map3 Settings
        (Decode.field "z-index" Decode.int)
        (Decode.field "org" organisation)
        (Decode.field "users" (Decode.list user))
```

And finally, we could run the `settings` decoder against our `document` using
`Toml.Decode.decodeString settings document`.

## Contributing

Code in this project is formatted with `elm-format`. The currently used version
is specified in the `package.json` file. Verifying formatting is part of CI.

Examples in the documentation are verified using `elm-verify-examples`.

Unit tests are available in the `tests` directory and can be executed using
`elm-test`.

To run all tests locally:

```bash
$ npm i # Install all dependencies
$ npm test # Run tests
```

All contributions are welcome. Opening an issue and starting a conversation
before delving into the code is very much appreciated! Collaboration requires
communication.

## License

Licensed with love under BSD-3. Refer to the `LICENSE` file for more info.
