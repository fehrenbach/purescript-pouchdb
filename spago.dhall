{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "pouchdb"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "effect"
    , "foreign"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    , "unordered-collections"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
