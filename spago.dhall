{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescreeps"
, dependencies =
    [ "console"
    , "effect"
    , "foreign-object"
    , "monoidal-containers"
    , "psci-support"
    , "random"
    , "run"
    , "screeps-classy"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
