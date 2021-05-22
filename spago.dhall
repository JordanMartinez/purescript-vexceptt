{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "effect"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "spec"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  , "veither"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
