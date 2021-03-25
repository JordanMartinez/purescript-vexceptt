{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "spec"
  , "transformers"
  , "unsafe-coerce"
  , "veither"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
