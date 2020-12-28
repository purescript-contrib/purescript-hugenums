{ name = "precise"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "gen"
  , "numbers"
  , "integers"
  , "lists"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
