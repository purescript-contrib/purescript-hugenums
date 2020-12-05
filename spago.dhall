{ name = "precise"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "gen"
  , "generics-rep"
  , "numbers"
  , "integers"
  , "lists"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
