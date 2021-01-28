{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-select"
  , "psci-support"
  , "remotedata"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
