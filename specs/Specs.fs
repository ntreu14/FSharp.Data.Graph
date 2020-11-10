module Specs

open Expecto

let specs : Test =
  testList "FSharp Graph specs"
    [
      // Implement tests
    ]

let testConfig = 
  { defaultConfig with runInParallel=true; verbosity=Logging.LogLevel.Verbose }

[<EntryPoint>]
let main argv =
  runTestsWithArgs testConfig argv specs
 