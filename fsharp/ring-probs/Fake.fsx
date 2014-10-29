#r "packages/FAKE/tools/FakeLib.dll" // include Fake lib
open Fake
open Fake.FscHelper


// Directories
let buildRootDir  = "./build/"
let srcRootDir = "./src/"
let appSubDir = "app/"
let testSubDir = "test/"
let buildAppDir   = buildRootDir + appSubDir
let buildTestDir   = buildRootDir + testSubDir
let srcAppDir = srcRootDir + appSubDir
let srcTestDir = srcRootDir + testSubDir
let deployDir = "./deploy/"



// version info
let version = "0.1"  // or retrieve from CI server


// Targets
Target "Clean" (fun _ ->
                CleanDirs [buildRootDir; deployDir]
                )

Target "RingProbs.exe" (fun _ ->
                        ScanImmediately !! (srcAppDir + "**/*.fs")
                        |> Fsc (fun parameters ->
                                { parameters with
                                  Output = buildRootDir + "RingProbs.exe"
                                  FscTarget = Exe })
                        )

Target "Build" (fun _ ->
                trace "Building ..."
                )

Target "Test" (fun _ ->
               trace "Testing ..."
               )

Target "Deploy" (fun _ ->
                 trace "Deploying ..."
                 )

// define the dependencies
"RingProbs.exe"
  ==> "Build"

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Deploy"


RunTargetOrDefault "Test"
