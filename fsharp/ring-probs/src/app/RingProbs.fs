//
// Copyright (c) 2014, Justin Grant <justin at imagine27 dot com>
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this list
// of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this
// list of conditions and the following disclaimer in the documentation and/or
// other materials provided with the distribution.
// Neither the name of the <ORGANIZATION> nor the names of its contributors may be
// used to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   // LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
                                                                       // NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

//
// This code calculates the probablities of all the node positions in a
// ring topology of N nodes where moving clockwise with some
// probability P (where 0 >= P >=1) or counter-clockwise with
// probability 1-P after S state transitions.
//
// Initial node probablity for 5 node ring at S=0 is P=1 for starting node.
//
// N = 5 (nodes)
// For S = 0 (initial state)
//
//   1 - P = 0.5                  P = 0.5
//   Counter-clockwise <--- ---> Clockwise
//                 +-----+
//                 | P = |
//      +----------+ 1.0 +----------+
//      |          |     |          |
//      |          +-----+          |
//   +--+--+                     +--+--+
//   |     |                     |     |
//   | 0.0 |                     | 0.0 |
//   |     |                     |     |
//   +--+--+                     +--+--+
//      |                           |
//      |                           |
//      |  +-----+         +-----+  |
//      |  |     |         |     |  |
//      +--+ 0.0 +---------+ 0.0 +--+
//         |     |         |     |
//         +-----+         +-----+
//
//
// Node probablities for 5 node ring after 2 state transitions
//
// N = 5 (nodes)
// S = 2
//
//   1 - P = 0.5                  P = 0.5
//   Counter-clockwise <--- ---> Clockwise
//                 +-----+
//                 | P = |
//      +----------+ 0.5 +-------------+
//      |          |     |             |
//      |          +-----+             |
//   +--+--+                        +--+--+
//   |     |                        |     |
//   | 0.0 |                        | 0.0 |
//   |     |                        |     |
//   +--+--+                        +--+--+
//      |                              |
//      |                              |
//      |  +------+         +-------+  |
//      |  |      |         |       |  |
//      +--+ 0.25 +---------+ 0.25  +--+
//         |      |         |       |
//         +------+         +-------+
//


module RingProbs


type ParsedArgs = { probability : float; nodes : int; states : int }


let printUsage msg = printfn "\n%s\n\n\
                              Usage: RingProbs.exe probability nodecount states\n\
                              \te.g. RingProbs.exe 0.5 5 10\n" msg
                     exit 0

let rec calcStateProbs (prob: float, i: int,
                        currProbs: float [], newProbs: float []) = 
  if i < 0 then
    newProbs
  else
    let maxIndex = currProbs.Length-1
    // Match prev, next probs based on the fact that this is a
    // ring structure.
    let (prevProb, nextProb) =
      match i with
        | i when i = maxIndex -> (currProbs.[i-1], currProbs.[0])
        | 0 -> (currProbs.[maxIndex], currProbs.[i+1])
        | _ -> (currProbs.[i-1], currProbs.[i+1])
    let newProb = prob * prevProb + (1.0 - prob) * nextProb
    Array.set newProbs i newProb
    calcStateProbs(prob, i-1, currProbs, newProbs)



let calcRingProbs parsedArgs =
  // Probs at S = 0.
  //   Make certain that we are positioned at only start location.
  //     e.g. P(Start Node) = 1
  let startProbs =
    Array.concat [ [| 1.0 |] ; [| for _ in 1 .. parsedArgs.nodes - 1 -> 0.0 |] ] 
  let endProbs =
    List.fold (fun probs _ ->
               calcStateProbs(parsedArgs.probability, probs.Length-1,
                              probs, Array.create probs.Length 0.0))
              startProbs [1..parsedArgs.states]
  endProbs
  
let getParsedArgs args =
  try
    match args with
      | [|ps; ns; ss|] -> Some { probability = float ps; nodes = int ns; states = int ss; }
      | _ -> printUsage ""
  with | ex -> printUsage(ex.ToString())



[<EntryPoint>]
let main (args : string[]) =

  let args = getParsedArgs(args).Value
  printfn "\nRunning ring probabilities with parameters ...\n%A\n" args
  printfn "%A" (calcRingProbs args)
  0
