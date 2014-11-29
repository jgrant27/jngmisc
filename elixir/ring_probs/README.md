RingProbs
=========


This code calculates the probablities of all the node positions in a
ring topology of N nodes where moving clockwise with some
probability is P (where 0 >= P >=1) or counter-clockwise with
probability is 1-P, after S state transitions.

Here an example :

Initial node probablity for 5 node ring at S=0 is P=1 for starting node.

<pre>
                 N = 5 (nodes)
                 For S = 0 (initial state)

       1 - P = 0.5                        P = 0.5
   Counter-clockwise <---             ---> Clockwise
                            +-----+
                            | P = |
                 +----------+ 1.0 +----------+
                 |          |     |          |
                 |          +-----+          |
              +--+--+                     +--+--+
              |     |                     |     |
              | 0.0 |                     | 0.0 |
              |     |                     |     |
              +--+--+                     +--+--+
                 |                           |
                 |                           |
                 |  +-----+         +-----+  |
                 |  |     |         |     |  |
                 +--+ 0.0 +---------+ 0.0 +--+
                    |     |         |     |
                    +-----+         +-----+

</pre>

 Node probablities for the same 5 node ring after 2 state transitions

<pre>
                        N = 5 (nodes)
                        S = 2

       1 - P = 0.5                      P = 0.5
   Counter-clockwise <---             ---> Clockwise
                            +-----+
                            | P = |
                 +----------+ 0.5 +-------------+
                 |          |     |             |
                 |          +-----+             |
              +--+--+                        +--+--+
              |     |                        |     |
              | 0.0 |                        | 0.0 |
              |     |                        |     |
              +--+--+                        +--+--+
                 |                              |
                 |                              |
                 |  +------+         +-------+  |
                 |  |      |         |       |  |
                 +--+ 0.25 +---------+ 0.25  +--+
                    |      |         |       |
                    +------+         +-------+
</pre>


Example usage :

```sh
iex(27)> :timer.tc(fn -> RingProbs.calc_ring_probs(0.5, 1000000, 10) end, [])
  Calculating probs for ring with 1000000 nodes after 10 state transitions
  with a clockwise probability change of 0.5

{1512932,
 [0.24609375, 0.0, 0.205078125, 0.0, 0.1171875, 0.0, 0.0439453125, 0.0,
  0.009765625, 0.0, 9.765625e-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  ...]}
```
... or from the command line ...

```sh
$ mix run lib/ring_probs.ex 0.5 5 2
Calculating ring node probabilities where P=0.5 N=5 S=2 ...

[0.5, 0.0, 0.25, 0.25, 0.0]

calc time: 4.796 msecs
```
