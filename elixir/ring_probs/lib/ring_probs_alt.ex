#
# Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this list
# of conditions and the following disclaimer.
# Redistributions in binary form must reproduce the above copyright notice, this
# list of conditions and the following disclaimer in the documentation and/or
# other materials provided with the distribution.
# Neither the name of the <ORGANIZATION> nor the names of its contributors may be
# used to endorse or promote products derived from this software without specific
# prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

#
# This code calculates the probablities of all the node positions in a
# ring topology of N nodes where moving clockwise with some
# probability P (where 0 >= P >=1) or counter-clockwise with
# probability 1-P after S state transitions.
#
# Initial node probablity for 5 node ring at S=0 is P=1 for starting node.
#
#                  N = 5 (nodes)
#                  For S = 0 (initial state)
#
#        1 - P = 0.5                        P = 0.5
#    Counter-clockwise <---             ---> Clockwise
#                             +-----+
#                             | P = |
#                  +----------+ 1.0 +----------+
#                  |          |     |          |
#                  |          +-----+          |
#               +--+--+                     +--+--+
#               |     |                     |     |
#               | 0.0 |                     | 0.0 |
#               |     |                     |     |
#               +--+--+                     +--+--+
#                  |                           |
#                  |                           |
#                  |  +-----+         +-----+  |
#                  |  |     |         |     |  |
#                  +--+ 0.0 +---------+ 0.0 +--+
#                     |     |         |     |
#                     +-----+         +-----+
#
#
#  Node probablities for 5 node ring after 2 state transitions
#
#                         N = 5 (nodes)
#                         S = 2
#
#        1 - P = 0.5                      P = 0.5
#    Counter-clockwise <---             ---> Clockwise
#                             +-----+
#                             | P = |
#                  +----------+ 0.5 +-------------+
#                  |          |     |             |
#                  |          +-----+             |
#               +--+--+                        +--+--+
#               |     |                        |     |
#               | 0.0 |                        | 0.0 |
#               |     |                        |     |
#               +--+--+                        +--+--+
#                  |                              |
#                  |                              |
#                  |  +------+         +-------+  |
#                  |  |      |         |       |  |
#                  +--+ 0.25 +---------+ 0.25  +--+
#                     |      |         |       |
#                     +------+         +-------+
#

#
# Example usage :
#
# iex(27)> :timer.tc(fn -> RingProbs.calc_ring_probs(0.5, 1000000, 10) end, [])
#   Calculating probs for ring with 1000000 nodes after 10 state transitions
#   with a clockwise probability change of 0.5
#
# {1512932,
#  [0.24609375, 0.0, 0.205078125, 0.0, 0.1171875, 0.0, 0.0439453125, 0.0,
#   0.009765625, 0.0, 9.765625e-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
#   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
#   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
#   ...]}
#
# ... or from the command line ...
#
# $ mix run lib/ring_probs.ex 0.5 5 2
# Calculating ring node probabilities where P=0.5 N=5 S=2 ...
#
# [0.5, 0.0, 0.25, 0.25, 0.0]
#
# calc time: 4.796 msecs
#


defmodule RingProbsAlt do
  use Application

  import Enum

  # Util functions for Erlang arrays. 
  def atake(arr, n) do
    reduce 0..n-1, :array.new([size: n, fixed: true, default: 0.0]), 
                          fn(i, res) -> :array.set(i, :array.get(i, arr), res) end
  end

  def adrop(arr, n) do
    sz = :array.size(arr)
    if n < sz do
      reduce 0..sz-1-n, 
                :array.new([size: n, fixed: true, default: 0.0]), 
                       fn(i, res) -> :array.set(i, :array.get(n+i, arr), res) end
    else 
      :array.new(0) 
    end
  end

  def pmap_earray(fun, arr) do
    IO.puts :erlang.system_info(:process_limit)
    me = self    
    pids = :array.map fn (i, elem) ->
                           spawn fn -> (send me, { self, fun.(i, elem) }) end
                      end, arr
    :array.map fn (_, pid) ->
                    receive do { ^pid, result } -> result end
               end, pids
  end

  def calc_state_probs(p, prev_probs)
  when is_float(p) and p >= 0 and p <= 1 do
    sz = :array.size(prev_probs)
    #pmap_earray fn(i, _) ->
    :array.map fn(i, _) ->
                   prev_i = if i == 0 do sz - 1 else i - 1 end
                   prev_prob = :array.get(prev_i, prev_probs)
                   next_i = if i == sz - 1 do 0 else i + 1 end
                   next_prob = :array.get(next_i, prev_probs)
                   p * prev_prob + (1 - p) * next_prob
               end, prev_probs
  end

  def calc_ring_probs(p, n, s)
  when is_float(p) and p >= 0 and p <= 1 and
  is_integer(n) and n > 0 and
  is_integer(s) and s >= 0 do

    # Probs at S = 0. 
    #   Certain that we are positioned at only start location.
    #     e.g. P(Start Node) = 1
    initial_probs = :array.new [size: n, fixed: true, default: 0.0]
    initial_probs = :array.set 0, 1.0, initial_probs
    final_probs = initial_probs
    IO.puts "Calculating ring node probabilities where P=#{p} N=#{n} S=#{s} ...\n"

    # If we are moving beyond the initial state then do the calculation ...
    if s > 0 do
      # ... through all the states ...
      final_probs = 
        reduce 1..s, 
                  initial_probs,
                  fn (_, new_probs) -> calc_state_probs(p, new_probs) end
    end

    final_probs
  end

  # See http://elixir-lang.org/docs/stable/Application.html
  # for more information on OTP Applications
  def start(_type, _args)  do
    res = RingProbs.Supervisor.start_link

    if count(System.argv) === 3 do
     {p, _} = Float.parse(at(System.argv, 0))
      {n, _} = Integer.parse(at(System.argv, 1))
      {s, _} = Integer.parse(at(System.argv, 2))
    else
      [p, n, s] = [0.5, 10000, 2]
    end

    {time, probs} = :timer.tc(fn -> RingProbsAlt.calc_ring_probs(p, n, s) end, [])

    nc = 100
    truncated = true
    if n <= nc do
      nc = n
      truncated = false
    end

    if truncated do
      IO.inspect atake(probs, div(nc,2)), limit: :infinity
      IO.puts "... #{n - div(nc, 2)} node probabilities ..."
      IO.inspect adrop(probs, n - div(nc,2)), limit: :infinity
    else
      IO.inspect probs, limit: :infinity
    end

    IO.puts "\ncalc time: #{time/1000} msecs\n"

    res
  end

  def main(_) do

  end

end
