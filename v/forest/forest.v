// # v run -prod forest.v

module main

import time

const (
  MAX_VAL = 7
  MAX_THREADS = 1000
)

// Define our binary node and funcs

struct Node {
  is_init bool = false
pub mut:
  val int
  left &Node = &Node{}
  right &Node = &Node{}
}

fn get_indent(n int) string {
  mut indent := ' '
  for i:=0; i < n; i++ {
    indent += ' '
  }
  return indent
}

pub fn (n Node) str() string {
  if !n.is_init {
    return 'Node{}'
  }
  mut s := 'Node{ val: $n.val'
  if n.left.is_init {
    indent := get_indent(n.val)
    s += '\n$indent left: ' + n.left.str()
  }
  if n.right.is_init {
    indent := get_indent(n.val)
    s += '\n$indent right: ' + n.right.str()
  }
  s += ' }'
  return s
}

pub fn create_node(v int) &Node {
  return &Node{ is_init: true, val: v }
}

pub fn (n mut Node) set_children(ln &Node, rn &Node) {
  n.left = ln
  n.right = rn
}



// Some work funcs

pub fn (n mut Node) create_tree() {
  mut ln := create_node(n.val + 1)
  if n.val + 1 < MAX_VAL {
    ln.create_tree()
  }
  mut rn := create_node(n.val + 1)
  if n.val + 1 < MAX_VAL {
    rn.create_tree()
  }
  n.set_children(ln, rn)
}

fn do_work(i int){
  println("thread $i running ...")
  mut root := create_node(1)
  root.create_tree()
  ts := *root
  println("thread $i tree:\n$ts")
  println("thread $i finished.\n")
}


// Entry point
fn main() {
  for i := 1; i <= MAX_THREADS; i++ {
    go do_work(i)
  }
  for {
    time.sleep_ms(1000)
  }
}
