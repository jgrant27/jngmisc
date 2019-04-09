#[derive(Debug, PartialEq)]
struct Tree<'a> {
  val: i64,
  left: Option<&'a Tree<'a>>,
  right: Option<&'a Tree<'a>>,
}

fn main() {
    let t = Tree{val: 0, left: None, right: None};
    dbg!(&t);
    let u = Tree{val: 1, left: Some(&t), right: None};
    dbg!(&u);
    assert!(&t == u.left.unwrap());
}
