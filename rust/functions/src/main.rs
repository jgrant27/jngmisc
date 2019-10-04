fn compose<'a,F,G,T,U,V>(f: F, g: G) -> Box<dyn Fn(T) -> V + 'a>
    where F: Fn(U) -> V + 'a,
          G: Fn(T) -> U + 'a,
{
   Box::new(move |x| f(g(x)))
}

fn main() {
    fn g(a: i128) -> i128 {
        println!("in function g input value is {}", a);
        let r = a * a;
        println!("in function g computed value is {}", r);
        r
    }
    fn f(b: i128) -> i128 {
        println!("in function f input value is {}", b);
        let r = b + b + 1;
        println!("in function f computed value is {}", r);
        r
    }
    let h = compose(f, g);
    assert_eq!(19, h(3));
}
