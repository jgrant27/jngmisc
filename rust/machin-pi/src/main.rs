use ramp::Int;

#[macro_use]
extern crate partial_application;

fn machinpi(dcnt: usize) -> Int {
    fn arccot(x: &Int, unity: &Int) -> Int {
        fn _arccot(x: &Int, n: &Int, xpow: &Int, term: &Int, l: &Int) -> Int {
            let zero = Int::from(0);
            if zero == *term {
                term.to_owned()
            } else {
                let one = Int::from(1);
                let a = partial!(_arccot =>
                                 x,
                                 &(n + 2),
                                 &(xpow / x),
                                 &(xpow / n),
                                 _);
                let b = xpow / n;
                if one == *l {
                    a(&zero) + b
                } else {
                    a(&one) - b
                }
            }
        }
        _arccot(
            &(x * x),
            &Int::from(1),
            &(unity / x),
            &(unity / x),
            &Int::from(1),
        )
    }

    let guard = 10 + 10_f32.log10().ceil() as usize;
    let unity = Int::from(10).pow(dcnt + guard);
    let a = 4 * arccot(&Int::from(5), &unity);
    let b = arccot(&Int::from(239), &unity);
    let c = &Int::from(10).pow(guard);
    4 * (a - b) / c
}

fn main() {
    let builder = std::thread::Builder::new()
        .name("calc_thread".into())
        .stack_size(64 * 1024 * 1024); // 64MB of stack space
    let handler = builder
        .spawn(|| {
            let args: Vec<String> = std::env::args().collect();
            let dcnt = 8;
            let _cnt = match args.len() {
                n if n <= 1 => dcnt,
                _ => match args[1].parse::<usize>().ok() {
                    None => dcnt,
                    Some(cnt) => cnt,
                },
            };
            println!("first {} digits of π are {}", _cnt, machinpi(_cnt - 1));
        })
        .unwrap();
    handler.join().unwrap();
}
