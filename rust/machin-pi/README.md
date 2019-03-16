# machin-pi
## Calculate the digits of π !

### Install rust

Follow the [instructions](https://www.rust-lang.org/tools/install) here.


### Run the app

```
machin-pi jgrant$ time cargo +nightly run --release 100000
    Finished release [optimized] target(s) in 0.79s
     Running `target/release/machin-pi 100000`
first 100000 digits of π are 31415926...5549362464

real	0m37.502s
user	0m11.545s
sys	0m11.517s
```