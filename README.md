# memorize
`memorize` is a proc-macro crate which provides the attribute `#[memorize(...)]` to convert `const fn`s into lookup tables at compile time. Use wisely: beware premature optimization.

## Examples
These examples are meant to show off `#[memorize(...)]` without necessarily presenting useful applications.
```rust
// For all x in -3..3, memorize the output of square
#[memorize(domain = -3..3)]
const fn square(x: i32) -> u32 {
    (x * x) as u32
}

fn use_square() {
    // The outputs for these calls will be taken from a constant table
    for x in -3..3 {
        square(x);
    }
    // These calls will actually compute x * x
    for x in 4..100 {
        square(x);
    }
}

// For all x in -3..3, for all y in -3..=3, memorize the output of times
#[memorize(domain = (-3..3, -3..=3))]
const fn times(x: i32, y: i32) -> u32 {
    (x * y) as u32
}

// You can use arrays in domain for non-contiguous arguments
#[memorize(domain = ([1, 100], -3..3, -3..3))]
const fn check_eq(what: usize, x: i32, y: i32) -> usize {
    if x == y {
        what
    } else {
        0
    }
}

// Array domains can specify multiple arguments
#[memorize(domain = [(20, 1), (30, -10), (1000, 2)])]
const fn check_pos(what: usize, r: i64) -> usize {
    if r > 0 {
        what
    } else {
        0
    }
}

// Domains can contain arbitrary constant expressions
#[memorize(domain = {
    let mut domain = [(0, 0); 100];
    let mut i = 0;
    while i < 10 {
        domain[i] = (i as u32, i as u32 * 10);
        i += 1;
    }
    domain
})]
const fn im_hungry(food_size: u32, distance_to_me: u32) -> &'static str {
    if food_size > distance_to_me / 10 {
        "i will consume"
    } else {
        "too lazy"
    }
}

// Ranges can use constant expressions, so long as you still use .. or ..=
#[memorize(domain = -100..(2i32.pow(7) + 1))]
const fn tell_me_a_joke(desired_funniness: i32) -> &'static str {
    if desired_funniness >= 0 {
        "no"
    } else {
        "What do you call it when you eat a watch?
        
        TIME CONSUMING"
    }
}

// You can use const generics too, but because const generics can only
// literally appear in type position right now, you need to explicitely
// supply the size of each const generic part of your domain. 
// If you don't get the sizes right, bad things could happen silently.
#[memorize(domain = (0..300, 0..N), format = (_, N))]
const fn if_in_range<const N: usize>(what: usize, lower: usize) -> usize {
    if lower <= what && what < N {
        what
    } else {
        0
    }
}
```
