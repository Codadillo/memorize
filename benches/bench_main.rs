use criterion::*;
use memorize::memorize;

// https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
const fn unmem_modinv(a: u32, m: u32) -> u32 {
    const fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
        if a == 0 {
            return (b, 0, 1);
        } else {
            let (g, y, x) = egcd(b % a, a);
            return (g, x - (b / a) * y, y);
        }
    }

    let (g, x, _) = egcd(a as i64, m as i64);
    if g == 1 {
        (x % m as i64).abs() as u32
    } else {
        panic!("Could not find modular inverse");
    }
}

#[memorize(domain = {
        let mut domain = [(0, 997); 996];
        let mut i = 0;
        while i < domain.len() {
            domain[i].0 = i as u32 + 1;
            i += 1;
        }
        domain
    })]
const fn l_modinv_array(a: u32, m: u32) -> u32 {
    unmem_modinv(a, m)
}

#[memorize(domain = (1..997, 997..=997))]
const fn l_modinv_range(a: u32, m: u32) -> u32 {
    unmem_modinv(a, m)
}

#[memorize(domain = {
    let mut domain = [(0, 11); 10];
    let mut i = 0;
    while i < domain.len() {
        domain[i].0 = i as u32 + 1;
        i += 1;
    }
    domain
})]
const fn s_modinv_array(a: u32, m: u32) -> u32 {
    unmem_modinv(a, m)
}

#[memorize(domain = (1..11, 11..=11))]
const fn s_modinv_range(a: u32, m: u32) -> u32 {
    unmem_modinv(a, m)
}

fn bench_modinv(c: &mut Criterion) {
    macro_rules! group_bench {
        ($group:ident, $name:expr, $func:ident($range:expr, $($arg:expr, )* )) => {
            $group.bench_function($name, |b| b.iter(|| {
                for i in $range {
                    $func(black_box(i), $(black_box($arg), )*);
                }
            }));
        };
    }

    // We will do some comparisons on a "large" input domain
    let mut g = c.benchmark_group("LargeInput");

    // First let's compare the performances of modinv within the memorized domain.
    //
    // As we can see, the memorized version of modinv using a range domain
    // drastically outperforms the unmemorized version.
    //
    // On the other hand, the version using a domain in array format performs worse
    // than the unmemorized version because it uses an O(n) lookup in its memory,
    // which ends up being slower han just calling modinv directly.
    // This is why you should be careful when using range domains.
    group_bench!(g, "InDomain/Unmemorized", unmem_modinv(1..997, 997,));
    group_bench!(g, "InDomain/Memorized/range", l_modinv_range(1..997, 997,));
    group_bench!(g, "InDomain/Memorized/array", l_modinv_array(1..997, 997,));

    // Now let's compare their performances outside of the memorized domain.
    //
    // The unmemorized version performs similarly to the memorized version using
    // a range domain.
    //
    // Like before, the array domain version is drastically outperformed by the other
    // two versions.
    group_bench!(g, "!InDomain/Unmemorized", unmem_modinv(1..991, 991,));
    group_bench!(g, "!InDomain/Memorized/range", l_modinv_range(1..991, 991,));
    group_bench!(g, "!InDomain/Memorized/array", l_modinv_array(1..991, 991,));

    g.finish();

    // We will now do some comparisons on a small input domain
    let mut g = c.benchmark_group("SmallInput");

    // First let's compare the performances of modinv within the memorized domain.
    //
    // As we can see, every memorized version of modinv outperforms the
    // unmemorized version.
    group_bench!(g, "InDomain/Unmemorized", unmem_modinv(1..11, 11,));
    group_bench!(g, "InDomain/Memorized/range", s_modinv_range(1..11, 11,));
    group_bench!(g, "InDomain/Memorized/array", s_modinv_array(1..11, 11,));

    // Now let's compare their performances outside of the memorized domain.
    //
    // The unmemorized version performs similarly to the range domain version.
    // The array domain version performs much much worse because its O(n) check is 
    // run through fully with no chance to short circuit. 
    group_bench!(g, "!InDomain/Unmemorized", unmem_modinv(1..13, 13,));
    group_bench!(g, "!InDomain/Memorized/range", l_modinv_range(1..13, 13,));
    group_bench!(g, "!InDomain/Memorized/array", l_modinv_array(1..13, 13,));

    g.finish();
}

criterion_group!(benches, bench_modinv);
criterion_main!(benches);
