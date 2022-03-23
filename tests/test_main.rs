use memorize::memorize;

const fn modinv(a: u32, m: u32) -> u32 {
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

#[test]
fn range_domain() {
    #[memorize(domain = 1..6)]
    const fn modinv_11(a: u32) -> u32 {
        modinv(a, 11)
    }

    for a in 1..11 {
        assert_eq!(modinv_11(a), modinv(a, 11));
    }
}

#[test]
fn range_inc_domain() {
    #[memorize(domain = 1..=6)]
    const fn modinv_11(a: u32) -> u32 {
        modinv(a, 11)
    }

    for a in 1..11 {
        assert_eq!(modinv_11(a), modinv(a, 11));
    }
}

#[test]
fn range_single_domain() {
    #[memorize(domain = 1..=1)]
    const fn modinv_11(a: u32) -> u32 {
        modinv(a, 11)
    }

    for a in 1..11 {
        assert_eq!(modinv_11(a), modinv(a, 11));
    }
}

#[test]
fn range_half_negative_domain() {
    #[memorize(domain = -1..3)]
    const fn times_3(x: i32) -> i32 {
        x * 3
    }

    for x in -100..100 {
        assert_eq!(times_3(x), x * 3);
    }
}

#[test]
fn range_negative_domain() {
    #[memorize(domain = -10..-3)]
    const fn times_3(x: i32) -> i32 {
        x * 3
    }

    for x in -100..100 {
        assert_eq!(times_3(x), x * 3);
    }
}

#[test]
fn range_single_negative_domain() {
    #[memorize(domain = -10..=-10)]
    const fn times_3(x: i32) -> i32 {
        x * 3
    }

    for x in -100..100 {
        assert_eq!(times_3(x), x * 3);
    }
}

#[test]
fn array_domain() {
    #[memorize(domain = [1, 2, 3, 4, 5])]
    const fn modinv_11(a: u32) -> u32 {
        modinv(a, 11)
    }

    for a in 1..11 {
        assert_eq!(modinv_11(a), modinv(a, 11));
    }
}

#[test]
fn array_multiarg_domain() {
    #[memorize(domain = [(1, 11), (2, 11), (3, 11), (4, 11), (5, 11)])]
    const fn modinv_t(a: u32, m: u32) -> u32 {
        modinv(a, m)
    }

    for a in 1..11 {
        for m in [11, 13, 17, 19, 23] {
            assert_eq!(modinv_t(a, m), modinv(a, m));
        }
    }
}

#[test]
fn array_expr_domain() {
    #[memorize(domain = {
        let mut out = [(0, 11); 10];
        let mut i = 0;
        while i < 10 {
            out[i].0 = (i + 1) as u32;
            i += 1;
        }
        out
    })]
    const fn modinv_t(a: u32, m: u32) -> u32 {
        modinv(a, m)
    }

    for a in 1..11 {
        for m in [11, 13, 17, 19, 23] {
            assert_eq!(modinv_t(a, m), modinv(a, m));
        }
    }
}

#[test]
fn range_product_domain() {
    #[memorize(domain = (-11..11, 23..40))]
    const fn times(x: i32, y: i32) -> i32 {
        x * y
    }

    for x in -100..100 {
        for y in -100..100 {
            assert_eq!(times(x, y), x * y)
        }
    }
}

#[test]
fn range_many_product_domain() {
    #[memorize(domain = (-11..11, 0..5, -12..=20, 0..10))]
    const fn times(x: i32, y: i32, z: i32, w: i32) -> i32 {
        x * y * z * w
    }

    for x in -10..10 {
        for y in -10..10 {
            for z in -10..10 {
                for w in -10..10 {
                    assert_eq!(times(x, y, z, w), x * y * z * w)
                }
            }
        }
    }
}

#[test]
fn array_product_domain() {
    #[memorize(domain = ([1, 2, 3, 4, 5, 6], [7, 11, 13, 17, 19]))]
    const fn modinv_t(a: u32, b: u32) -> u32 {
        modinv(a, b)
    }

    for a in 1..7 {
        for m in [7, 11, 13, 17, 19, 23] {
            assert_eq!(modinv_t(a, m), modinv(a, m));
        }
    }
}

#[test]
fn array_product_many_domain() {
    #[memorize(domain = ([0, 1, 2, 3, 4], [1, 2, 3], [-1, -2], [3]))]
    const fn times(x: i32, y: i32, z: i32, w: i32) -> i32 {
        x * y * z * w
    }

    for x in -10..10 {
        for y in -10..10 {
            for z in -10..10 {
                for w in -10..10 {
                    assert_eq!(times(x, y, z, w), x * y * z * w)
                }
            }
        }
    }
}

#[test]
fn array_tuple_product_many_domain() {
    #[memorize(domain = ([(0, 3), (1, 1), (2, 2), (3, 1), (4, -1)], [-1, -2], [3]))]
    const fn times(x: i32, y: i32, z: i32, w: i32) -> i32 {
        x * y * z * w
    }

    for x in -10..10 {
        for y in -10..10 {
            for z in -10..10 {
                for w in -10..10 {
                    assert_eq!(times(x, y, z, w), x * y * z * w)
                }
            }
        }
    }
}

#[test]
fn array_range_product_domain() {
    #[memorize(domain = ([1, 2, 3], -100..100))]
    const fn times(x: i32, y: i32) -> i32 {
        x * y
    }

    for x in -10..10 {
        for y in -10..10 {
            assert_eq!(times(x, y), x * y)
        }
    }
}

#[test]
fn range_array_product_domain() {
    #[memorize(domain = (1..7, [7, 11, 13, 17, 19]))]
    const fn modinv_t(a: u32, b: u32) -> u32 {
        modinv(a, b)
    }

    for a in 1..7 {
        for m in [7, 11, 13, 17, 19, 23] {
            assert_eq!(modinv_t(a, m), modinv(a, m));
        }
    }
}

#[test]
fn range_array_product_many_domain() {
    #[memorize(domain = ([0, 1, 2, 3, 4], 1..3, [-1, -2], 5..=17))]
    const fn times(x: i32, y: i32, z: i32, w: i32) -> i32 {
        x * y * z * w
    }

    for x in -10..10 {
        for y in -10..10 {
            for z in -10..10 {
                for w in -10..10 {
                    assert_eq!(times(x, y, z, w), x * y * z * w)
                }
            }
        }
    }
}
