use IntegralKind::*;
use Signedness::*;

use super::*;
use crate::ty::Arithmetic;

#[test]
fn test_conversion_rank_comparison() {
    let lhs_ty = Arithmetic::Integral(Integral { signedness: Signed, kind: Int });
    let rhs_ty = Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int });
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(lhs_ty, rhs_ty, compare_by_size_with_unsigned_as_tie_breaker),
    );
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(rhs_ty, lhs_ty, compare_by_size_with_unsigned_as_tie_breaker),
    );
}

macro_rules! test_usual_arithmetic_conversions {
    ($name:ident, $lhs_ty:expr, $rhs_ty:expr, $expected:expr $(,)?) => {
        #[test]
        fn $name() {
            assert_eq!(
                perform_usual_arithmetic_conversions($lhs_ty, $rhs_ty),
                $expected,
            );
        }
    };
}

test_usual_arithmetic_conversions!(
    char_to_int,
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
);

test_usual_arithmetic_conversions!(
    char_and_short_to_int,
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Short }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
);

test_usual_arithmetic_conversions!(
    unsigned_signed_to_unsigned,
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
);

test_usual_arithmetic_conversions!(
    unsigned_short_signed_to_signed,
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Short }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Char }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
);

test_usual_arithmetic_conversions!(
    unsigned_signed_long_to_unsigned,
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: LongLong }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: LongLong }),
);

test_usual_arithmetic_conversions!(
    unsigned_long_and_int_to_unsigned_long,
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
);

test_usual_arithmetic_conversions!(
    signed_long_and_unsigned_int_to_long,
    Arithmetic::Integral(Integral { signedness: Signed, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Long }),
);

#[test]
fn test_enum_underlying_ty() {
    use constexpr::Integral::*;

    assert_eq!(
        ty::Type::UINT,
        ty::Type::Arithmetic(find_underlying_ty([0, 1, 2, 3].map(Unsigned))),
    );

    assert_eq!(
        ty::Type::INT,
        ty::Type::Arithmetic(find_underlying_ty([0, 1, -2, 3].map(Signed))),
    );

    assert_eq!(
        ty::Type::ULONG,
        ty::Type::Arithmetic(find_underlying_ty([0, 1, 0x1_0000_0000, 3].map(Unsigned))),
    );

    assert_eq!(
        ty::Type::LONG,
        ty::Type::Arithmetic(find_underlying_ty([0, -1, 0x1_0000_0000, 3].map(Signed))),
    );

    assert_eq!(
        ty::Type::LONG,
        ty::Type::Arithmetic(find_underlying_ty(
            [0, -0x1_0000_0000, 0x1_0000_0000, 3].map(Signed),
        )),
    );
}

#[should_panic]
#[test]
fn test_enum_underlying_ty_impossible() {
    use constexpr::Integral::*;

    assert_eq!(
        ty::Type::LONG,
        ty::Type::Arithmetic(find_underlying_ty([Signed(-1), Unsigned(u64::MAX)])),
    );
}
