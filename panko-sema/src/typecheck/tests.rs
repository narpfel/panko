use IntegralKind::*;
use Signedness::*;
use rstest::rstest;

use super::*;

#[test]
fn test_conversion_rank_comparison() {
    let lhs_ty = Arithmetic::Integral(Integral { signedness: Signed, kind: Int });
    let rhs_ty = Arithmetic::Integral(Integral {
        signedness: Signedness::Unsigned,
        kind: Int,
    });
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(lhs_ty, rhs_ty, compare_by_size_with_unsigned_as_tie_breaker),
    );
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(rhs_ty, lhs_ty, compare_by_size_with_unsigned_as_tie_breaker),
    );
}

#[rstest]
#[case(
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Signed, kind: PlainChar }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Short }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Short }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Char }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: LongLong }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: LongLong }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Long }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Signed, kind: Long }),
    Arithmetic::Integral(Integral { signedness: Unsigned, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Signed, kind: Long }),
)]
fn test_usual_arithmetic_conversions(
    #[case] lhs_ty: Arithmetic,
    #[case] rhs_ty: Arithmetic,
    #[case] expected: Arithmetic,
) {
    assert_eq!(
        perform_usual_arithmetic_conversions(lhs_ty, rhs_ty),
        expected,
    );
}
