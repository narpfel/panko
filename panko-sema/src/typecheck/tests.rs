use rstest::rstest;
use IntegralKind::*;
use Signedness::*;

use super::*;

#[test]
fn test_conversion_rank_comparison() {
    let lhs_ty = Arithmetic::Integral(Integral { signedness: None, kind: Int });
    let rhs_ty = Arithmetic::Integral(Integral {
        signedness: Some(Signedness::Unsigned),
        kind: Int,
    });
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(lhs_ty, rhs_ty, SIZE_WITH_UNSIGNED_AS_TIE_BREAKER),
    );
    assert_eq!(
        [lhs_ty, rhs_ty],
        std::cmp::minmax_by_key(rhs_ty, lhs_ty, SIZE_WITH_UNSIGNED_AS_TIE_BREAKER),
    );
}

#[rstest]
#[case(
    Arithmetic::Char,
    Arithmetic::Char,
    Arithmetic::Integral(Integral { signedness: None, kind: Int }),
)]
#[case(
    Arithmetic::Char,
    Arithmetic::Integral(Integral { signedness: None, kind: Short }),
    Arithmetic::Integral(Integral { signedness: None, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Int }),
    Arithmetic::Integral(Integral { signedness: None, kind: Int }),
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Short }),
    Arithmetic::Integral(Integral { signedness: None, kind: Char }),
    Arithmetic::Integral(Integral { signedness: None, kind: Int }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Long }),
    Arithmetic::Integral(Integral { signedness: None, kind: LongLong }),
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: LongLong }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Long }),
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Int }),
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Long }),
)]
#[case(
    Arithmetic::Integral(Integral { signedness: Some(Signed), kind: Long }),
    Arithmetic::Integral(Integral { signedness: Some(Unsigned), kind: Int }),
    Arithmetic::Integral(Integral { signedness: Some(Signed), kind: Long }),
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
