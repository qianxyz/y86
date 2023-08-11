use y86::assemble;

#[test]
fn yas_sum() {
    let src = include_str!("./sum.ys");
    let output = assemble(src.lines()).unwrap();

    let expected = include_str!("./sum.yo");
    for (out, exp) in output.iter().zip(expected.lines()) {
        assert_eq!(out, exp);
    }
}
