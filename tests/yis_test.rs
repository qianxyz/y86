use y86::emulate;

#[test]
fn yis_sum() {
    let obj = include_str!("./sum.yo");
    let vm = emulate(obj.lines()).unwrap();

    assert_eq!(vm.stat, y86::Stat::Hlt);
    assert_eq!(vm.registers[0], 0xabcdabcdabcd);
}
