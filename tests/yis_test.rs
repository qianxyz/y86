use y86::{decode, VM};

#[test]
fn yis_sum() {
    let obj = include_str!("./sum.yo");
    let mem = decode(obj.lines()).unwrap();
    let mut vm = VM::from_memory(mem);

    vm.run();

    assert_eq!(vm.stat, y86::Stat::Hlt);
    assert_eq!(vm.registers[0], 0xabcdabcdabcd);
}
