
use trybuild;

#[test]
fn add() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/generated/should_fail/*.rs");
    t.pass("tests/ui/generated/should_pass/*.rs");
}
