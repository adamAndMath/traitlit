use traitlit::lit;

trait Test {
    fn test() -> bool;
}

#[lit(uint)]
impl Test for _ {
    fn test() -> bool {
        true
    }
}

#[lit(int)]
impl Test for _ {
    fn test() -> bool {
        false
    }
}

#[lit(uint)]
trait Zero {
    fn zero() -> Self {
        0
    }
}

fn main() {
    assert_eq!(u32::test(), true);
    assert_eq!(i8::test(), false);
    assert_eq!(usize::zero(), 0);
}