use traitlit::lit;

trait Test {
    fn test() -> bool;
}

#[lit(V = u_)]
impl Test for V {
    fn test() -> bool {
        true
    }
}

#[lit(V = i_)]
impl Test for V {
    fn test() -> bool {
        false
    }
}

#[lit(V = u_)]
trait Zero {
    fn zero() -> Self {
        0
    }
}

#[lit(V = [u128])]
trait Foo<T = V> {
    fn foo() -> Self {
        1
    }
}

fn main() {
    assert_eq!(u32::test(), true);
    assert_eq!(i8::test(), false);
    assert_eq!(usize::zero(), 0);
    assert_eq!(<u128 as Foo<u128>>::foo(), 1);
}