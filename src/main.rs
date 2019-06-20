use traitlit::lit;

trait Test {
    fn test() -> Self;
}

#[lit(V = u_)]
impl Test for V {
    fn test() -> V {
        1
    }
}

#[lit(V = i_)]
impl Test for V {
    fn test() -> V {
        0
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
    assert_eq!(u32::test(), 1);
    assert_eq!(i8::test(), 0);
    assert_eq!(usize::zero(), 0);
    assert_eq!(<u128 as Foo<u128>>::foo(), 1);
}