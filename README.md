# traitlit
This crate defines an attribute called `lit`. This attribute converts a single implementation of a trait to seperate implementations for the specified types.

Let's say I want to define a `Zero` trait using lit.
```rust
pub trait Zero {
    fn zero() -> Self;
}
```
Normaly this would be achived by either manually implementing the trait for the desired types, or by creating a macro to produce a specific set of implementations. Using `lit` we can simply implement it as follows.
```rust
#[lit(usize, isize)]
impl Zero for _ {
    fn zero() -> Self {
        0
    }
}
```
This generates two implementations, one for `usize` and one for `isize`. This is very repetitive, as most such traits needs to be implemented by every type in a specific category. To this end `lit` allows you to write `u_` instead of `u8`, `u16`, `u32`, `u64`, `u128`, and `usize`, aswell as `i_` for the corosponding signed types. `lit` also accepts `f_` which corrosponds to `f32` and `f64`. With this the previous example becomes:
```rust
#[lit(u_, i_)]
impl Zero for _ {
    fn zero() -> Self {
        0
    }
}
```
The current implementation would however not work for float types, as `0` is not a float literal. This can be easily remedied using type casting.
```rust
#[lit(u_, i_, f_)]
impl Zero for _ {
    fn zero() -> Self {
        0 as Self
    }
}
```
To simplify the implementation further, we can combine the trait definition and implementation:
```rust
#[lit(u_, i_, f_)]
trait Zero {
    fn zero() -> Self {
        0 as Self
    }
}
```
This will however assume that all implementations in the trait are part of the individual implementations rather than a default. If you wish to have defaults in your trait, keep the trait and implementation seperate.

You may have noticed the `_` in the `impl` block. As `_` isn't normaly allowed anywhere in this line, all `_` are replaced by the type being implemented.