Like C++ `std::move` use `&mut impl StdMove`

# Examples

Move resource types

```rust
use std_move::r#move;

let mut a = vec![1, 2, 3];
let b;
b = r#move!(a);
assert_eq!(a, []);
assert_eq!(b, [1, 2, 3]);
```

Trivial types

```rust
use std_move::r#move;

let mut n = 3;
assert_eq!(r#move!(n), 3);
assert_eq!(r#move!(n), 3);
assert_eq!(n, 3);
```
