#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(test)]
use alloc::vec;
use alloc::{
    borrow::{Cow, ToOwned},
    collections::*,
    ffi::CString,
    rc::Rc,
    string::String,
    sync::Arc,
    vec::Vec,
};
use core::{
    cmp,
    marker::{PhantomData, PhantomPinned},
    mem::MaybeUninit,
    num::*,
    ptr::NonNull,
    task::Poll,
};
#[cfg(feature = "std")]
use std::{collections::*, ffi::OsString, path::PathBuf};

/// Simply copy self, or replace self with a simple instance
pub trait StdMove: Sized {
    /// Simply copy self, or replace self with a simple instance
    #[must_use]
    fn std_move(&mut self) -> Self;
}

/// [`Clone`] like impl macro
#[macro_export]
macro_rules! impl_trivial {
    (@gen($self:ident)) => { $self.clone() };
    (@gen($self:ident) {$($b:tt)*}) => {{ $($b)* }};
    ($(
        $(#[$meta:meta])*
        $ty:ty $([$($g:tt)*])? $({$($b:tt)*})?
    ),+ $(,)?) => {
        $(
            $(#[$meta])*
            impl$(<$($g)*>)? $crate::StdMove for $ty {
                fn std_move(&mut self) -> Self {
                    $crate::impl_trivial!(@gen(self) $({$($b)*})?)
                }
            }
        )+
    };
}
/// [`take`] like impl macro
///
/// [`take`]: core::mem::take
#[macro_export]
macro_rules! impl_take {
    (@gen($self:ident)) => { ::core::mem::take($self) };
    (@gen($self:ident) {$($b:tt)*}) => {{ $($b)* }};
    ($(
        $(#[$meta:meta])*
        $ty:ty $([$($g:tt)*])? $({$($b:tt)*})?
    ),+ $(,)?) => {
        $(
            $(#[$meta])*
            impl$(<$($g)*>)? $crate::StdMove for $ty {
                fn std_move(&mut self) -> Self {
                    $crate::impl_take!(@gen(self) $({$($b)*})?)
                }
            }
        )+
    };
}
macro_rules! impl_tuples {
    () => {
        impl_tuples! { () T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 }
    };
    (() $cur:ident $($rest:ident)*) => {
        impl_tuples! {
            @impl
            /// This trait is implemented for tuples up to 12 items long.
            $cur
        }
        impl_tuples! { ($cur) $($rest)* }
    };
    (($($c:ident)*)) => {};
    (($($c:ident)*) $cur:ident $($rest:ident)*) => {
        impl_tuples! {
            @impl
            #[doc(hidden)]
            $($c)* $cur
        }
        impl_tuples! { ($($c)* $cur) $($rest)* }
    };
    (@impl $(#[$meta:meta])* $($i:ident)+) => {
        $(#[$meta])*
        impl<$($i),+> StdMove for ($($i,)+)
        where $($i: StdMove,)+
        {
            fn std_move(&mut self) -> Self {
                #[allow(non_snake_case)]
                let ($($i,)+) = self;
                ($($i.std_move(),)+)
            }
        }
    };
}

impl<T: StdMove, const N: usize> StdMove for [T; N] {
    fn std_move(&mut self) -> Self {
        self.each_mut().map(StdMove::std_move)
    }
}
impl<T: StdMove> StdMove for cmp::Reverse<T> {
    fn std_move(&mut self) -> Self {
        Self(self.0.std_move())
    }
}
impl<T: ToOwned> StdMove for Cow<'_, T> where T::Owned: StdMove {
    fn std_move(&mut self) -> Self {
        match self {
            Cow::Borrowed(x) => Self::Borrowed(*x),
            Cow::Owned(x) => Self::Owned(x.std_move()),
        }
    }
}
impl<T> StdMove for MaybeUninit<T> {
    fn std_move(&mut self) -> Self {
        unsafe {
            // SAFETY: MaybeUninit 并不会运行析构函数, 使用哪个实例由用户决定
            core::ptr::read(self)
        }
    }
}

impl_tuples!();
impl_trivial! {
    (),
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    u128,
    f32,
    f64,
    usize,
    bool,
    char,
    NonZeroI8,
    NonZeroI16,
    NonZeroI32,
    NonZeroI64,
    NonZeroI128,
    NonZeroIsize,
    NonZeroU8,
    NonZeroU16,
    NonZeroU32,
    NonZeroU64,
    NonZeroU128,
    NonZeroUsize,
    PhantomData<T> [T: ?Sized],
    PhantomPinned,
    &T [T: ?Sized],
    *mut T [T: ?Sized],
    *const T [T: ?Sized],
    NonNull<T> [T: ?Sized],
    cmp::Ordering,
    core::sync::atomic::Ordering,
    Rc<T> [T],
    Arc<T> [T],
}
impl_take! {
    Vec<T> [T],
    VecDeque<T> [T],
    LinkedList<T> [T],
    BTreeSet<T> [T],
    BTreeMap<K, V> [K, V],
    BinaryHeap<T> [T: Ord],
    Option<T> [T],
    String,
    CString,
    Poll<T> [T] { Poll::Pending },
}
#[cfg(feature = "std")]
impl_take! {
    HashSet<T, S> [T, S: Default],
    HashMap<K, V, S> [K, V, S: Default],
    OsString,
    PathBuf,
}

/// Like C++ `std::move` use `&mut impl StdMove`
///
/// # Examples
///
/// Move resource types
///
/// ```
/// # use std_move::r#move;
/// let mut a = vec![1, 2, 3];
/// let b;
/// b = r#move!(a);
/// assert_eq!(a, []);
/// assert_eq!(b, [1, 2, 3]);
/// ```
///
/// Trivial types
///
/// ```
/// # use std_move::r#move;
/// let mut n = 3;
/// assert_eq!(r#move!(n), 3);
/// assert_eq!(r#move!(n), 3);
/// assert_eq!(n, 3);
/// ```
#[macro_export]
macro_rules! r#move {
    ($e:expr) => {
        $crate::StdMove::std_move(&mut $e)
    };
}

#[test]
fn it_works() {
    let mut a = vec![1, 2, 3];
    let b;
    b = r#move!(a);
    assert_eq!(a, []);
    assert_eq!(b, [1, 2, 3]);

    let mut n = 3;
    assert_eq!(r#move!(n), 3);
    assert_eq!(r#move!(n), 3);
    assert_eq!(n, 3);
}

#[test]
fn move_maybe_uninit() {
    let mut a = MaybeUninit::new(vec![1, 2, 3]);
    let b = r#move!(a);
    assert_eq!(unsafe { b.assume_init() }, [1, 2, 3]);
}
