use std::marker::PhantomData;


pub trait Nat {}

pub struct Z;
pub struct S<N: Nat>(PhantomData<N>);

impl Nat for Z {}
impl<N: Nat> Nat for S<N> {}
