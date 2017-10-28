use std::collections::HashMap;
use ast::cfg::{Size, Symbol, Type};
use ast::typed::Struct;


const POINTER_SIZE: u32 = ::plank_ir::ir::POINTER_SIZE;
const FUNCTION_SIZE: u32 = ::plank_ir::ir::FUNCTION_SIZE;

pub struct LayoutEngine<'a> {
    structs: &'a HashMap<Symbol, Struct>,
}

impl<'a> LayoutEngine<'a> {
    pub fn new(structs: &'a HashMap<Symbol, Struct>) -> Self {
        LayoutEngine { structs }
    }

    #[allow(dead_code)]
    pub fn size_of(&self, ty: &Type) -> Option<u32> {
        self.size_align(ty).map(|(size, _)| size)
    }

    #[allow(dead_code)]
    pub fn align_of(&self, ty: &Type) -> Option<u32> {
        self.size_align(ty).map(|(_, align)| align)
    }

    pub fn size_align(&self, ty: &Type) -> Option<(u32, u32)> {
        match *ty {
            Type::Bool => Some((1, 1)),
            Type::Error => None,
            Type::Pointer(_) => Some((POINTER_SIZE, POINTER_SIZE)),
            Type::Function(_, _) => Some((FUNCTION_SIZE, FUNCTION_SIZE)),
            Type::Int(_, Size::Bit8) => Some((1, 1)),
            Type::Int(_, Size::Bit16) => Some((2, 2)),
            Type::Int(_, Size::Bit32) => Some((4, 4)),
            Type::Var(_) => None,
            Type::Concrete(sym, ref params) => {
                let s = &self.structs[&sym];
                debug_assert_eq!(params.len(), s.type_params.len());
                let mapping = s.type_params
                    .iter()
                    .cloned()
                    .zip(params.iter().cloned())
                    .collect();
                s.fields
                    .iter()
                    .map(|field| field.typ.replace(&mapping))
                    .fold(Some((0, 1)), |a, b| match (a, b) {
                        (Some((s, a)), ty) => match self.size_align(&ty) {
                            Some((s2, a2)) => {
                                let s = (s + a2 - 1) / a2 * a2 + s2;
                                let a = lcm(a, a2);
                                Some((s, a))
                            }
                            _ => None,
                        },
                        _ => None,
                    })
                    .map(|(s, a)| {
                        let s = (s + a - 1) / a * a;
                        (s, a)
                    })
            }
        }
    }

    pub fn field_info(&self, ty: &Type, field: usize) -> (u32, Type) {
        match *ty {
            Type::Bool |
            Type::Error |
            Type::Pointer(_) |
            Type::Function(_, _) |
            Type::Int(_, _) |
            Type::Var(_) => panic!("no fields on type"),
            Type::Concrete(sym, ref params) => {
                let s = &self.structs[&sym];
                debug_assert_eq!(params.len(), s.type_params.len());
                let mapping = s.type_params
                    .iter()
                    .cloned()
                    .zip(params.iter().cloned())
                    .collect();
                debug_assert!(field < s.fields.len());
                let (size, last_size) = s.fields
                    .iter()
                    .take(field + 1)
                    .map(|field| field.typ.replace(&mapping))
                    .fold((0, 0), |(s, _), ty| {
                        let (s2, a2) = self.size_align(&ty).unwrap();
                        let s = (s + a2 - 1) / a2 * a2 + s2;
                        (s, s2)
                    });
                let offset = size - last_size;
                let typ = s.fields[field].typ.clone();
                (offset, typ)
            }
        }
    }
}

fn lcm(a: u32, b: u32) -> u32 {
    fn gcd(mut a: u32, mut b: u32) -> u32 {
        while b > 0 {
            a %= b;
            ::std::mem::swap(&mut a, &mut b);
        }
        a
    }
    a * b / gcd(a, b)
}
