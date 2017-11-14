use std::rc::Rc;
use ast::typed::{Mutability, Signedness, Size, Type, TypeVar};
use super::rollback_map::Map;


#[derive(Debug, Clone)]
enum VarTarget {
    Type(Type),
    UnsizedInt(Signedness),
    Int,
}

pub struct UnifyTable {
    var_target: Map<TypeVar, VarTarget>,
    next_var: u32,
}

impl UnifyTable {
    pub fn new() -> UnifyTable {
        UnifyTable {
            var_target: Map::new(),
            next_var: 0,
        }
    }

    pub fn fresh_var(&mut self) -> TypeVar {
        let var = TypeVar(self.next_var);
        self.next_var += 1;
        var
    }

    pub fn fresh_int_var(&mut self, sign: Option<Signedness>) -> TypeVar {
        let var = TypeVar(self.next_var);
        match sign {
            Some(sign) => self.var_target.insert(var, VarTarget::UnsizedInt(sign)),
            None => self.var_target.insert(var, VarTarget::Int),
        }
        self.var_target.commit();
        self.next_var += 1;
        var
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<Type, ()> {
        match self.unify_raw(a, b, true) {
            Ok(()) => {
                self.commit();
                // `a` might get coerced to `b`, so we return `b` as result
                Ok(b.clone())
            }
            Err(()) => {
                self.rollback();
                Err(())
            }
        }
    }

    fn unify_raw(&mut self, a: &Type, b: &Type, allow_coerce: bool) -> Result<(), ()> {
        let a = self.shallow_normalize(a);
        let b = self.shallow_normalize(b);
        match (a, b) {
            (Type::Concrete(a, ref ap), Type::Concrete(b, ref bp)) => if a == b {
                assert_eq!(ap.len(), bp.len());
                for (a, b) in ap.iter().zip(bp.iter()) {
                    self.unify_raw(a, b, false)?;
                }
                Ok(())
            } else {
                Err(())
            },
            (Type::Function(ref ap, ref a), Type::Function(ref bp, ref b)) => {
                if ap.len() == bp.len() {
                    for (a, b) in ap.iter().zip(bp.iter()) {
                        // flip `a` and `b` when unifying because fns are
                        // contravariant on their arguments - so we can only
                        // coerce types backwards.
                        self.unify_raw(b, a, true)?;
                    }
                    self.unify_raw(a, b, true)
                } else {
                    Err(())
                }
            }
            (Type::Int(sign1, size1), Type::Int(sign2, size2)) => {
                if sign1 == sign2 && size1 == size2 {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (Type::Pointer(m1, ref a), Type::Pointer(m2, ref b)) if m1 == m2 => {
                self.unify_raw(a, b, false)
            }
            (Type::Pointer(Mutability::Mut, ref a), Type::Pointer(Mutability::Const, ref b))
            if allow_coerce => {
                self.unify_raw(a, b, false)
            }
            (Type::Var(a), ty) | (ty, Type::Var(a)) => self.unify_var_type(a, ty),
            (Type::Bool, Type::Bool) |
            (Type::Unit, Type::Unit) |
            (Type::Error, _) | (_, Type::Error) => Ok(()),
            (_, _) => Err(()),
        }
    }

    fn unify_var_var(&mut self, a: TypeVar, b: TypeVar) -> Result<(), ()> {
        let at = self.var_target.get(&a).cloned();
        let bt = self.var_target.get(&b).cloned();
        match (at, bt) {
            (Some(VarTarget::Type(_)), _) | (_, Some(VarTarget::Type(_))) => {
                panic!("cannot unify non-normalized var")
            }
            (None, _) | (Some(VarTarget::Int), Some(_)) => {
                self.var_target.insert(a, VarTarget::Type(Type::Var(b)));
                Ok(())
            }
            (_, None) | (Some(_), Some(VarTarget::Int)) => {
                self.var_target.insert(b, VarTarget::Type(Type::Var(a)));
                Ok(())
            }
            (Some(VarTarget::UnsizedInt(s1)), Some(VarTarget::UnsizedInt(s2))) => if s1 == s2 {
                self.var_target.insert(a, VarTarget::Type(Type::Var(b)));
                Ok(())
            } else {
                Err(())
            },
        }
    }

    fn unify_var_type(&mut self, v: TypeVar, b: Type) -> Result<(), ()> {
        match b {
            Type::Var(b) if b == v => return Ok(()),
            _ => {}
        }
        if self.occurs(v, &b) {
            return Err(());
        }
        let vt = self.var_target.get(&v).cloned();
        match (vt, b) {
            (_, Type::Error) => {
                self.var_target.insert(v, VarTarget::Type(Type::Error));
                Ok(())
            }
            (Some(VarTarget::Type(_)), _) => panic!("cannot unify non-normalized var"),
            (_, Type::Var(b)) => self.unify_var_var(v, b),
            (None, ty) | (Some(VarTarget::Int), ty @ Type::Int(_, _)) => {
                self.var_target.insert(v, VarTarget::Type(ty));
                Ok(())
            }
            (Some(VarTarget::UnsizedInt(sign)), Type::Int(sign2, size)) => if sign == sign2 {
                self.var_target
                    .insert(v, VarTarget::Type(Type::Int(sign2, size)));
                Ok(())
            } else {
                Err(())
            },
            _ => Err(()),
        }
    }

    fn occurs(&mut self, var: TypeVar, typ: &Type) -> bool {
        let typ = self.shallow_normalize(typ);
        match typ {
            Type::Bool | Type::Error | Type::Int(_, _) | Type::Unit => false,
            Type::Concrete(_, ref params) => {
                for param in params.iter() {
                    if self.occurs(var, param) {
                        return true;
                    }
                }
                false
            }
            Type::Function(ref params, ref out) => {
                for param in params.iter() {
                    if self.occurs(var, param) {
                        return true;
                    }
                }
                self.occurs(var, out)
            }
            Type::Pointer(_, ref to) => self.occurs(var, to),
            Type::Var(v) => var == v,
        }
    }

    fn get_var_type(&self, var: TypeVar) -> Type {
        match self.var_target.get(&var) {
            Some(&VarTarget::Type(ref ty)) => self.shallow_normalize(ty),
            Some(&VarTarget::Int) | Some(&VarTarget::UnsizedInt(_)) | None => Type::Var(var),
        }
    }

    pub fn shallow_normalize(&self, a: &Type) -> Type {
        match a {
            &Type::Var(var) => self.get_var_type(var),
            ty => ty.clone(),
        }
    }

    pub fn normalize(&mut self, a: &Type) -> Result<Type, ()> {
        match self.shallow_normalize(a) {
            Type::Bool => Ok(Type::Bool),
            Type::Unit => Ok(Type::Unit),
            Type::Concrete(sym, ref params) => {
                let mut normalized = Vec::new();
                for param in &**params {
                    normalized.push(self.normalize(param)?);
                }
                Ok(Type::Concrete(sym, normalized.into()))
            }
            Type::Error => Ok(Type::Error),
            Type::Function(ref params, ref out) => {
                let out = self.normalize(out)?;
                let mut normalized = Vec::new();
                for param in &**params {
                    normalized.push(self.normalize(param)?);
                }
                Ok(Type::Function(normalized.into(), Rc::new(out)))
            }
            ty @ Type::Int(_, _) => Ok(ty),
            Type::Pointer(mutability, ref ty) => {
                let ty = self.normalize(ty)?;
                Ok(Type::Pointer(mutability, Rc::new(ty)))
            }
            Type::Var(var) => {
                match self.var_target.get(&var) {
                    Some(&VarTarget::Type(_)) => panic!("var not normalized"),
                    Some(&VarTarget::Int) => {
                        return Ok(Type::Int(Signedness::Signed, Size::Bit32));
                    }
                    Some(&VarTarget::UnsizedInt(sign)) => {
                        return Ok(Type::Int(sign, Size::Bit32));
                    }
                    None => {
                        // there is no sensible default
                        // fall though because of borrowck
                    }
                }
                // unify this var with an error to prevent further
                // error reports about same variable
                self.var_target.insert(var, VarTarget::Type(Type::Error));
                Err(())
            }
        }
    }

    pub fn describe_var(&self, var: TypeVar) -> &'static str {
        match self.var_target.get(&var) {
            Some(&VarTarget::Int) => "{int}",
            Some(&VarTarget::UnsizedInt(Signedness::Signed)) => "{signed}",
            Some(&VarTarget::UnsizedInt(Signedness::Unsigned)) => "{unsigned}",
            _ => "_",
        }
    }

    fn commit(&mut self) {
        self.var_target.commit();
    }

    fn rollback(&mut self) {
        self.var_target.rollback();
    }
}
