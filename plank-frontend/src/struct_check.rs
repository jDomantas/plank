use std::collections::{HashMap, HashSet};
use ast::resolved::{Program, Symbol, Type};
use CompileCtx;


type Pair = (Symbol, Symbol);

struct Solver {
    node_index: HashMap<Pair, u32>,
    edges: HashMap<u32, HashSet<u32>>,
    needed_visits: HashMap<u32, u32>,
    next_node_index: u32,
    visit_queue: Vec<u32>,
}

impl Solver {
    fn new(symbols: &HashSet<Symbol>) -> Solver {
        let nodes = (symbols.len() * symbols.len()) as u32;
        let indices = symbols
            .iter()
            .flat_map(|&a| symbols
                .iter()
                .map(move |&b| (a, b)))
            .enumerate()
            .map(|(index, pair)| (pair, index as u32))
            .collect();
        let needed_visits = (0..nodes)
            .into_iter()
            .map(|i| (i, 1))
            .collect();
        let mut solver = Solver {
            node_index: indices,
            edges: HashMap::new(),
            needed_visits,
            next_node_index: nodes,
            visit_queue: Vec::new(),
        };
        for &a in symbols {
            for &b in symbols {
                for &c in symbols {
                    let first = solver.concrete_node(a, b);
                    let second = solver.concrete_node(b, c);
                    let result = solver.concrete_node(a, c);
                    let node = solver.artificial_node();
                    solver.add_edge(first, node);
                    solver.add_edge(second, node);
                    solver.add_edge(node, result);
                    solver.needed_visits.insert(node, 2);
                }
            }
        }
        solver
    }

    fn concrete_node(&self, a: Symbol, b: Symbol) -> u32 {
        self.node_index[&(a, b)]
    }

    fn add_edge(&mut self, from: u32, to: u32) {
        self.edges.entry(from).or_insert_with(HashSet::new).insert(to);
    }

    fn artificial_node(&mut self) -> u32 {
        self.next_node_index += 1;
        self.next_node_index - 1
    }

    fn add_fact(&mut self, fact: Pair) {
        let node = self.concrete_node(fact.0, fact.1);
        if self.needed_visits[&node] != 0 {
            self.visit_queue.push(node);
            self.needed_visits.insert(node, 0);
        }
    }

    fn add_rule<I: ExactSizeIterator<Item=Pair>>(&mut self, bounds: I, result: Pair) {
        if bounds.len() == 0 {
            self.add_fact(result);
        } else {
            let node = self.artificial_node();
            self.needed_visits.insert(node, bounds.len() as u32);
            let result = self.concrete_node(result.0, result.1);
            self.add_edge(node, result);
            for (a, b) in bounds {
                let condition = self.concrete_node(a, b);
                self.add_edge(condition, node);
            }
        }
    }

    fn solve(&mut self) {
        while let Some(node) = self.visit_queue.pop() {
            if let Some(outgoing) = self.edges.get(&node) {
                for &out in outgoing {
                    match self.needed_visits[&out] {
                        0 => {}
                        1 => {
                            self.needed_visits.insert(out, 0);
                            self.visit_queue.push(out);
                        }
                        x => {
                            self.needed_visits.insert(out, x - 1);
                        }
                    }
                }
            }
        }
    }

    fn is_recursive(&self, sym: Symbol) -> bool {
        let node = self.concrete_node(sym, sym);
        self.needed_visits[&node] == 0
    }

    fn add_struct(&mut self, program: &Program, root: Symbol, typ: &Type, acc: &mut HashSet<Pair>) {
        match *typ {
            Type::Wildcard |
            Type::I8 |
            Type::U8 |
            Type::I16 |
            Type::U16 |
            Type::I32 |
            Type::U32 |
            Type::Bool |
            Type::Unit |
            Type::Pointer(_) |
            Type::Function(_, _) |
            Type::Error => {}
            Type::Concrete(sym, ref params) => {
                let sym = *sym;
                self.add_rule(acc.iter().cloned(), (root, sym));
                if !params.is_empty() {
                    let symbols = &program.structs[&sym].name.type_params;
                    debug_assert_eq!(params.len(), symbols.len());
                    for (typ, &p) in params.iter().zip(symbols.iter()) {
                        let new_pair = (sym, *p);
                        if acc.contains(&new_pair) {
                            self.add_struct(program, root, typ, acc);
                        } else {
                            acc.insert(new_pair);
                            self.add_struct(program, root, typ, acc);
                            acc.remove(&new_pair);
                        }
                    }
                }
            }
        }
    }
}

pub(crate) fn check_program(program: &mut Program, ctx: &mut CompileCtx) {
    let symbols = program.structs
        .values()
        .flat_map(|s| s.name.type_params.iter().map(|x| **x))
        .chain(program.structs.keys().cloned())
        .collect::<Vec<_>>();
    let symbols_hashset = symbols.iter().cloned().collect::<HashSet<_>>();
    debug_assert_eq!(symbols.len(), symbols_hashset.len());
    let mut solver = Solver::new(&symbols_hashset);
    let mut acc = HashSet::new();
    for (&name, s) in &program.structs {
        for var in &s.fields {
            solver.add_struct(program, name, &var.typ, &mut acc);
        }
    }
    solver.solve();
    for (&name, s) in &mut program.structs {
        if solver.is_recursive(name) {
            let msg = format!(
                "struct `{}` is recursive",
                ctx.symbols.get_name(name),
            );
            ctx.reporter
                .error(msg, s.complete_span)
                .span(s.complete_span)
                .build();
            for var in &mut s.fields {
                *var.typ = Type::Error;
            }
        }
    }
}
