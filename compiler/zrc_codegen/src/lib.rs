use anyhow::bail;
use anyhow::Context as _;
use std::{collections::HashMap, fmt::Display};

#[derive(PartialEq, Debug, Clone)]
struct Counter {
    value: usize,
}
impl Counter {
    fn new(initial_value: usize) -> Self {
        Counter {
            value: initial_value,
        }
    }

    fn next(&mut self) -> usize {
        self.value += 1;
        self.value - 1
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCg {
    name: String,
    parameters: Vec<(String, zrc_typeck::tast::ty::Type)>,
    ret: zrc_typeck::BlockReturnType,
    blocks: Vec<BasicBlockData>,
    next_instruction_id: Counter,
    /// One of our design decisions involves using memory registers extremely often, and `alloca`s can only
    /// be easily optimized to SSA registers by SROA if they are located at the beginning of the first basic block
    /// in the function. For this reason, the allocas are located here.
    allocations: Vec<String>,
}
impl FunctionCg {
    pub fn new(
        name: String,
        ret: zrc_typeck::BlockReturnType,
        parameters: Vec<(String, zrc_typeck::tast::ty::Type)>,
        parent_scope: &CgScope,
    ) -> (Self, BasicBlock, CgScope) {
        let mut scope = parent_scope.clone();
        let mut cg = Self {
            blocks: vec![BasicBlockData::new(0)],
            next_instruction_id: Counter::new(1),
            allocations: Vec::new(),
            name,
            ret,
            parameters: parameters
                .clone()
                .into_iter()
                .enumerate()
                .map(|(i, (_, ty))| (format!("%p{i}"), ty))
                .collect(),
        };
        let bb = BasicBlock { id: 0 };

        for (id, parameter) in parameters.into_iter().enumerate() {
            // promote all of the parameters passed by value into allocas
            let ptr = cg_alloc(&mut cg, &bb, &get_llvm_typename(parameter.1.clone()));

            // store the parameter into the alloca
            cg_store(
                &mut cg,
                &bb,
                &get_llvm_typename(parameter.1),
                &ptr,
                &format!("%p{id}"),
            );

            // insert the parameter into the scope
            scope.insert(&parameter.0, &ptr);
        }

        (cg, bb, scope)
    }

    fn new_bb(&mut self) -> BasicBlock {
        let id = self.blocks.len();
        self.blocks.push(BasicBlockData {
            id,
            instructions: Vec::new(),
        });
        BasicBlock { id }
    }

    fn new_reg(&mut self) -> String {
        format!("%l{}", self.next_instruction_id.next())
    }

    fn add_instruction_to_bb(&mut self, bb: &BasicBlock, instr: &str) -> anyhow::Result<()> {
        let bb = self.blocks.get_mut(bb.id).context("Invalid BB ID")?;
        bb.instructions.push(instr.to_string());

        Ok(())
    }
}

impl Display for FunctionCg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "define {} {}({}) {{",
            get_llvm_typename(self.ret.clone().into_tast_type()),
            self.name,
            self.parameters
                .iter()
                .map(|(name, ty)| format!("{ty} {name}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        for (i, bb) in self.blocks.iter().enumerate() {
            if i == 0 {
                for instr in &self.allocations {
                    writeln!(f, "    {}", instr)?;
                }
                for instr in &bb.instructions {
                    writeln!(f, "    {}", instr)?;
                }
            } else {
                writeln!(f, "bb{}:", bb.id)?;
                for instr in &bb.instructions {
                    writeln!(f, "    {}", instr)?;
                }
            }
        }
        writeln!(f, "}}");
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
struct BasicBlockData {
    id: usize,
    instructions: Vec<String>,
}
impl BasicBlockData {
    fn new(id: usize) -> Self {
        BasicBlockData {
            id,
            instructions: Vec::new(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BasicBlock {
    id: usize,
}
impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%bb{}", self.id)
    }
}
impl BasicBlock {
    fn add_instruction(&self, cg: &mut FunctionCg, instr: &str) -> anyhow::Result<()> {
        cg.add_instruction_to_bb(self, instr)
    }
}

/// Determine the order of the values within a struct.
///
/// FIXME: This MUST use insertion order in the future for C FFI compat, instead of our.. alphabetical order approach?
fn determine_order_of_struct(
    values: HashMap<String, zrc_typeck::tast::ty::Type>,
) -> Vec<(String, zrc_typeck::tast::ty::Type)> {
    let mut values = values.into_iter().collect::<Vec<_>>();
    values.sort_by_key(|(k, _)| k.clone());
    values
}

fn get_llvm_typename(ty: zrc_typeck::tast::ty::Type) -> String {
    use zrc_typeck::tast::ty::Type;

    match ty {
        Type::I8 | Type::U8 => "i8".to_string(),
        Type::I16 | Type::U16 => "i16".to_string(),
        Type::I32 | Type::U32 => "i32".to_string(),
        Type::I64 | Type::U64 => "u64".to_string(),
        Type::Void => "void".to_string(),
        Type::Bool => "i1".to_string(),
        Type::Ptr(_) => "ptr".to_string(),
        Type::Fn(params, ret) => format!(
            "{} ({})*",
            get_llvm_typename(ret.into_tast_type()),
            params
                .into_iter()
                .map(get_llvm_typename)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Struct(entries) => format!(
            "{{ {} }}",
            determine_order_of_struct(entries)
                .into_iter()
                .map(|(_, v)| format!("{v}"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// Allocates a new register and returns it. This also properly handles the alloca instruction needing
/// to be at the beginning of a basic block.
fn cg_alloc(cg: &mut FunctionCg, _bb: &BasicBlock, ty: &str) -> String {
    let reg = cg.new_reg();
    cg.allocations.push(format!("{reg} = alloca {ty}"));
    reg
}
/// Loads a type `T` from a `T*`
fn cg_load(cg: &mut FunctionCg, bb: &BasicBlock, ty: &str, ptr: &str) -> String {
    let reg = cg.new_reg();
    bb.add_instruction(cg, &format!("{reg} = load {ty}, ptr {ptr}"));
    reg
}
/// Stores a type `T` to a `T*`
fn cg_store(cg: &mut FunctionCg, bb: &BasicBlock, ty: &str, ptr: &str, value: &str) {
    bb.add_instruction(cg, &format!("store {ty} {value}, ptr {ptr}"));
}

#[derive(PartialEq, Clone, Debug)]
pub struct CgScope {
    /// Maps identifiers to their LLVM register
    identifiers: HashMap<String, String>,
}
impl CgScope {
    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &str) -> Option<&String> {
        self.identifiers.get(ident)
    }

    pub fn insert(&mut self, ident: &str, reg: &str) {
        self.identifiers.insert(ident.to_string(), reg.to_string());
    }
}

/// Returns a register containing a pointer to the place inputted
pub fn cg_place(
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    place: zrc_typeck::tast::expr::Place,
) -> anyhow::Result<(String, BasicBlock)> {
    // Again, produces a REGISTER holding the pointer.

    use zrc_typeck::tast::expr::PlaceKind;

    Ok(match place.1 {
        PlaceKind::Variable(x) => {
            let reg = scope
                .get(&x)
                .with_context(|| format!("Identifier {} not found in scope", x))?
                .clone();
            (reg, bb.clone())
        }
        PlaceKind::Deref(x) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;
            (x_ptr, bb)
        }
        PlaceKind::Index(x, index) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x.clone())?;
            let (index_ptr, bb) = cg_expr(cg, &bb, scope, *index.clone())?;

            let x_typename = get_llvm_typename(x.clone().0);

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let index_typename = get_llvm_typename(index.0.clone());

            let index_reg = cg_load(cg, &bb, &index_typename, &index_ptr);

            let result_typename = match x.0 {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, {index_typename} {index_reg}",
                )
            );

            (result_reg, bb)
        }
        PlaceKind::Arrow(x, key) => {
            let (x_ptr, bb) = cg_place(cg, bb, scope, *x.clone())?;

            let x_typename = get_llvm_typename(x.0.clone());

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let result_typename = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    determine_order_of_struct(entries)
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &key)
                        .with_context(|| format!("Struct {} has no field {}", x.0, key))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, i32 0, i32 0, i32 {key_idx}",
                )
            );

            (result_reg, bb)
        }
        PlaceKind::Dot(x, key) => {
            let (x_ptr, bb) = cg_place(cg, bb, scope, *x.clone())?;

            let x_typename = get_llvm_typename(x.0.clone());

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let result_typename = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => get_llvm_typename(
                    entries
                        .get(&key)
                        .with_context(|| format!("Struct {} has no field {}", x.0.clone(), key))?
                        .clone(),
                ),
                _ => unreachable!(), // per typeck, this is always a struct
            };

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    determine_order_of_struct(entries)
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &key)
                        .with_context(|| format!("Struct {} has no field {}", x.0, key))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, i32 0, i32 {key_idx}",
                )
            );

            (result_reg, bb)
        }
    })
}

/// Returns a register containing a pointer to the result of the expression.
pub fn cg_expr(
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &CgScope,
    expr: zrc_typeck::tast::expr::TypedExpr,
) -> anyhow::Result<(String, BasicBlock)> {
    use zrc_typeck::tast::{expr::TypedExprKind, ty::Type};
    // Remember you must ALWAYS return a POINTER register. This is an intentional design choice for now. llvm can optimize away the pointer if it wants to.

    Ok(match expr.1 {
        TypedExprKind::Ternary(condition, lhs, rhs) => {
            let (condition_ptr, bb) = cg_expr(cg, bb, scope, *condition)?;
            // condition_ptr: i1*

            let result_typename = get_llvm_typename(expr.0.clone());

            let if_true_bb = cg.new_bb();
            let if_false_bb = cg.new_bb();

            // load the condition
            let condition_reg = cg_load(cg, &bb, "i1", &condition_ptr);
            // and branch on it
            bb.add_instruction(
                cg,
                &format!("br i1 {condition_reg}, label {if_true_bb}, label {if_false_bb}"),
            );

            // generate the expressions on both sides
            let (if_true_ptr, if_true_bb) = cg_expr(cg, &if_true_bb, scope, *lhs)?;
            let (if_false_ptr, if_false_bb) = cg_expr(cg, &if_false_bb, scope, *rhs)?;
            // if_true_ptr is a pointer accessible on the true side and vice versa

            let terminating_bb = cg.new_bb();

            // branch to the terminating bb
            if_true_bb.add_instruction(cg, &format!("br label {terminating_bb}"));
            if_false_bb.add_instruction(cg, &format!("br label {terminating_bb}"));

            let result_ptr = cg.new_reg();

            // now generate a phi node
            terminating_bb.add_instruction(cg, &format!("{result_ptr} = phi ptr [{if_true_ptr}, {if_true_bb}], [{if_false_ptr}, {if_false_bb}]"));

            (result_ptr, terminating_bb)
        }

        TypedExprKind::NumberLiteral(n) => {
            let typename = get_llvm_typename(expr.0.clone());
            let reg = cg_alloc(cg, bb, &typename);
            cg_store(cg, &bb, &typename, &reg, &n);
            (reg, bb.clone())
        }
        TypedExprKind::BooleanLiteral(b) => {
            let typename = get_llvm_typename(expr.0.clone());
            let reg = cg_alloc(cg, bb, &typename);
            cg_store(cg, &bb, &typename, &reg, &b.to_string());
            (reg, bb.clone())
        }

        TypedExprKind::Comma(lhs, rhs) => {
            let (_, bb) = cg_expr(cg, bb, scope, *lhs)?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;
            (rhs_ptr, bb)
        }

        TypedExprKind::BinaryBitwise(op, lhs, rhs) => {
            let (lhs_ptr, bb) = cg_expr(cg, bb, scope, *lhs)?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;

            let result_typename = get_llvm_typename(expr.0.clone());
            let result_ptr = cg_alloc(cg, &bb, &result_typename);

            let lhs_reg = cg_load(cg, &bb, &result_typename, &lhs_ptr);
            let rhs_reg = cg_load(cg, &bb, &result_typename, &rhs_ptr);

            let op = match op {
                zrc_typeck::tast::expr::BinaryBitwise::And => "and",
                zrc_typeck::tast::expr::BinaryBitwise::Or => "or",
                zrc_typeck::tast::expr::BinaryBitwise::Xor => "xor",
                zrc_typeck::tast::expr::BinaryBitwise::Shl => "shl",
                zrc_typeck::tast::expr::BinaryBitwise::Shr => "lshr", // should it be ashr?
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_typename} {lhs_reg}, {rhs_reg}"),
            );
            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::Equality(op, lhs, rhs) => {
            // it's either *T == *U or iN == iN

            let (lhs_ptr, bb) = cg_expr(cg, bb, scope, *lhs.clone())?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;

            let operand_typename = get_llvm_typename(lhs.0.clone());

            let lhs_reg = cg_load(cg, &bb, &operand_typename, &lhs_ptr);
            let rhs_reg = cg_load(cg, &bb, &operand_typename, &rhs_ptr);

            let op = match op {
                zrc_typeck::tast::expr::Equality::Eq => "icmp eq",
                zrc_typeck::tast::expr::Equality::Neq => "icmp ne",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {operand_typename} {lhs_reg}, {rhs_reg}"),
            );

            let result_ptr = cg_alloc(cg, &bb, "i1");
            cg_store(cg, &bb, "i1", &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::Comparison(op, lhs, rhs) => {
            let (lhs_ptr, bb) = cg_expr(cg, bb, scope, *lhs.clone())?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;

            let result_typename = get_llvm_typename(lhs.0.clone());

            let lhs_reg = cg_load(cg, &bb, &result_typename, &lhs_ptr);
            let rhs_reg = cg_load(cg, &bb, &result_typename, &rhs_ptr);

            // the operands are integers
            let op = match (op, expr.0.clone()) {
                (zrc_typeck::tast::expr::Comparison::Lt, ty) if ty.is_signed_integer() => {
                    "icmp slt"
                }
                (zrc_typeck::tast::expr::Comparison::Gt, ty) if ty.is_signed_integer() => {
                    "icmp sgt"
                }
                (zrc_typeck::tast::expr::Comparison::Lte, ty) if ty.is_signed_integer() => {
                    "icmp sle"
                }
                (zrc_typeck::tast::expr::Comparison::Gte, ty) if ty.is_signed_integer() => {
                    "icmp sge"
                }

                (zrc_typeck::tast::expr::Comparison::Lt, _) => "icmp ult",
                (zrc_typeck::tast::expr::Comparison::Gt, _) => "icmp ugt",
                (zrc_typeck::tast::expr::Comparison::Lte, _) => "icmp ule",
                (zrc_typeck::tast::expr::Comparison::Gte, _) => "icmp uge",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_typename} {lhs_reg}, {rhs_reg}"),
            );

            let result_ptr = cg_alloc(cg, &bb, "i1");
            cg_store(cg, &bb, "i1", &result_ptr, &result_reg);

            (result_ptr, bb)
        }
        TypedExprKind::Arithmetic(op, lhs, rhs) => {
            let (lhs_ptr, bb) = cg_expr(cg, bb, scope, *lhs)?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let lhs_reg = cg_load(cg, &bb, &result_typename, &lhs_ptr);
            let rhs_reg = cg_load(cg, &bb, &result_typename, &rhs_ptr);

            let op = match op {
                zrc_typeck::tast::expr::Arithmetic::Addition => "add",
                zrc_typeck::tast::expr::Arithmetic::Subtraction => "sub",
                zrc_typeck::tast::expr::Arithmetic::Multiplication => "mul",
                zrc_typeck::tast::expr::Arithmetic::Division => "sdiv",
                zrc_typeck::tast::expr::Arithmetic::Modulo => "srem",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_typename} {lhs_reg}, {rhs_reg}"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);
            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }
        TypedExprKind::Logical(op, lhs, rhs) => {
            let (lhs_ptr, bb) = cg_expr(cg, bb, scope, *lhs)?;
            let (rhs_ptr, bb) = cg_expr(cg, &bb, scope, *rhs)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let lhs_reg = cg_load(cg, &bb, &result_typename, &lhs_ptr);
            let rhs_reg = cg_load(cg, &bb, &result_typename, &rhs_ptr);

            // "and/or i1" works for bools
            let op = match op {
                zrc_typeck::tast::expr::Logical::And => "and",
                zrc_typeck::tast::expr::Logical::Or => "or",
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {op} {result_typename} {lhs_reg}, {rhs_reg}"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);
            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::UnaryBitwiseNot(x) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let x_reg = cg_load(cg, &bb, &result_typename, &x_ptr);

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = xor {result_typename} {x_reg}, -1"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);
            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::UnaryNot(x) => {
            // x is bool
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let x_reg = cg_load(cg, &bb, &result_typename, &x_ptr);

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = xor {result_typename} {x_reg}, 1"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);

            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::UnaryMinus(x) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let x_reg = cg_load(cg, &bb, &result_typename, &x_ptr);

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = sub {result_typename} 0, {x_reg}"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);

            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        TypedExprKind::UnaryAddressOf(x) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            cg_alloc(cg, &bb, &result_typename);

            cg_store(cg, &bb, &result_typename, &result_reg, &x_ptr);

            (result_reg, bb)
        }

        TypedExprKind::UnaryDereference(x) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x)?;

            let result_typename = get_llvm_typename(expr.0.clone());

            let x_reg = cg_load(cg, &bb, &result_typename, &x_ptr);

            let result_ptr = cg_alloc(cg, &bb, &result_typename);

            cg_store(cg, &bb, &result_typename, &result_ptr, &x_reg);

            (result_ptr, bb)
        }

        TypedExprKind::Assignment(place, value) => {
            let (place_ptr, bb) = cg_place(cg, bb, scope, *place)?;
            let (value_ptr, bb) = cg_expr(cg, &bb, scope, *value.clone())?;

            let value_typename = get_llvm_typename(value.0.clone());

            let value_reg = cg_load(cg, &bb, &value_typename, &value_ptr);

            cg_store(cg, &bb, &value_typename, &place_ptr, &value_reg);

            (place_ptr, bb)
        }

        TypedExprKind::Identifier(id) => {
            let reg = scope
                .get(&id)
                .with_context(|| format!("Identifier {} not found in scope", id))?
                .clone();
            (reg, bb.clone())
        }

        TypedExprKind::Index(x, index) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x.clone())?;
            let (index_ptr, bb) = cg_expr(cg, &bb, scope, *index.clone())?;

            let x_typename = get_llvm_typename(x.clone().0);

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let index_typename = get_llvm_typename(index.0.clone());

            let index_reg = cg_load(cg, &bb, &index_typename, &index_ptr);

            let result_typename = match x.0 {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, {index_typename} {index_reg}",
                )
            );

            (result_reg, bb)
        }

        TypedExprKind::Dot(x, prop) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x.clone())?;

            let x_typename = get_llvm_typename(x.0.clone());

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let result_typename = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => get_llvm_typename(
                    entries
                        .get(&prop)
                        .with_context(|| format!("Struct {} has no field {}", x.0.clone(), prop))?
                        .clone(),
                ),
                _ => unreachable!(), // per typeck, this is always a struct
            };

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    determine_order_of_struct(entries)
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &prop)
                        .with_context(|| format!("Struct {} has no field {}", x.0, prop))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, i32 0, i32 {key_idx}",
                )
            );

            (result_reg, bb)
        }

        TypedExprKind::Arrow(x, prop) => {
            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x.clone())?;

            let x_typename = get_llvm_typename(x.0.clone());

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let result_typename = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Ptr(x) => get_llvm_typename(*x),
                _ => unreachable!(), // per typeck, this is always a pointer
            };

            let result_reg = cg.new_reg();

            let key_idx = match x.0.clone() {
                zrc_typeck::tast::ty::Type::Struct(entries) => {
                    determine_order_of_struct(entries)
                        .into_iter()
                        .enumerate()
                        .find(|(_, (k, _))| k == &prop)
                        .with_context(|| format!("Struct {} has no field {}", x.0.clone(), prop))?
                        .0
                }
                _ => unreachable!(), // per typeck, this is always a struct
            };

            bb.add_instruction(
                cg,
                &format!(
                    "{result_reg} = getelementptr {result_typename}, {x_typename} {x_reg}, i32 0, i32 0, i32 {key_idx}",
                )
            );

            (result_reg, bb)
        }

        TypedExprKind::Call(f, args) => {
            let (f_ptr, bb) = cg_expr(cg, bb, scope, *f.clone())?;

            let f_typename = get_llvm_typename(f.0.clone());

            let args = args
                .into_iter()
                .map(|arg| {
                    let (arg_ptr, bb) = cg_expr(cg, &bb, scope, arg.clone())?;
                    let arg_typename = get_llvm_typename(arg.0.clone());
                    let arg_reg = cg_load(cg, &bb, &arg_typename, &arg_ptr);
                    Ok(format!("{arg_typename} {arg_reg}"))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;

            let args = args.join(", ");

            let result_typename = get_llvm_typename(expr.0.clone());

            if result_typename == "void" {
                bb.add_instruction(cg, &format!("call {result_typename} {f_ptr}({args})"));

                let undef = cg_alloc(cg, &bb, "i8");

                // store the undefined value here as `void` results should never be loaded but our
                // code generator expects a loadable value
                cg_store(cg, &bb, "i8", &undef, "undef");

                (undef, bb)
            } else {
                let result_reg = cg.new_reg();

                bb.add_instruction(
                    cg,
                    &format!("{result_reg} = call {result_typename} {f_ptr}({args})"),
                );

                let result_ptr = cg_alloc(cg, &bb, &result_typename);

                cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

                (result_ptr, bb)
            }
        }

        TypedExprKind::Cast(x, t) => {
            use zrc_typeck::tast::ty::Type::*;
            let cast_opcode = match (x.0.clone(), t.clone()) {
                (x, y) if x == y => "bitcast",
                // signed -> signed = sext
                // signed -> unsigned = sext
                // unsigned -> signed = zext
                // unsigned -> unsigned = zext
                // ptr -> ptr = bitcast
                // T -> T = bitcast
                // int -> ptr = inttoptr
                // ptr -> int = ptrtoint
                // int -> fn = inttoptr
                // fn -> int = ptrtoint
                (Bool, I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "zext",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Bool) => "trunc",

                (I8, I16 | U16 | I32 | U32 | I64 | U64) => "sext",
                (U8, I16 | U16 | I32 | U32 | I64 | U64) => "zext",
                (I16, I32 | U32 | I64 | U64) => "sext",
                (U16, I32 | U32 | I64 | U64) => "zext",
                (I32, I64 | U64) => "sext",
                (U32, I64 | U64) => "zext",

                (I16 | U16 | I32 | U32 | I64 | U64, I8) => "trunc",
                (I16 | U16 | I32 | U32 | I64 | U64, U8) => "trunc",
                (I32 | U32 | I64 | U64, I16) => "trunc",
                (I32 | U32 | I64 | U64, U16) => "trunc",
                (I64 | U64, I32) => "trunc",
                (I64 | U64, U32) => "trunc",

                // also handle things like I32 => U32
                // is this even the correct way to do it?
                (I8, U8) => "bitcast",
                (I16, U16) => "bitcast",
                (I32, U32) => "bitcast",
                (I64, U64) => "bitcast",

                (Ptr(_), Ptr(_)) => "bitcast",
                (Ptr(_), I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "ptrtoint",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Ptr(_)) => "inttoptr",
                (I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64, Fn(_, _)) => "inttoptr",
                (Fn(_, _), I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64) => "ptrtoint",
                _ => bail!("invalid cast from {} to {}", x.0, t),
            };

            let (x_ptr, bb) = cg_expr(cg, bb, scope, *x.clone())?;

            let x_typename = get_llvm_typename(x.0.clone());

            let x_reg = cg_load(cg, &bb, &x_typename, &x_ptr);

            let result_typename = get_llvm_typename(expr.0.clone());

            let result_reg = cg.new_reg();

            bb.add_instruction(
                cg,
                &format!("{result_reg} = {cast_opcode} {x_typename} {x_reg} to {result_typename}"),
            );

            let result_ptr = cg_alloc(cg, &bb, &result_typename);

            cg_store(cg, &bb, &result_typename, &result_ptr, &result_reg);

            (result_ptr, bb)
        }

        // FIXME: Once we have module-scoped type check done, support strings
        TypedExprKind::StringLiteral(_) => {
            bail!("the Zirco code generator does not yet support string literals. aborting!")
        }
    })
}

/// Consists of the [`BasicBlock`]s to `br` to when encountering certain instructions.
/// It is passed to [`cg_block`] to allow it to properly handle break and continue.
#[derive(PartialEq, Debug, Clone)]
pub struct LoopBreakaway {
    /// Points to the exit basic block.
    on_break: BasicBlock,
    /// For `for` loops, points to the latch. For `while` loops, points to the header.
    on_continue: BasicBlock,
}

/// Declares the variable, creating its allocation and also evaluating the assignment
pub fn cg_declaration(
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &mut CgScope,
    declaration: zrc_typeck::tast::stmt::TypedDeclaration,
) -> anyhow::Result<BasicBlock> {
    use zrc_typeck::tast::stmt::{LetDeclaration, TypedDeclaration};
    let mut bb = bb.clone();

    Ok(match declaration {
        // We do nothing for structure declarations because they are not emitted in the LLVM IR (the types are inlined)
        TypedDeclaration::StructDeclaration { .. } => bb.clone(),

        TypedDeclaration::DeclarationList(declarations) => {
            for let_declaration in declarations {
                // allocate space for it
                let ptr = cg_alloc(cg, &bb, &get_llvm_typename(let_declaration.ty.clone()));

                // store it in scope
                scope.insert(&let_declaration.name, &ptr);

                if let Some(value) = let_declaration.value {
                    bb = cg_expr(
                        cg,
                        &bb,
                        scope,
                        zrc_typeck::tast::expr::TypedExpr(
                            let_declaration.ty.clone(),
                            zrc_typeck::tast::expr::TypedExprKind::Assignment(
                                Box::new(zrc_typeck::tast::expr::Place(
                                    let_declaration.ty,
                                    zrc_typeck::tast::expr::PlaceKind::Variable(
                                        let_declaration.name,
                                    ),
                                )),
                                Box::new(value),
                            ),
                        ),
                    )?
                    .1;
                }
            }

            bb
        }

        // TODO: Handle functions (this is done once we have a module code-generator struct)
        TypedDeclaration::FunctionDeclaration { .. } => todo!(),
    })
}

// TODO: Something is needed to stop code generating within a block once a 'br' is reached.
// Currently, LLVM will accept it and create a useless block, which is removed later on.
pub fn cg_block(
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    parent_scope: &CgScope,
    block: Vec<zrc_typeck::tast::stmt::TypedStmt>,
    // If set to None, break and continue are invalid.
    // If set to Some, break and continue will break to the bbs listed in there.
    breakaway: Option<LoopBreakaway>,
) -> anyhow::Result<BasicBlock> {
    use zrc_typeck::tast::stmt::*;

    let mut scope = parent_scope.clone();

    block
        .into_iter()
        .try_fold(bb.clone(), |bb, stmt| -> anyhow::Result<BasicBlock> {
            Ok(match stmt {
                TypedStmt::EmptyStmt => bb,
                TypedStmt::ExprStmt(expr) => cg_expr(cg, &bb, &scope, expr)?.1,
                TypedStmt::IfStmt(cond, then, then_else) => {
                    let (cond_ptr, bb) = cg_expr(cg, &bb, &scope, cond.clone())?;

                    let then_bb = cg.new_bb();
                    let then_else_bb = if then_else.is_some() {
                        Some(cg.new_bb())
                    } else {
                        None
                    };
                    let terminating_bb = cg.new_bb();

                    let cond_reg = cg_load(cg, &bb, "i1", &cond_ptr);
                    bb.add_instruction(
                        cg,
                        &format!(
                            "br i1 {cond_reg}, label {then_bb}, label {}",
                            // without an else, the else branch is just lead to the terminator
                            then_else_bb.clone().unwrap_or(terminating_bb.clone())
                        ),
                    );

                    let then_bb = cg_block(cg, &then_bb, &scope, then, breakaway.clone())?;
                    let then_else_bb = then_else
                        .map(|te| {
                            cg_block(
                                cg,
                                &then_else_bb.clone().unwrap(),
                                &scope,
                                te,
                                breakaway.clone(),
                            )
                        })
                        .transpose()?;

                    then_bb.add_instruction(cg, &format!("br label {terminating_bb}"));
                    if let Some(te) = then_else_bb {
                        te.add_instruction(cg, &format!("br label {terminating_bb}"));
                    }

                    terminating_bb
                }

                TypedStmt::BlockStmt(body) => cg_block(cg, &bb, &scope, body, breakaway.clone())?,

                TypedStmt::ReturnStmt(Some(ex)) => {
                    // FIXME: If a return statement is not the last thing in a block the code generator will continue and emit a llvm syntax error
                    let (ex_ptr, bb) = cg_expr(cg, &bb, &scope, ex.clone())?;
                    let ex_type = get_llvm_typename(ex.0.clone());
                    let ex_reg = cg_load(cg, &bb, &ex_type, &ex_ptr);
                    bb.add_instruction(cg, &format!("ret {ex_type} {ex_reg}"));

                    bb
                }
                TypedStmt::ReturnStmt(None) => {
                    bb.add_instruction(cg, "ret void");
                    bb
                }

                TypedStmt::ContinueStmt => {
                    // We can jump into whatever the 'continue' target is in `breakaway`
                    // This is going to be the loop header for `while` loops and the loop latch for `for` loops

                    match breakaway.clone() {
                        Some(LoopBreakaway { on_continue, .. }) => {
                            bb.add_instruction(cg, &format!("br label {on_continue}"));
                            // make sure to use 'bb' here not 'on_continue' so later statements are appended to this block
                            bb
                        }
                        None => bail!("continue statement outside of loop"),
                    }
                }

                TypedStmt::BreakStmt => {
                    // Jump into the 'break' target in `breakaway`
                    // This is the exit portion of the loop

                    match breakaway.clone() {
                        Some(LoopBreakaway { on_break, .. }) => {
                            bb.add_instruction(cg, &format!("br label {on_break}"));
                            bb
                        }
                        None => bail!("break statement outside of loop"),
                    }
                }

                TypedStmt::Declaration(d) => cg_declaration(cg, &bb, &mut scope, d)?,

                TypedStmt::ForStmt {
                    init,
                    cond,
                    post,
                    body: body_code,
                } => {
                    // For loops generate a somewhat more complicated CFG, with a few parts.
                    // The preheader, where `init` runs. Breaks to the header.
                    // The header, where `cond` is checked and breaks to either the exit or the body.
                    // The body, where most of the body runs. Breaks to the latch. `break` transfers to the exit by force and `continue` transfers to the latch by force.
                    // The latch, where `post` runs and breaks back to the header
                    // The exit, which is the basic block we return.

                    // loops lie in an implicit subscope
                    let mut scope = scope.clone();

                    // The block we are currently in will become the preheader. Generate the `init` code if there is any.
                    let bb = match init {
                        None => bb,
                        Some(init) => cg_declaration(cg, &bb, &mut scope, *init)?,
                    };

                    let header = cg.new_bb();
                    bb.add_instruction(cg, &format!("br label {header}"));

                    let body = cg.new_bb();
                    let latch = cg.new_bb();
                    let exit = cg.new_bb();

                    // You are now officially in the loop. Within the header, we check `cond` and use that to jump to either the body or the exit condition.
                    let header = match cond {
                        None => {
                            header.add_instruction(cg, &format!("br label {body}"));
                            header
                        }
                        Some(cond) => {
                            let (cond_ptr, header) = cg_expr(cg, &header, &scope, cond.clone())?;

                            let cond_reg =
                                cg_load(cg, &header, &get_llvm_typename(cond.0), &cond_ptr);

                            header.add_instruction(
                                cg,
                                &format!("br i1 {cond_reg}, label {body}, label {exit}"),
                            );

                            header
                        }
                    };

                    // Generate the body
                    let body = cg_block(
                        cg,
                        &body,
                        &scope,
                        body_code,
                        Some(LoopBreakaway {
                            on_break: exit.clone(),
                            on_continue: latch.clone(),
                        }),
                    )?;

                    // The body breaks to latch
                    body.add_instruction(cg, &format!("br label {latch}"));

                    // Latch runs post and then breaks right back to the header.
                    let latch = if let Some(post) = post {
                        cg_expr(cg, &latch, &scope, post)?.1
                    } else {
                        latch
                    };

                    latch.add_instruction(cg, &format!("br label {header}"));

                    exit
                }
                TypedStmt::WhileStmt(cond, body_code) => {
                    // While loops are similar to for loops but much simpler.
                    // The preheader simply just breaks to the header.
                    // The header checks the condition and breaks to the exit or the body.
                    // The body simply breaks to the header.
                    // The exit is the continued code

                    // `break` => exit
                    // `continue` => header

                    let header = cg.new_bb();
                    bb.add_instruction(cg, &format!("br label {header}"));

                    let body = cg.new_bb();
                    let exit = cg.new_bb();

                    let (cond_ptr, header) = cg_expr(cg, &bb, &scope, cond.clone())?;
                    let cond_reg = cg_load(cg, &bb, &get_llvm_typename(cond.0.clone()), &cond_ptr);

                    header.add_instruction(
                        cg,
                        &format!("br i1 {cond_reg}, label {body}, label {exit}"),
                    );

                    let body = cg_block(
                        cg,
                        &bb,
                        &scope,
                        body_code,
                        Some(LoopBreakaway {
                            on_break: exit.clone(),
                            on_continue: header.clone(),
                        }),
                    )?;

                    // The body breaks to header
                    body.add_instruction(cg, &format!("br label {header}"));

                    exit
                }
            })
        })
}
