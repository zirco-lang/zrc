pub mod expr;
pub mod stmt;

pub use expr::cg_expr;
pub use stmt::{cg_block, cg_program};

use std::{collections::HashMap, fmt::Display};

use anyhow::Context as _;

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

/// A code generator for one module, the unit of compilation in Zirco
/// corresponding to one source file.
pub struct ModuleCg {
    pub declarations: Vec<String>,
    global_constant_id: Counter,
}
impl ModuleCg {
    pub fn new() -> Self {
        Self {
            declarations: Vec::new(),
            global_constant_id: Counter::new(0),
        }
    }
}
impl Display for ModuleCg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in &self.declarations {
            writeln!(f, "{}", decl)?;
        }
        Ok(())
    }
}
impl Default for ModuleCg {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCg {
    name: String,
    parameters: Vec<(String, zrc_typeck::tast::ty::Type)>,
    ret: zrc_typeck::typeck::BlockReturnType,
    blocks: Vec<BasicBlockData>,
    next_instruction_id: Counter,
    /// One of our design decisions involves using memory registers extremely
    /// often, and `alloca`s can only be easily optimized to SSA registers
    /// by SROA if they are located at the beginning of the first basic block
    /// in the function. For this reason, the allocas are located here.
    allocations: Vec<String>,
}
impl FunctionCg {
    pub fn new(
        name: String,
        ret: zrc_typeck::typeck::BlockReturnType,
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
            )
            .unwrap(); // we KNOW cg is this bb already

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
                .map(|(name, ty)| format!("{} {name}", get_llvm_typename(ty.clone())))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
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
        write!(f, "}}")?;
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
/// FIXME: This MUST use insertion order in the future for C FFI compat, instead
/// of our.. alphabetical order approach?
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

/// Allocates a new register and returns it. This also properly handles the
/// alloca instruction needing to be at the beginning of a basic block.
fn cg_alloc(cg: &mut FunctionCg, _bb: &BasicBlock, ty: &str) -> String {
    let reg = cg.new_reg();
    cg.allocations.push(format!("{reg} = alloca {ty}"));
    reg
}
/// Loads a type `T` from a `T*`
fn cg_load(cg: &mut FunctionCg, bb: &BasicBlock, ty: &str, ptr: &str) -> anyhow::Result<String> {
    let reg = cg.new_reg();
    bb.add_instruction(cg, &format!("{reg} = load {ty}, ptr {ptr}"))?;
    Ok(reg)
}
/// Stores a type `T` to a `T*`
fn cg_store(
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    ty: &str,
    ptr: &str,
    value: &str,
) -> anyhow::Result<()> {
    bb.add_instruction(cg, &format!("store {ty} {value}, ptr {ptr}"))?;
    Ok(())
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
impl Default for CgScope {
    fn default() -> Self {
        Self::new()
    }
}
