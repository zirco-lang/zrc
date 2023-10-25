#![allow(unknown_lints)] // in case you use non-nightly clippy
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::absolute_paths,
    clippy::as_conversions,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::deref_by_slicing,
    clippy::disallowed_script_idents,
    clippy::else_if_without_else,
    clippy::empty_structs_with_brackets,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::let_underscore_must_use,
    clippy::min_ident_chars,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::non_ascii_literal,
    clippy::redundant_type_annotations,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::unseparated_literal_suffix,
    clippy::string_to_string,
    clippy::todo,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unneeded_field_pattern,
    clippy::wildcard_enum_match_arm,

    // These should be enabled in any non-user-facing code, like the parser, but not in the
    // frontend.
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    clippy::missing_panics_doc
)]

pub mod expr;
pub mod stmt;

use std::{collections::HashMap, fmt::Display};

use anyhow::Context as _;
pub use expr::cg_expr;
pub use stmt::{cg_block, cg_program};
use zrc_typeck::tast::{stmt::ArgumentDeclarationList, ty::Type};

#[derive(PartialEq, Debug, Clone)]
struct Counter {
    value: usize,
}
impl Counter {
    const fn new(initial_value: usize) -> Self {
        Self {
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
#[derive(Debug, PartialEq, Clone)]
pub struct ModuleCg {
    pub declarations: Vec<String>,
    global_constant_id: Counter,
}
impl ModuleCg {
    #[must_use]
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
            writeln!(f, "{decl}")?;
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
pub struct FunctionCg<'input> {
    name: String,
    parameters: Vec<(String, Type<'input>)>,
    ret: zrc_typeck::typeck::BlockReturnType<'input>,
    blocks: Vec<BasicBlockData>,
    next_instruction_id: Counter,
    /// One of our design decisions involves using memory registers extremely
    /// often, and `alloca`s can only be easily optimized to SSA registers
    /// by SROA if they are located at the beginning of the first basic block
    /// in the function. For this reason, the allocas are located here.
    allocations: Vec<String>,
}
impl<'input> FunctionCg<'input> {
    #[must_use]
    pub fn new(
        name: String,
        ret: zrc_typeck::typeck::BlockReturnType<'input>,
        parameters: Vec<(&'input str, Type<'input>)>,
        parent_scope: &CgScope<'input>,
    ) -> (Self, BasicBlock, CgScope<'input>) {
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
            let ptr = cg_alloc(&mut cg, bb, &get_llvm_typename(parameter.1.clone()));

            // store the parameter into the alloca
            cg_store(
                &mut cg,
                bb,
                &get_llvm_typename(parameter.1),
                &ptr,
                &format!("%p{id}"),
            )
            .unwrap(); // we KNOW cg is this bb already

            // insert the parameter into the scope
            scope.insert(parameter.0, ptr);
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

    fn add_instruction_to_bb(&mut self, bb: BasicBlock, instr: &str) -> anyhow::Result<()> {
        let bb = self.blocks.get_mut(bb.id).context("Invalid BB ID")?;
        bb.instructions.push(instr.to_string());

        Ok(())
    }
}
impl<'input> Display for FunctionCg<'input> {
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
                    writeln!(f, "    {instr}")?;
                }
                for instr in &bb.instructions {
                    writeln!(f, "    {instr}")?;
                }
            } else {
                writeln!(f, "bb{}:", bb.id)?;
                for instr in &bb.instructions {
                    writeln!(f, "    {instr}")?;
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
    const fn new(id: usize) -> Self {
        Self {
            id,
            instructions: Vec::new(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct BasicBlock {
    id: usize,
}
impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%bb{}", self.id)
    }
}
impl BasicBlock {
    fn add_instruction(self, cg: &mut FunctionCg, instr: &str) -> anyhow::Result<()> {
        cg.add_instruction_to_bb(self, instr)
    }
}

fn get_llvm_typename(ty: Type) -> String {
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
            match params {
                ArgumentDeclarationList::NonVariadic(params) => params
                    .into_iter()
                    .map(|x| get_llvm_typename(x.ty))
                    .collect::<Vec<_>>()
                    .join(", "),
                ArgumentDeclarationList::Variadic(params) => format!(
                    "{}, ...",
                    params
                        .into_iter()
                        .map(|x| get_llvm_typename(x.ty))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            }
        ),
        Type::Struct(entries) => format!(
            "{{ {} }}",
            entries
                .into_iter()
                .map(|(_, value)| get_llvm_typename(value))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// Allocates a new register and returns it. This also properly handles the
/// alloca instruction needing to be at the beginning of a basic block.
fn cg_alloc(cg: &mut FunctionCg, _bb: BasicBlock, ty: &str) -> String {
    let reg = cg.new_reg();
    cg.allocations.push(format!("{reg} = alloca {ty}"));
    reg
}
/// Loads a type `T` from a `T*`
fn cg_load(cg: &mut FunctionCg, bb: BasicBlock, ty: &str, ptr: &str) -> anyhow::Result<String> {
    let reg = cg.new_reg();
    bb.add_instruction(cg, &format!("{reg} = load {ty}, ptr {ptr}"))?;
    Ok(reg)
}
/// Stores a type `T` to a `T*`
fn cg_store(
    cg: &mut FunctionCg,
    bb: BasicBlock,
    ty: &str,
    ptr: &str,
    value: &str,
) -> anyhow::Result<()> {
    bb.add_instruction(cg, &format!("store {ty} {value}, ptr {ptr}"))?;
    Ok(())
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct CgScope<'input> {
    /// Maps identifiers to their LLVM register
    identifiers: HashMap<&'input str, String>,
}
impl<'input> CgScope<'input> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
        }
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<&String> {
        self.identifiers.get(ident)
    }

    pub fn insert(&mut self, ident: &'input str, reg: String) {
        self.identifiers.insert(ident, reg);
    }
}
impl<'input> Default for CgScope<'input> {
    fn default() -> Self {
        Self::new()
    }
}
