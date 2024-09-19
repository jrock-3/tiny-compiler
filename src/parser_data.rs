#![allow(dead_code)]
use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Debug,
    io::BufRead,
};

use crate::tokenizer::Tokenizer;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpType {
    Add,
    Sub,
    Mul,
    Div,
}
pub struct InvalidITypeError;

impl TryFrom<IType> for OpType {
    type Error = InvalidITypeError;

    fn try_from(value: IType) -> Result<Self, Self::Error> {
        match value {
            IType::Add { .. } => Ok(OpType::Add),
            IType::Sub { .. } => Ok(OpType::Sub),
            IType::Mul { .. } => Ok(OpType::Mul),
            IType::Div { .. } => Ok(OpType::Div),
            _ => Err(InvalidITypeError),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IType {
    Const(isize),

    Add {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
    },
    Sub {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
    },
    Mul {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
    },
    Div {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
    },
    Cmp {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
    },

    Phi {
        inst1: (usize, Option<usize>),
        inst2: (usize, Option<usize>),
        var: usize,
    },

    End,
    Bra {
        block: usize,
    },
    Bne {
        inst: usize,
        block: Option<usize>,
    },
    Beq {
        inst: usize,
        block: Option<usize>,
    },
    Ble {
        inst: usize,
        block: Option<usize>,
    },
    Blt {
        inst: usize,
        block: Option<usize>,
    },
    Bge {
        inst: usize,
        block: Option<usize>,
    },
    Bgt {
        inst: usize,
        block: Option<usize>,
    },

    // User defined functions
    Jsr {
        block: usize,
    },
    Ret {
        inst: Option<(usize, Option<usize>)>,
    },
    GetPar1,
    GetPar2,
    GetPar3,
    SetPar1 {
        inst: (usize, Option<usize>),
    },
    SetPar2 {
        inst: (usize, Option<usize>),
    },
    SetPar3 {
        inst: (usize, Option<usize>),
    },

    // Builtins
    Read,
    Write {
        inst: (usize, Option<usize>),
    },
    WriteNL,

    // Placeholder
    Assignment {
        var: usize,
        dep_var: Option<usize>,
    },
    Empty,
}

impl IType {
    pub fn branch_block(&self, block: usize) -> Self {
        match self {
            &IType::Beq { inst, .. } => IType::Beq {
                inst,
                block: Some(block),
            },
            &IType::Bne { inst, .. } => IType::Bne {
                inst,
                block: Some(block),
            },
            &IType::Bgt { inst, .. } => IType::Bgt {
                inst,
                block: Some(block),
            },
            &IType::Bge { inst, .. } => IType::Bge {
                inst,
                block: Some(block),
            },
            &IType::Blt { inst, .. } => IType::Blt {
                inst,
                block: Some(block),
            },
            &IType::Ble { inst, .. } => IType::Ble {
                inst,
                block: Some(block),
            },
            _ => unreachable!(),
        }
    }

    pub fn into(self, var: usize, phi: usize) -> IType {
        match self {
            IType::Phi {
                inst1,
                inst2,
                var: v,
            } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Phi {
                    inst1,
                    inst2,
                    var: v,
                }
            }
            IType::Add { inst1, inst2 } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Add { inst1, inst2 }
            }
            IType::Sub { inst1, inst2 } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Sub { inst1, inst2 }
            }
            IType::Mul { inst1, inst2 } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Mul { inst1, inst2 }
            }
            IType::Div { inst1, inst2 } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Div { inst1, inst2 }
            }
            IType::Cmp { inst1, inst2 } => {
                let inst1 = if inst1.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst1
                };

                let inst2 = if inst2.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst2
                };

                IType::Cmp { inst1, inst2 }
            }
            IType::Ret { inst } => {
                let inst = if inst.map(|e| e.1) == Some(Some(var)) {
                    Some((phi, Some(var)))
                } else {
                    inst
                };
                IType::Ret { inst }
            }
            IType::SetPar1 { inst } => {
                let inst = if inst.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst
                };
                IType::SetPar1 { inst }
            }
            IType::SetPar2 { inst } => {
                let inst = if inst.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst
                };
                IType::SetPar2 { inst }
            }
            IType::SetPar3 { inst } => {
                let inst = if inst.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst
                };
                IType::SetPar3 { inst }
            }
            IType::Write { inst } => {
                let inst = if inst.1 == Some(var) {
                    (phi, Some(var))
                } else {
                    inst
                };
                IType::Write { inst }
            }
            itype @ _ => itype,
        }
    }

    fn update_phi(self, inst: usize, frame_status: FrameStatus) -> IType {
        match self {
            IType::Phi {
                inst1, inst2, var, ..
            } => match frame_status {
                FrameStatus::FallThrough => IType::Phi {
                    inst1: (inst, inst1.1),
                    inst2,
                    var,
                },
                FrameStatus::Follow => IType::Phi {
                    inst1,
                    inst2: (inst, inst2.1),
                    var,
                },
            },
            // IType::Phi {
            //     inst1: (inst, inst1.1),
            //     inst2,
            //     var,
            // },
            _ => unreachable!(),
        }
    }

    pub fn update_inst(self, from_inst: usize, to_inst: usize) -> IType {
        match self {
            IType::Phi { inst1, inst2, var } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Phi { inst1, inst2, var }
            }
            IType::Add { inst1, inst2 } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Add { inst1, inst2 }
            }
            IType::Sub { inst1, inst2 } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Sub { inst1, inst2 }
            }
            IType::Mul { inst1, inst2 } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Mul { inst1, inst2 }
            }
            IType::Div { inst1, inst2 } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Div { inst1, inst2 }
            }
            IType::Cmp { inst1, inst2 } => {
                let inst1 = if inst1.0 == from_inst {
                    (to_inst, inst1.1)
                } else {
                    inst1
                };
                let inst2 = if inst2.0 == from_inst {
                    (to_inst, inst2.1)
                } else {
                    inst2
                };
                IType::Cmp { inst1, inst2 }
            }
            IType::Ret { inst } => {
                let inst = inst.map(|inst| {
                    if inst.0 == from_inst {
                        (to_inst, inst.1)
                    } else {
                        inst
                    }
                });
                IType::Ret { inst }
            }
            IType::SetPar1 { inst } => {
                let inst = if inst.0 == from_inst {
                    (to_inst, inst.1)
                } else {
                    inst
                };
                IType::SetPar1 { inst }
            }
            IType::SetPar2 { inst } => {
                let inst = if inst.0 == from_inst {
                    (to_inst, inst.1)
                } else {
                    inst
                };
                IType::SetPar2 { inst }
            }
            IType::SetPar3 { inst } => {
                let inst = if inst.0 == from_inst {
                    (to_inst, inst.1)
                } else {
                    inst
                };
                IType::SetPar3 { inst }
            }
            IType::Write { inst } => {
                let inst = if inst.0 == from_inst {
                    (to_inst, inst.1)
                } else {
                    inst
                };
                IType::Write { inst }
            }
            itype @ _ => itype,
        }
    }
}

impl PartialEq for IType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            IType::Const(val) => {
                if let IType::Const(v) = other {
                    val == v
                } else {
                    false
                }
            }
            IType::Add { inst1, inst2 } => {
                if let IType::Add {
                    inst1: i1,
                    inst2: i2,
                } = other
                {
                    inst1.0 == i1.0 && inst2.0 == i2.0
                } else {
                    false
                }
            }
            IType::Sub { inst1, inst2 } => {
                if let IType::Sub {
                    inst1: i1,
                    inst2: i2,
                } = other
                {
                    inst1.0 == i1.0 && inst2.0 == i2.0
                } else {
                    false
                }
            }
            IType::Mul { inst1, inst2 } => {
                if let IType::Mul {
                    inst1: i1,
                    inst2: i2,
                } = other
                {
                    inst1.0 == i1.0 && inst2.0 == i2.0
                } else {
                    false
                }
            }
            IType::Div { inst1, inst2 } => {
                if let IType::Div {
                    inst1: i1,
                    inst2: i2,
                } = other
                {
                    inst1.0 == i1.0 && inst2.0 == i2.0
                } else {
                    false
                }
            }
            IType::Cmp { inst1, inst2 } => {
                if let IType::Cmp {
                    inst1: i1,
                    inst2: i2,
                } = other
                {
                    inst1.0 == i1.0 && inst2.0 == i2.0
                } else {
                    false
                }
            }
            IType::Phi { inst1, inst2, var } => {
                if let IType::Phi {
                    inst1: i1,
                    inst2: i2,
                    var: v,
                } = other
                {
                    inst1 == i1 && inst2 == i2 && var == v
                } else {
                    false
                }
            }
            IType::End => matches!(other, IType::End),
            IType::Bra { block } => {
                if let IType::Bra { block: b } = other {
                    block == b
                } else {
                    false
                }
            }
            IType::Bne { inst, block } => {
                if let IType::Bne { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Beq { inst, block } => {
                if let IType::Beq { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Ble { inst, block } => {
                if let IType::Ble { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Blt { inst, block } => {
                if let IType::Blt { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Bge { inst, block } => {
                if let IType::Bge { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Bgt { inst, block } => {
                if let IType::Bgt { inst: i, block: b } = other {
                    inst == i && block == b
                } else {
                    false
                }
            }
            IType::Jsr { block } => {
                if let IType::Jsr { block: b } = other {
                    block == b
                } else {
                    false
                }
            }
            IType::Ret { inst } => {
                if let IType::Ret { inst: i } = other {
                    inst.map(|i| i.0) == i.map(|i| i.0)
                } else {
                    false
                }
            }
            IType::GetPar1 => matches!(other, IType::GetPar1),
            IType::GetPar2 => matches!(other, IType::GetPar2),
            IType::GetPar3 => matches!(other, IType::GetPar3),
            IType::SetPar1 { inst } => {
                if let IType::SetPar1 { inst: i } = other {
                    inst.0 == i.0
                } else {
                    false
                }
            }
            IType::SetPar2 { inst } => {
                if let IType::SetPar2 { inst: i } = other {
                    inst.0 == i.0
                } else {
                    false
                }
            }
            IType::SetPar3 { inst } => {
                if let IType::SetPar3 { inst: i } = other {
                    inst.0 == i.0
                } else {
                    false
                }
            }
            IType::Read => matches!(other, IType::Read),
            IType::Write { inst } => {
                if let IType::Write { inst: i } = other {
                    inst.0 == i.0
                } else {
                    false
                }
            }
            IType::WriteNL => matches!(other, IType::WriteNL),
            IType::Assignment { var, .. } => {
                if let IType::Assignment { var: v, .. } = other {
                    var == v
                } else {
                    false
                }
            }
            IType::Empty => {
                if let IType::Empty = other {
                    true
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Inst {
    id: usize,
    itype: IType,
    dom: Option<usize>,
    block: Option<usize>,
}

impl Inst {
    pub fn new(id: usize, instruction: IType, dom: Option<usize>, block: Option<usize>) -> Inst {
        Inst {
            id,
            itype: instruction,
            dom,
            block,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn dom(&self) -> Option<usize> {
        self.dom
    }

    pub fn itype(&self) -> IType {
        self.itype
    }

    pub fn block(&self) -> Option<usize> {
        self.block
    }

    pub fn set_fall_through(&mut self, block: usize) {
        if let IType::Phi {
            inst1, inst2, var, ..
        } = self.itype
        {
            self.itype = IType::Phi {
                inst1: (block, inst1.1),
                inst2,
                var,
            }
        } else {
            unreachable!()
        }
    }

    pub fn set_follow(&mut self, block: usize) {
        if let IType::Phi {
            inst1, inst2, var, ..
        } = self.itype
        {
            self.itype = IType::Phi {
                inst1,
                inst2: (block, inst2.1),
                var,
            }
        } else {
            unreachable!()
        }
    }

    pub fn update_phi(&mut self, var: usize, phi: usize) {
        self.itype = self.itype.into(var, phi);
    }

    fn generate_graph<R: BufRead>(
        &self,
        inst_list: &InstList,
        block_list: &BlockList,
        tokenizer: &Tokenizer<R>,
    ) -> String {
        let itype_str = match self.itype {
            IType::Const(val) => format!("const #{}", val),
            IType::Add { inst1, inst2 } => format!("add ({}) ({})", inst1.0, inst2.0),
            IType::Sub { inst1, inst2 } => format!("sub ({}) ({})", inst1.0, inst2.0),
            IType::Mul { inst1, inst2 } => format!("mul ({}) ({})", inst1.0, inst2.0),
            IType::Div { inst1, inst2 } => format!("div ({}) ({})", inst1.0, inst2.0),
            IType::Cmp { inst1, inst2 } => format!("cmp ({}) ({})", inst1.0, inst2.0),
            IType::Phi { inst1, inst2, var } => {
                format!(
                    "({}) phi ({}) ({})",
                    tokenizer.get_var(var),
                    inst1.0,
                    inst2.0
                )
            }
            IType::End => "end".to_string(),
            IType::Bra { block } => {
                format!(
                    "bra ({})",
                    block_list.get(block).get_first_inst(inst_list).unwrap()
                )
            }
            IType::Bne { inst, block } => format!(
                "bne ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Beq { inst, block } => format!(
                "beq ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Ble { inst, block } => format!(
                "ble ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Blt { inst, block } => format!(
                "blt ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Bge { inst, block } => format!(
                "bge ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Bgt { inst, block } => format!(
                "bgt ({}) ({})",
                inst,
                block_list
                    .get(block.unwrap())
                    .get_first_inst(inst_list)
                    .unwrap()
            ),
            IType::Jsr { block } => {
                format!(
                    "jsr {}",
                    block_list.get(block).get_first_inst(inst_list).unwrap()
                )
            }
            IType::Ret { inst } => match inst {
                Some(inst) => format!("ret ({})", inst.0),
                None => "ret".to_string(),
            },
            IType::GetPar1 => "getpar1".to_string(),
            IType::GetPar2 => "getpar2".to_string(),
            IType::GetPar3 => "getpar3".to_string(),
            IType::SetPar1 { inst } => format!("setpar1 ({})", inst.0),
            IType::SetPar2 { inst } => format!("setpar2 ({})", inst.0),
            IType::SetPar3 { inst } => format!("setpar3 ({})", inst.0),
            IType::Read => "read".to_string(),
            IType::Write { inst } => format!("write ({})", inst.0),
            IType::WriteNL => "writeNL".to_string(),
            IType::Empty => r"\<empty\>".to_string(),
            IType::Assignment { .. } => unimplemented!(),
        };

        format!("{}: {}", self.id, itype_str)
    }

    pub fn propagate_phi(&mut self, inst: usize, frame_status: FrameStatus) {
        self.itype = self.itype.update_phi(inst, frame_status);
    }

    fn update_inst(&mut self, from_inst: usize, to_inst: usize) {
        self.itype = self.itype.update_inst(from_inst, to_inst);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    id: usize,
    dom: Option<usize>,
    pub insts: Vec<usize>,
    next: (Option<usize>, Option<usize>),
    // inst, opt<var>
    pub var_map: BTreeMap<usize, Option<(usize, Option<usize>)>>,
    op_map: BTreeMap<OpType, usize>,
    phis: BTreeMap<usize, usize>,
}

impl Block {
    pub fn new(id: usize) -> Block {
        Block {
            id,
            dom: None,
            insts: Vec::new(),
            next: (None, None),
            var_map: BTreeMap::new(),
            op_map: BTreeMap::new(),
            phis: BTreeMap::new(),
        }
    }

    pub fn from(id: usize, prev: &Block) -> Block {
        Block {
            id,
            dom: Some(prev.id),
            insts: Vec::new(),
            next: (None, None),
            var_map: prev.var_map.clone(),
            op_map: prev.op_map.clone(),
            phis: BTreeMap::new(),
        }
    }

    pub fn get_var_inst(&self, id: usize) -> Option<usize> {
        self.var_map
            .get(&id)
            .expect("Var should exist")
            .map(|inst| inst.0.clone())
    }

    pub fn get_dom_inst(&self, itype: IType) -> Option<usize> {
        self.op_map
            .get(&OpType::try_from(itype).ok()?)
            .map(|inst| inst.clone())
    }

    pub fn get_last_inst(&self) -> Option<usize> {
        self.insts.last().cloned()
    }

    pub fn get_first_inst(&self, inst_list: &InstList) -> Option<usize> {
        self.insts
            .iter()
            .filter(|&inst_id| !matches!(inst_list.get(*inst_id).itype(), IType::Assignment { .. }))
            .next()
            .cloned()
    }

    pub fn get_next(&self) -> (Option<usize>, Option<usize>) {
        self.next
    }

    pub fn assign_var(&mut self, var: usize, inst: usize, dep_var: Option<usize>) {
        self.var_map.insert(var, Some((inst, dep_var)));
    }

    pub fn declare_var(&mut self, var: usize) {
        self.var_map.insert(var, None);
    }

    pub fn add_inst(&mut self, inst: usize, itype: IType) -> usize {
        self.insts.push(inst);
        if let Ok(optype) = OpType::try_from(itype) {
            self.op_map.insert(optype, inst);
        }

        inst
    }

    pub fn set_fall_through(&mut self, block: usize) {
        self.next.0 = Some(block);
    }

    pub fn set_follow(&mut self, block: usize) {
        self.next.1 = Some(block);
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn dom(&self) -> Option<usize> {
        self.dom
    }

    pub fn get_phi(&self, var: usize) -> Option<usize> {
        self.phis.get(&var).copied()
    }

    pub fn add_phi(&mut self, var: usize, phi: usize, dep_var: Option<usize>, assignment: usize) {
        self.phis.insert(var, phi);
        self.var_map.insert(var, Some((phi, dep_var)));
        self.insts.insert(0, assignment);
        self.insts.insert(0, phi);
    }

    pub fn generate_graph<R: BufRead>(
        &self,
        inst_list: &InstList,
        block_list: &BlockList,
        tokenizer: &Tokenizer<R>,
    ) -> String {
        let inst_data = inst_list.generate_graph(&self.insts, inst_list, block_list, tokenizer);
        let mut next_data = String::new();

        if !matches!(
            inst_list.get(self.get_last_inst().unwrap()).itype(),
            IType::Ret { .. }
        ) {
            next_data += match self.next {
            (Some(fall_through), Some(follow)) => format!(
                "\tbb{}:s -> bb{}:n [label=\"fall-through\"];\n\tbb{0}:s -> bb{}:n [label=\"branch\"];\n",
                self.id, fall_through, follow
            ),
            (Some(fall_through), None) => format!("\tbb{}:s -> bb{}:n ;\n", self.id, fall_through),
            _ => "".to_string(),
        }
        .as_str();
        }

        next_data += match self.dom {
            Some(dom) => format!(
                "\tbb{}:b -> bb{}:b [color=blue, style=dotted, label=\"dom\"];\n",
                dom, self.id
            ),
            None => "".to_string(),
        }
        .as_str();

        format!(
            "\tbb{0} [shape=record, label=\"<b>BB{} | {{ {} }}\"];\n{}\n",
            self.id, inst_data, next_data
        )
    }

    pub fn resolve_phis(&mut self) {
        for &inst in self.phis.values().rev() {
            self.insts.insert(0, inst);
        }
    }

    pub fn delete_inst(&mut self, inst: usize) {
        let idx = self.insts.iter().position(|&id| id == inst).unwrap();
        self.insts.remove(idx);
    }

    pub fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.insts.iter()
    }

    pub fn update_var(&mut self, renamed_var: usize, phi: usize) {
        for key in self.var_map.clone().keys() {
            if self
                .var_map
                .get(key)
                .unwrap()
                .is_some_and(|val| val.1.is_some_and(|var| var == renamed_var))
            {
                self.var_map.insert(*key, Some((phi, Some(renamed_var))));
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InstList {
    pub instructions: Vec<Inst>,
    pub inst_count: usize,
}

impl InstList {
    pub fn new() -> InstList {
        InstList {
            instructions: Vec::new(),
            inst_count: 0,
        }
    }

    pub fn add(&mut self, itype: IType, dom: Option<usize>, block: Option<usize>) -> usize {
        let inst_id = self.inst_count;
        let inst = Inst::new(inst_id, itype, dom, block);
        self.inst_count += 1;
        self.instructions.push(inst);
        inst_id
    }

    pub fn get(&self, idx: usize) -> &Inst {
        self.instructions
            .get(idx)
            .expect("Instruction should exist")
    }

    pub fn get_mut(&mut self, idx: usize) -> &mut Inst {
        self.instructions
            .get_mut(idx)
            .expect("Instruction should exist")
    }

    pub fn get_matching_inst(&self, itype: IType, start: usize) -> Option<usize> {
        self.find_inst(itype, Some(start))
    }

    fn find_inst(&self, itype: IType, idx: Option<usize>) -> Option<usize> {
        let inst = self.get(idx?);
        if itype == inst.itype() {
            return idx;
        }
        self.find_inst(itype, inst.dom())
    }

    pub fn generate_graph<R: BufRead>(
        &self,
        insts: &[usize],
        inst_list: &InstList,
        block_list: &BlockList,
        tokenizer: &Tokenizer<R>,
    ) -> String {
        if insts.is_empty() {
            return r"\<empty\>".to_string();
        }
        insts
            .into_iter()
            .filter_map(|&inst_id| {
                let inst = self.get(inst_id);
                if matches!(inst.itype(), IType::Assignment { .. }) {
                    None
                } else {
                    Some(inst.generate_graph(inst_list, block_list, tokenizer))
                }
            })
            // .map(|&inst_id| self.get(inst_id).generate_graph(block_list, tokenizer))
            .collect::<Vec<_>>()
            .join(" | ")
    }

    pub fn generate_instructions<R: BufRead>(
        &self,
        insts: &[usize],
        inst_list: &InstList,
        block_list: &BlockList,
        tokenizer: &Tokenizer<R>,
    ) -> String {
        if insts.is_empty() {
            return r"\<empty\>".to_string();
        }
        insts
            .into_iter()
            .filter_map(|&inst_id| {
                let inst = self.get(inst_id);
                if matches!(inst.itype(), IType::Assignment { .. })
                    || matches!(inst.itype(), IType::Empty)
                {
                    None
                } else {
                    Some(inst.generate_graph(inst_list, block_list, tokenizer))
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn rename_inst(&mut self, from_inst: usize, to_inst: usize) {
        for inst in self.instructions.iter_mut() {
            inst.update_inst(from_inst, to_inst);
        }
    }

    pub fn remove_block(&mut self, inst_id: usize) {
        self.get_mut(inst_id).block = None;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockList {
    pub blocks: Vec<Block>,
    block_count: usize,
}

impl BlockList {
    pub fn new() -> BlockList {
        BlockList {
            blocks: Vec::new(),
            block_count: 0,
        }
    }

    pub fn add(&mut self) -> usize {
        let block_id = self.block_count;
        let block = Block::new(block_id);
        self.block_count += 1;
        self.blocks.push(block);
        block_id
    }

    pub fn add_from(&mut self, block: usize) -> usize {
        let block_id = self.block_count;
        let block = Block::from(block_id, self.get(block));
        self.block_count += 1;
        self.blocks.push(block);
        block_id
    }

    pub fn get(&self, idx: usize) -> &Block {
        self.blocks.get(idx).expect("Block should exist")
    }

    pub fn get_mut(&mut self, idx: usize) -> &mut Block {
        self.blocks.get_mut(idx).expect("Block should exist")
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Block> {
        self.blocks.iter()
    }

    pub fn resolve_phis(&mut self) {
        for block in self.blocks.iter_mut() {
            block.resolve_phis();
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Const(isize),
    Inst(usize),
    Var(usize),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FrameState {
    Conditional,
    While,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FrameStatus {
    FallThrough,
    Follow,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockFrame {
    pub state: Option<FrameState>,
    pub follow: Option<usize>,
    pub fall_through: Option<usize>,
    pub join: Option<usize>,
    pub root: Option<usize>,
    pub status: Option<FrameStatus>,
}

impl BlockFrame {
    pub fn new() -> Self {
        BlockFrame {
            state: None,
            follow: None,
            fall_through: None,
            join: None,
            root: None,
            status: None,
        }
    }

    pub fn conditional(&mut self) -> &mut Self {
        self.state = Some(FrameState::Conditional);
        self
    }

    pub fn while_loop(&mut self) -> &mut Self {
        self.state = Some(FrameState::While);
        self
    }

    pub fn follow(&mut self, follow: usize) -> &mut Self {
        self.follow = Some(follow);
        self
    }

    pub fn join(&mut self, join: usize) -> &mut Self {
        self.join = Some(join);
        self
    }

    pub fn fall_through(&mut self, fall_through: usize) -> &mut Self {
        self.fall_through = Some(fall_through);
        self
    }

    pub fn root(&mut self, root: usize) -> &mut Self {
        self.root = Some(root);
        self
    }

    pub fn status(&mut self, status: FrameStatus) -> &mut Self {
        self.status = Some(status);
        self
    }

    pub fn is_fall_through(&self) -> bool {
        self.status == Some(FrameStatus::FallThrough)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockState {
    pub curr: usize,
    stack: VecDeque<BlockFrame>,
}

impl BlockState {
    pub fn new(curr: usize) -> BlockState {
        BlockState {
            curr,
            stack: VecDeque::new(),
        }
    }

    pub fn curr(&self) -> usize {
        self.curr
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn front(&self) -> &BlockFrame {
        self.stack.front().unwrap()
    }

    pub fn front_mut(&mut self) -> &mut BlockFrame {
        self.stack.front_mut().unwrap()
    }

    pub fn push(&mut self, blocks: BlockFrame) {
        self.stack.push_front(blocks);
    }

    pub fn pop(&mut self) -> Option<BlockFrame> {
        self.stack.pop_front()
    }
}
