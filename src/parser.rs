#![allow(dead_code, unused_variables)]
use std::{collections::BTreeMap, fmt::Debug, io::BufRead};

use crate::{
    parser_data::{
        BlockFrame, BlockList, BlockState, FrameState, FrameStatus, IType, InstList, Operand,
    },
    tokenizer::Tokenizer,
    tokenizer_data::{
        RelOp, Token, PREDEFINED_INPUTNUM_ID, PREDEFINED_OUTPUTNEWLINE_ID, PREDEFINED_OUTPUTNUM_ID,
    },
};

#[derive(Debug, PartialEq)]
pub struct Parser<R: BufRead + Debug> {
    tokenizer: Tokenizer<R>,
    const_map: BTreeMap<isize, usize>,
    // func ident -> block id
    func_map: BTreeMap<usize, usize>,
    block_state: BlockState,
    blocks: BlockList,
    insts: InstList,
    curr_init: usize,
}

impl<R: BufRead + Debug> Parser<R> {
    pub fn new(input: R) -> Parser<R> {
        let mut block_list = BlockList::new();
        let init_block = block_list.add();
        let start_block = block_list.add_from(init_block);

        let block_state = BlockState::new(start_block);

        Parser {
            tokenizer: Tokenizer::new(input),
            const_map: BTreeMap::new(),
            func_map: BTreeMap::new(),
            block_state,
            blocks: block_list,
            insts: InstList::new(),
            curr_init: start_block,
        }
    }

    // Helper
    fn consume(&mut self, token: Token) -> Option<()> {
        if self.tokenizer.peek() == Some(token) {
            self.tokenizer.next();
            Some(())
        } else {
            None
        }
    }

    fn op_to_inst(&mut self, operand: Operand) -> (usize, Option<usize>) {
        match operand {
            Operand::Const(val) => (self.get_const(val), None),
            Operand::Inst(inst) => (inst, None),
            Operand::Var(id) => (self.var_to_val(self.block_state.curr(), id), Some(id)),
        }
    }

    fn add_inst(&mut self, block: usize, itype: IType) -> usize {
        let inst = self.insts.add(
            itype,
            self.blocks.get(block).get_dom_inst(itype),
            Some(block),
        );

        self.blocks.get_mut(block).add_inst(inst, itype);
        inst
    }

    pub fn get_const(&mut self, val: isize) -> usize {
        if let Some(inst) = self.const_map.get(&val) {
            inst.to_owned()
        } else {
            let inst = self.add_inst(0, IType::Const(val));
            self.const_map.insert(val, inst);
            inst
        }
    }

    fn compute(&mut self, op: Token, x: Operand, y: Operand) -> Operand {
        match (x, y) {
            (Operand::Const(val1), Operand::Const(val2)) => match op {
                Token::Plus => Operand::Const(val1 + val2),
                Token::Minus => Operand::Const(val1 - val2),
                Token::Times => Operand::Const(val1 * val2),
                Token::Divide => Operand::Const(val1 / val2),
                _ => unreachable!(),
            },
            (x, y) => {
                let inst1 = self.op_to_inst(x);
                let inst2 = self.op_to_inst(y);

                let inst = match op {
                    Token::Plus => {
                        self.add_inst(self.block_state.curr(), IType::Add { inst1, inst2 })
                    }
                    Token::Minus => {
                        self.add_inst(self.block_state.curr(), IType::Sub { inst1, inst2 })
                    }
                    Token::Times => {
                        self.add_inst(self.block_state.curr(), IType::Mul { inst1, inst2 })
                    }
                    Token::Divide => {
                        self.add_inst(self.block_state.curr(), IType::Div { inst1, inst2 })
                    }
                    _ => unreachable!(),
                };

                Operand::Inst(inst)
            }
        }
    }

    fn load_arg(&mut self, inst: (usize, Option<usize>), argnum: usize) -> usize {
        let itype = match argnum {
            0 => IType::SetPar1 { inst },
            1 => IType::SetPar2 { inst },
            2 => IType::SetPar3 { inst },
            _ => unimplemented!("More than 3 args not implemented"),
        };

        let inst = self.add_inst(self.block_state.curr(), itype);

        inst
    }

    fn declare_arg(&mut self, argnum: usize) -> usize {
        let itype = match argnum {
            0 => IType::GetPar1,
            1 => IType::GetPar2,
            2 => IType::GetPar3,
            _ => unimplemented!("More than 3 args not implemented"),
        };

        let inst = self.add_inst(self.block_state.curr(), itype);

        inst
    }

    // Parsers
    fn factor(&mut self) -> Option<Operand> {
        match self.tokenizer.peek()? {
            Token::Number(num) => {
                self.tokenizer.next();
                Some(Operand::Const(num))
            }
            Token::OpenParen => {
                self.tokenizer.next();
                let res = self.expression()?;
                self.consume(Token::CloseParen);
                Some(res)
            }
            Token::Ident(var) => {
                self.tokenizer.next();
                Some(Operand::Var(var))
            }
            Token::Call => self.func_call(),
            _ => None,
        }
    }

    fn term(&mut self) -> Option<Operand> {
        if let Token::Ident(_) | Token::Number(_) | Token::OpenParen | Token::Call =
            self.tokenizer.peek()?
        {
            let mut res = self.factor()?;
            loop {
                if let op @ (Token::Times | Token::Divide) = self.tokenizer.peek()? {
                    self.tokenizer.next();
                    let factor = self.factor()?;
                    res = self.compute(op, res, factor);
                } else {
                    break;
                }
            }
            Some(res)
        } else {
            None
        }
    }

    fn expression(&mut self) -> Option<Operand> {
        match self.tokenizer.peek()? {
            Token::Ident(_) | Token::Number(_) | Token::OpenParen | Token::Call => {
                let mut res = self.term()?;
                loop {
                    if let op @ (Token::Plus | Token::Minus) = self.tokenizer.peek()? {
                        self.tokenizer.next();
                        let term = self.term()?;
                        res = self.compute(op, res, term);
                    } else {
                        break;
                    }
                }
                Some(res)
            }
            _ => None,
        }
    }

    fn relation(&mut self) -> Option<IType> {
        let expr1 = self.expression()?;

        if let Token::RelOp(rel_op) = self.tokenizer.peek()? {
            self.tokenizer.next();

            let expr2 = self.expression()?;

            let inst1 = self.op_to_inst(expr1);
            let inst2 = self.op_to_inst(expr2);

            let cmp_inst = self.add_inst(self.block_state.curr(), IType::Cmp { inst1, inst2 });

            let instruction = match rel_op {
                RelOp::Equal => IType::Bne {
                    inst: cmp_inst,
                    block: None,
                },
                RelOp::NotEqual => IType::Beq {
                    inst: cmp_inst,
                    block: None,
                },
                RelOp::LessThan => IType::Bge {
                    inst: cmp_inst,
                    block: None,
                },
                RelOp::LessThanOrEqual => IType::Bgt {
                    inst: cmp_inst,
                    block: None,
                },
                RelOp::GreaterThan => IType::Ble {
                    inst: cmp_inst,
                    block: None,
                },
                RelOp::GreaterThanOrEqual => IType::Blt {
                    inst: cmp_inst,
                    block: None,
                },
            };

            Some(instruction)
        } else {
            return None;
        }
    }

    fn add_phi(&mut self, var: usize, inst_id: usize) {
        let curr_state = self.block_state.front().clone();
        let join_block = curr_state.join.unwrap();

        let phi_id = self.blocks.get(join_block).get_phi(var).unwrap();

        match curr_state.state.unwrap() {
            FrameState::While => {
                self.insts.get_mut(phi_id).set_follow(inst_id);
            }
            FrameState::Conditional => {
                if curr_state.is_fall_through() {
                    self.insts.get_mut(phi_id).set_fall_through(inst_id);
                } else {
                    self.insts.get_mut(phi_id).set_follow(inst_id);
                }
            }
        }

        let curr_frame = self.block_state.pop().unwrap();
        if !self.block_state.is_empty() {
            self.add_phi(var, phi_id);
        }
        self.block_state.push(curr_frame);
    }

    fn var_to_val(&mut self, block: usize, var: usize) -> usize {
        match self.blocks.get(block).get_var_inst(var) {
            Some(inst) => inst,
            None => {
                println!(
                    "[Warning] Variable {} is not initialized",
                    self.tokenizer.get_var(var)
                );
                let inst = self.get_const(0);
                self.blocks
                    .get_mut(self.block_state.curr())
                    .assign_var(var, inst, None);
                inst
            }
        }
    }

    fn assignment(&mut self) -> Option<()> {
        self.consume(Token::Let)?;

        if let Token::Ident(var) = self.tokenizer.peek()? {
            self.tokenizer.next();

            self.consume(Token::Assignment)?;

            let expr = self.expression()?;
            let dep_var = match expr {
                Operand::Var(dep_var) => Some(dep_var),
                _ => None,
            };
            let assignment = IType::Assignment { var, dep_var };
            let inst = self.op_to_inst(expr);

            self.add_inst(self.block_state.curr(), assignment);
            // self.insts.add(assignment, None, None);

            if !self.block_state.is_empty() {
                if let Some(_) = self.block_state.front().join {
                    self.add_phi(var, inst.0);
                }
            }

            self.blocks
                .get_mut(self.block_state.curr())
                .assign_var(var, inst.0, dep_var);

            Some(())
        } else {
            None
        }
    }

    fn func_call(&mut self) -> Option<Operand> {
        self.consume(Token::Call)?;

        if let Token::Ident(id) = self.tokenizer.peek()? {
            self.tokenizer.next();
            let args = if self.consume(Token::OpenParen).is_some() {
                if self.consume(Token::CloseParen).is_some() {
                    None
                } else {
                    let mut args = Vec::new();
                    loop {
                        let expr = self.expression()?;
                        let inst = self.op_to_inst(expr);

                        args.push(inst);

                        if !self.consume(Token::Comma).is_some() {
                            break;
                        }
                    }

                    self.consume(Token::CloseParen)?;

                    Some(args)
                }
            } else {
                None
            };

            let call = match id {
                PREDEFINED_INPUTNUM_ID => self.add_inst(self.block_state.curr(), IType::Read),
                PREDEFINED_OUTPUTNUM_ID => {
                    let inst = args
                        .expect("Should exist")
                        .get(0)
                        .expect("Should exist")
                        .to_owned();
                    self.add_inst(self.block_state.curr(), IType::Write { inst })
                }
                PREDEFINED_OUTPUTNEWLINE_ID => {
                    self.add_inst(self.block_state.curr(), IType::WriteNL)
                }
                id => {
                    let block = self.func_map.get(&id)?.clone();
                    if let Some(args) = args {
                        for (idx, &inst) in args.iter().enumerate() {
                            self.load_arg(inst, idx);
                        }
                    }
                    self.add_inst(self.block_state.curr(), IType::Jsr { block })
                }
            };

            Some(Operand::Inst(call))
        } else {
            None
        }
    }

    fn r#if(&mut self) -> Option<()> {
        self.consume(Token::If)?;

        let curr_block = self.block_state.curr();

        let branch_itype = self.relation()?;

        let fall_through_block = self.blocks.add_from(curr_block);

        self.blocks
            .get_mut(curr_block)
            .set_fall_through(fall_through_block);

        let join_block = self.blocks.add_from(curr_block);
        self.init_phi(join_block);

        self.block_state.push(
            BlockFrame::new()
                .conditional()
                .root(curr_block)
                .fall_through(fall_through_block)
                .join(join_block)
                .status(FrameStatus::FallThrough)
                .to_owned(),
        );

        self.consume(Token::Then)?;
        self.block_state.curr = fall_through_block;
        self.stat_sequence()?;
        let fall_through_block = self.block_state.curr();
        // self.block_state
        //     .front_mut()
        //     .status(FrameStatus::FallThrough);

        self.blocks
            .get_mut(fall_through_block)
            .set_fall_through(join_block);

        if self.consume(Token::Else).is_some() {
            let follow_block = self.blocks.add_from(curr_block);

            self.blocks.get_mut(curr_block).set_follow(follow_block);
            self.block_state.front_mut().follow(follow_block);

            // Update branch block
            self.add_inst(curr_block, branch_itype.branch_block(follow_block));

            // Fall through block should skip else block
            self.add_inst(fall_through_block, IType::Bra { block: join_block });

            self.block_state.curr = follow_block;
            self.block_state.front_mut().status(FrameStatus::Follow);
            self.stat_sequence()?;
            let follow_block = self.block_state.curr();
            self.block_state.front_mut().follow(follow_block);

            self.blocks
                .get_mut(follow_block)
                .set_fall_through(join_block);
        } else {
            // Update branch block
            self.block_state.front_mut().follow(join_block);
            self.blocks.get_mut(curr_block).set_follow(join_block);

            self.add_inst(curr_block, branch_itype.branch_block(join_block));
        }

        self.consume(Token::Fi)?;

        self.block_state.pop();
        self.block_state.curr = join_block;

        Some(())
    }

    fn r#while(&mut self) -> Option<()> {
        self.consume(Token::While)?;

        let curr_block = self.block_state.curr();

        let join_block = self.blocks.add_from(self.block_state.curr());
        self.init_phi(join_block);

        self.block_state.curr = join_block;
        let branch_itype = self.relation()?;

        let fall_through_block = self.blocks.add_from(join_block);

        self.blocks.get_mut(curr_block).set_fall_through(join_block);

        self.blocks
            .get_mut(join_block)
            .set_fall_through(fall_through_block);

        self.block_state.push(
            BlockFrame::new()
                .while_loop()
                .root(curr_block)
                .fall_through(fall_through_block)
                .join(join_block)
                .status(FrameStatus::FallThrough)
                .to_owned(),
        );

        self.consume(Token::Do)?;

        self.block_state.curr = fall_through_block;
        self.stat_sequence()?;
        let fall_through_block = self.block_state.curr();
        self.block_state
            .front_mut()
            .fall_through(fall_through_block);

        self.blocks
            .get_mut(fall_through_block)
            .set_fall_through(join_block);

        self.consume(Token::Od)?;

        self.add_inst(fall_through_block, IType::Bra { block: join_block });

        let follow_block = self.blocks.add_from(join_block);
        self.blocks.get_mut(join_block).set_follow(follow_block);

        self.add_inst(join_block, branch_itype.branch_block(follow_block));

        self.block_state.curr = follow_block;
        self.block_state.pop();

        Some(())
    }

    fn r#return(&mut self) -> Option<()> {
        self.consume(Token::Return)?;

        let op = self.expression();
        let inst = op.map(|op| self.op_to_inst(op));

        self.add_inst(self.block_state.curr().clone(), IType::Ret { inst });

        Some(())
    }

    fn statement(&mut self) -> Option<()> {
        match self.tokenizer.peek()? {
            Token::Let => self.assignment(),
            Token::Call => {
                self.func_call()?;
                Some(())
            }
            Token::If => self.r#if(),
            Token::While => self.r#while(),
            Token::Return => self.r#return(),
            _ => None,
        }
    }

    fn stat_sequence(&mut self) -> Option<()> {
        self.statement()?;

        while self.consume(Token::Semicolon).is_some() {
            if let None = self.statement() {
                break;
            }
        }

        Some(())
    }

    fn var_decl(&mut self) -> Option<()> {
        loop {
            if let Token::Ident(var) = self.tokenizer.peek()? {
                self.tokenizer.next();

                self.blocks
                    .get_mut(self.block_state.curr())
                    .declare_var(var);
            } else {
                return None;
            }

            if !self.consume(Token::Comma).is_some() {
                break;
            }
        }

        self.consume(Token::Semicolon)?;

        Some(())
    }

    fn func_decl(&mut self) -> Option<()> {
        let is_void = self.consume(Token::Void).is_some();

        self.consume(Token::Function)?;

        if let Token::Ident(id) = self.tokenizer.peek()? {
            self.tokenizer.next()?;

            // Create new block
            let start_block = self.blocks.add_from(0);
            self.func_map.insert(id, start_block);

            self.block_state.curr = start_block;
            self.curr_init = start_block;

            self.formal_param()?;

            self.consume(Token::Semicolon)?;

            self.func_body()?;

            self.block_state.curr = 1;
            self.curr_init = 1;

            self.consume(Token::Semicolon)?;

            Some(())
        } else {
            return None;
        }
    }

    fn formal_param(&mut self) -> Option<()> {
        self.consume(Token::OpenParen)?;

        let mut argnum = 0;
        if let Some(Token::Ident(id)) = self.tokenizer.peek() {
            self.tokenizer.next()?;

            let getpar_inst = self.declare_arg(argnum);
            argnum += 1;

            self.blocks
                .get_mut(self.block_state.curr())
                .assign_var(id, getpar_inst, None);

            while self.consume(Token::Comma).is_some() {
                if let Token::Ident(id) = self.tokenizer.peek()? {
                    self.tokenizer.next()?;

                    let getpar_inst = self.declare_arg(argnum);
                    argnum += 1;

                    self.blocks
                        .get_mut(self.block_state.curr())
                        .assign_var(id, getpar_inst, None);
                } else {
                    return None;
                }
            }
        }

        self.consume(Token::CloseParen)?;

        Some(())
    }

    fn func_body(&mut self) -> Option<()> {
        if self.consume(Token::Var).is_some() {
            self.var_decl()?;
        }

        self.consume(Token::OpenBrace)?;

        if let Token::CloseBrace = self.tokenizer.peek()? {
            self.tokenizer.next()?;
            return Some(());
        }

        self.stat_sequence()?;
        if !matches!(
            self.blocks
                .get(self.block_state.curr())
                .get_last_inst()
                .map(|inst| self.insts.get(inst).itype()),
            Some(IType::Ret { .. }),
        ) {
            self.add_inst(self.block_state.curr(), IType::Ret { inst: None });
        }

        self.consume(Token::CloseBrace)?;

        Some(())
    }

    pub fn computation(&mut self) -> Option<()> {
        self.consume(Token::Main)?;

        if let Token::Var = self.tokenizer.peek()? {
            self.tokenizer.next();
            self.var_decl()?;
        }

        while matches!(self.tokenizer.peek()?, Token::Void)
            || matches!(self.tokenizer.peek()?, Token::Function)
        {
            self.func_decl()?;
        }

        self.consume(Token::OpenBrace)?;

        self.stat_sequence()?;

        self.consume(Token::CloseBrace)?;

        self.consume(Token::Period)?;

        self.add_inst(self.block_state.curr(), IType::End);

        // self.blocks.resolve_phis();
        self.common_subexpression_elimination();
        self.remove_phis();
        self.common_subexpression_elimination();
        self.fill_empty();

        Some(())
    }

    fn common_subexpression_elimination(&mut self) {
        for inst_id in (0..self.insts.instructions.len()).rev() {
            let itype = self.insts.get(inst_id).itype();
            if let Some(dom_inst) = self.insts.get(inst_id).dom() {
                if let Some(block) = self.insts.get(inst_id).block() {
                    if let Some(cse_inst) = self.insts.get_matching_inst(itype, dom_inst) {
                        self.insts.rename_inst(inst_id, cse_inst);
                        self.blocks.get_mut(block).delete_inst(inst_id);
                        self.insts.remove_block(inst_id);
                    }
                }
            }
        }
    }

    fn remove_phis(&mut self) {
        for inst_id in (0..self.insts.instructions.len()).rev() {
            let itype = self.insts.get(inst_id).itype();
            if let IType::Phi { inst1, inst2, .. } = itype {
                if inst1.0 == inst2.0 {
                    self.insts.rename_inst(inst_id, inst1.0);
                    self.blocks
                        .get_mut(self.insts.get(inst_id).block().unwrap())
                        .delete_inst(inst_id);
                }
            }
        }
    }

    pub fn generate_graph(&self, output_file_path: &str) {
        let mut data = String::new();

        data += "digraph G {\n";

        for block in self.blocks.iter() {
            data += block
                .generate_graph(&self.insts, &self.blocks, &self.tokenizer)
                .as_str();
            data += "\n";
        }

        data += format!("\tbb{}:s -> bb{}:n\n", 0, 1).as_str();
        for block in self.func_map.values() {
            data += format!("\tbb{}:s -> bb{}:n\n", 0, block).as_str();
        }

        data += "}";

        std::fs::write(output_file_path, data).expect("Unable to write file");
    }

    fn fill_empty(&mut self) {
        for block_id in 0..self.blocks.blocks.len() {
            if let None = self.blocks.get(block_id).get_first_inst(&self.insts) {
                self.add_inst(block_id, IType::Empty);
            }
        }
    }

    fn init_phi(&mut self, join_block: usize) {
        for (var, val) in self.blocks.get(self.curr_init).var_map.clone() {
            let dep_var = match val {
                Some((_, dep_var)) => dep_var,
                None => None,
            };
            let val = self.var_to_val(join_block, var);
            let phi = self.insts.add(
                IType::Phi {
                    inst1: (val, dep_var),
                    inst2: (val, dep_var),
                    var,
                },
                None,
                Some(join_block),
            );
            let assignment =
                self.insts
                    .add(IType::Assignment { var, dep_var }, None, Some(join_block));
            self.blocks
                .get_mut(join_block)
                .add_phi(var, phi, dep_var, assignment);
        }
    }

    pub fn generate_instructions(&self, output_file_path: &str) {
        let data = self
            .blocks
            .iter()
            .map(|block| {
                self.insts.generate_instructions(
                    &block.insts,
                    &self.insts,
                    &self.blocks,
                    &self.tokenizer,
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        // .fold(String::new(), |acc, block_str| acc + block_str.as_str());

        std::fs::write(output_file_path, data).expect("Unable to write file");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_var_assignment() {
        let input = b"
    main
    var x;
    {
        let x <- 10
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        // x = 3
        assert_eq!(parser.insts.get(0).itype(), IType::Const(10));
        assert_eq!(parser.blocks.get(1).get_var_inst(3).unwrap(), 0);

        assert_eq!(parser.insts.get(2).itype(), IType::End);
        assert_eq!(parser.blocks.get(1).get_last_inst().unwrap(), 2);

        parser.generate_graph("./tests/basic-var-assignment.dot");
    }

    #[test]
    fn multiple_vars() {
        let input = b"
    main
    var x, y;
    {
        let x <- 10;
        let y <- 12
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        // x = 3, y = 4
        assert_eq!(parser.insts.get(0).itype(), IType::Const(10));
        assert_eq!(parser.blocks.get(1).get_var_inst(3).unwrap(), 0);

        assert_eq!(parser.insts.get(2).itype(), IType::Const(12));
        assert_eq!(parser.blocks.get(1).get_var_inst(4).unwrap(), 2);

        parser.generate_graph("./tests/multiple-vars.dot");
    }

    #[test]
    fn uninitialized_var() {
        let input = b"
    main
    var x, y;
    {
        let x <- y;
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        assert_eq!(parser.insts.get(0).itype(), IType::Const(0));
        assert_eq!(parser.blocks.get(1).get_var_inst(3).unwrap(), 0);
        assert_eq!(parser.blocks.get(1).get_var_inst(4).unwrap(), 0);
    }

    #[test]
    fn func_call() {
        let input = b"
    main
    {
        call InputNum();
        call InputNum
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        assert_eq!(parser.insts.get(0).itype(), IType::Read);
        assert_eq!(parser.insts.get(1).itype(), IType::Read);
    }

    #[test]
    fn arithmetic() {
        let input = b"
    main
    var x, y, z, w, h;
    {
        let x <- 1;
        let y <- 2;
        let z <- 3;
        let w <- 4;
        let h <- x * (z - w / y);
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        // x
        assert_eq!(parser.insts.get(0).itype(), IType::Const(1));
        assert_eq!(parser.blocks.get(1).get_var_inst(3).unwrap(), 0);

        // y
        assert_eq!(parser.insts.get(2).itype(), IType::Const(2));
        assert_eq!(parser.blocks.get(1).get_var_inst(4).unwrap(), 2);

        // z
        assert_eq!(parser.insts.get(4).itype(), IType::Const(3));
        assert_eq!(parser.blocks.get(1).get_var_inst(5).unwrap(), 4);

        // w
        assert_eq!(parser.insts.get(6).itype(), IType::Const(4));
        assert_eq!(parser.blocks.get(1).get_var_inst(6).unwrap(), 6);

        // h: Mul(x, Sub(z, Div(w, y)))
        assert!(matches!(parser.insts.get(8).itype(), IType::Div { .. }));
        assert!(matches!(parser.insts.get(9).itype(), IType::Sub { .. }));
        assert!(matches!(parser.insts.get(10).itype(), IType::Mul { .. }));
        assert_eq!(parser.blocks.get(1).get_var_inst(7).unwrap(), 10);

        parser.generate_graph("./tests/arithmetic.dot");
    }

    #[test]
    fn basic_if_else() {
        let input = b"
    main
    var a, b, c, d, e;
    {
        let a <- call InputNum();
        let b <- a;
        let c <- b;
        let d <- b + c;
        let e <- a + b;
        if a < 0 then
            let a <- d + e;
        else
            let d <- e + 1;
        fi;
        call OutputNum(a);
        call OutputNum(d);
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        // // a, b, c
        // assert_eq!(parser.insts.get(0).itype(), IType::Read);
        // assert_eq!(parser.blocks.get(1).get_var_inst(3).unwrap(), 0);
        // assert_eq!(parser.blocks.get(1).get_var_inst(4).unwrap(), 0);
        // assert_eq!(parser.blocks.get(1).get_var_inst(5).unwrap(), 0);
        //
        // // d, e
        // assert!(matches!(parser.insts.get(4).itype(), IType::Add { .. }));
        // assert_eq!(parser.blocks.get(1).get_var_inst(6).unwrap(), 4);
        // assert_eq!(parser.blocks.get(1).get_var_inst(7).unwrap(), 4);
        //
        // // if
        // assert_eq!(parser.insts.get(7).itype(), IType::Const(0));
        // assert!(matches!(parser.insts.get(8).itype(), IType::Cmp { .. }));
        // assert!(matches!(parser.insts.get(12).itype(), IType::Bge { .. }));
        //
        // // a
        // assert!(matches!(parser.insts.get(9).itype(), IType::Add { .. }));
        // assert_eq!(parser.blocks.get(2).get_var_inst(3).unwrap(), 9);
        //
        // // endif
        // assert!(matches!(parser.insts.get(13).itype(), IType::Bra { .. }));
        // assert_eq!(parser.blocks.get(2).get_last_inst().unwrap(), 13);
        //
        // // else
        // assert!(matches!(parser.insts.get(15).itype(), IType::Add { .. }));
        // assert_eq!(parser.blocks.get(4).get_var_inst(6).unwrap(), 15);
        //
        // // Phi: a
        // assert!(matches!(parser.insts.get(11).itype(), IType::Phi { .. }));
        // assert_eq!(parser.blocks.get(3).get_var_inst(3).unwrap(), 11);
        //
        // // Phi: d
        // assert!(matches!(parser.insts.get(17).itype(), IType::Phi { .. }));
        // assert_eq!(parser.blocks.get(3).get_var_inst(6).unwrap(), 17);

        parser.generate_graph("./tests/basic-if-else.dot");
    }

    #[test]
    fn basic_while() {
        let input = b"
    main
    var x, y, i, j;
    {
        let i <- call InputNum();
        let x <- 0;
        let y <- 0;
        let j <- i;

        while x < 10
        do
            let x <- i + 1;
            let y <- j + 1;
            let i <- i + 1;
        od;

        call OutputNum(x);
        call OutputNum(y);
        call OutputNum(i);
        call OutputNum(j);
    }
    .
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/basic-while.dot");
    }

    #[test]
    fn nested_while() {
        let input = b"
    main
    var x, y, i, j;
    {
        let i <- call InputNum();
        let x <- 0;
        let y <- 0;
        let j <- i;

        while x < 10
        do
            let x <- x + 1;
            let y <- j + 1;

            while j < 10
            do
                let x <- j + 1;
                let y <- i + 1;
                let j <- i + 1;
            od;

            let i <- i + 1;
        od;

        call OutputNum(x);
    }
    .
    ";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/nested-while.dot");
    }

    #[test]
    fn nested_while2() {
        let input = b"
    main
    var x, y, i, j;
    {
        let i <- call InputNum();
        let x <- 0;
        let y <- 0;
        let j <- i;

        while x < 10
        do
            let x <- i + 1;
            let y <- j + 1;

            while j < 10
            do
                let x <- j + 1;
                let y <- i + 1;
                let j <- j + 1;
            od;

            let i <- i + 1;
        od;

        call OutputNum(x);
    }
    .
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/nested-while2.dot");
        // assert!(false);
    }

    #[test]
    fn nested_while_if() {
        let input = b"
    main
    var i;
    {
        let i <- 0;
        while i < 10 do
            let i <- i + 1;
            if i == 2 then
                let i <- i + 1
            fi
        od;
        call OutputNum(i)
    }
    .
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/nested-while-if.dot");
    }

    #[test]
    fn nested_if_while() {
        let input = b"
    main
    var i;
    {
        let i <- 0;
        if i == 2 then
            while i < 10 do
                let i <- i + 1;
                let i <- i + 1
            od;
        fi;
        call OutputNum(i)
    }
    .
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/nested-if-while.dot");
    }

    #[test]
    fn nested_big() {
        let input = b"
main
var x,k,j,m;
{
let x <- call InputNum();
let k <- call InputNum();
while x < 10 do
    let j <- 0;
    let m <- 0;

    let x <- x + 1;
    if k > 5 then
        while j < 15 do
            let j <- j + 1;
        od;
        let k <- 0;
    else
        let k <- k + 1;
        while m < 20 do
            let m <- m * 2;
        od;
    fi;
od;
call OutputNum(x);
call OutputNum(k);
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/nested-big.dot");
    }

    #[test]
    fn nested_ifs() {
        let input = b"
main
var a, b;
{
    let a <- call InputNum();
    let b <- call InputNum();
    if a > 0 then
        if b > 0 then
            call OutputNum(0)
        else
            call OutputNum(1)
        fi
    else
        if b > 0 then
            call OutputNum(1)
        else
            call OutputNum(0)
        fi
    fi
}.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/nested-ifs.dot");
    }

    #[test]
    fn test_cse() {
        let input = b"
main
var a,b,c,d,e;
{
    let a <- call InputNum();
    let b <- a;
    let c <- b;
    let d <- b + c;
    let e <- a + b;
    if a < 0 then
        let d <-  d + e;
        let a <- d
    else
        let d <- e
    fi;
    call OutputNum(a)
}.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/test-cse.dot");
    }

    #[test]
    fn copy_loop() {
        let input = b"
main
var fa, fb, n, t;
{
    let fa <- 0;
    let fb <- 1;
    let t <- fa + fb;
    let n <- call InputNum();
    let t <- n - 1;
    while n > 0 do
        let t <- fb;
        let fb <- fa + fb;
        let fa <- t;
        let n <- n - 1;
    od;
    call OutputNum(fa);
    call OutputNewLine();
}
.";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/copy-loop.dot");
    }

    #[test]
    fn complex_phi() {
        let input = b"
main
var x,k,j,m;
{
let x <- call InputNum;
let k <- call InputNum();
while x < 10 do
    let j <- 0;
    let m <- 1;

    let x <- x + 1;
    call OutputNum(111);
    call OutputNewLine();
    if k > 5 then
        while j < 15 do
            let j <- j + k;
            while m < 20 do
                let k <- k * 1;
                let m <- m + 1
            od
        od;
        let k <- k - 1;
        call OutputNum(k);
        call OutputNewLine
    else
        while m < 20 do
            let m <- m + k
        od;
        let k <- k + 1;
        call OutputNum(k);
        call OutputNewLine
    fi;
    call OutputNum(k);
    call OutputNewLine
od;
call OutputNum(x);
call OutputNewLine;
call OutputNum(k);
call OutputNewLine
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/complex-phi.dot");
    }

    #[test]
    fn complex_if() {
        let input = b"
main
var a, b, sum, i, j;
{
    let a <- call InputNum();
    let b <- call InputNum();
    let sum <- 0;
    let i <- 1;
    while i <= a do
        let j <- 1;
        while j <= b do
            if i * j == 3 then
                let sum <- sum + i * j;
            fi;
            if i * j == 6 then
                let sum <- sum + i * j;
            fi;
            if i * j == 9 then
                let sum <- sum + i * j;
            fi;
            let j <- j + 1;
        od;
        let i <- i + 1;
    od;
    call OutputNum(sum);
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/complex-if.dot");
        // assert!(false);
    }

    #[test]
    fn basic_func() {
        let input = b"
main
var x;

function add(a, b); {
    return a + b
};

void function println(n); {
    call OutputNum(n);
    call OutputNewLine
};

{
    let x <- call add(1, 1);
    call println(x)
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/basic-func.dot");
    }

    #[test]
    fn fib() {
        let input = b"
main
var x;

function fibonacci(n); {
    if n <= 1 then
        return n
    fi;
    return call fibonacci(n - 1) + call fibonacci(n - 2)
};

{
    let x <- call InputNum;
    let x <- call fibonacci(x);
    call OutputNum(x);
    call OutputNewLine
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);
        // assert!(false);

        parser.generate_graph("./tests/fib.dot");
    }

    #[test]
    fn complex_func() {
        let input = b"
main
var a, six, g, red;


function retfunc(x);
{

    return x;
};

void function emptyfunc();
var trippy, ball, count;
{

    let trippy <- 25;
    if 10 > 3 then
    let trippy <- 40;
    let ball <- 30;
    else
    let ball <- 4;
    fi;

    let count <- 0;
    while count <= 5 do
    let count <- count + 1;
    od;


let count <- 0;
    while count <= 5 do
    let count <- count + 1;
    od;

};




{
let red <- 12;




call emptyfunc();


let a <- 900;

let six <- call retfunc(9);

let g <- call retfunc(9 + 9);





}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/complex-func.dot");
        // assert!(false);
    }

    #[test]
    fn func_gcd() {
        let input = b"
main
function mod(x,y); {
    if y == 0 then
        return x;
    fi;
    while x < 0 do
        let x <- x + y;
    od;
    while x >= y do
        let x <- x - y;
    od;
    return x;
};
function gcd(x,y); {
    if x == 0 then
        return y;
    fi;
    return call gcd(y, call mod(x,y));
};
{
    call OutputNum(call gcd(110,121));
    call OutputNewLine();
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/func-gcd.dot");
        // assert!(false);
    }

    #[test]
    fn func_mandlebrot() {
        let input = b"
main
var px, py, mval;

function mandelbrot(x,y);
var iters, x2, go,x0,y0; 
{
    let x0 <- x;
    let y0 <- y;
    let iters <- 0;
    let go <- 1;
    while go != 0 do
        if x*x+y*y > 4*10000*10000 then
            let go <- 0;
        fi;
        if iters >= 100 then
            let go <- 0;
        fi;
        if go != 0 then
            let x2 <- (x*x-y*y)/10000 + x0;
            let y <- (2*x*y)/10000 + y0;
            let x <- x2;
            let iters <- iters+1;
        fi;
    od;

    return iters;
};


{
    let px <- 0;
    let py <- 0;
    while py < 200 do
        let px <- 0;
        while px < 200 do
            let mval <- call mandelbrot( ((px-100)*4*10000)/200, ((py-100)*4*10000)/200);
            if mval == 100 then
                call OutputNum(8);
            else
                call OutputNum(1);
            fi;
            let px <- px + 1;
        od;
        let py <- py + 1;
        call OutputNewLine();
    od;


}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/func-mandlebrot.dot");
        // assert!(false);
    }

    #[test]
    fn triple_while() {
        let input = b"
main
var count, i, j, k, ilim, jlim, klim;
{
    let ilim <- call InputNum();
    let jlim <- call InputNum();
    let klim <- call InputNum();

    let count <- 0;
    let i <- 0;
    while i < ilim do
        let j <- 0;

        while j < jlim do
            let k <- 0;

            while k < klim do
                let k <- k + 1;

                if (i + j + k) / 100 < 50 then
                    let count <- count + 1;
                fi;
            od;

            let j <- j + 1;
        od;

        let i <- i + 1;
    od;

    call OutputNum(count);
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/triple-while.dot");
        // assert!(false);
    }

    #[test]
    fn func_triple_while() {
        let input = b"
main

void function loop(ilim, jlim, klim);
var count, i, j, k;
{
    let count <- 0;
    let i <- 0;
    while i < ilim do
        let j <- 0;

        while j < jlim do
            let k <- 0;

            while k < klim do
                let k <- k + 1;

                if (i + j + k) / 100 < 50 then
                    let count <- count + 1;
                fi;
            od;

            let j <- j + 1;
        od;

        let i <- i + 1;
    od;

    call OutputNum(count);
};

{
    call loop(100, 200, 300);
}
.
";
        let mut parser = Parser::new(&input[..]);

        parser.computation();
        // dbg!(&parser);

        parser.generate_graph("./tests/func-triple-while.dot");
        // assert!(false);
    }
}
