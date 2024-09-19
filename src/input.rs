use std::io::BufRead;

#[derive(Debug, PartialEq)]
pub struct Input<R: BufRead> {
    input: R,
    curr: Option<u8>,
}

impl<R: BufRead> Input<R> {
    pub fn new(input: R) -> Input<R> {
        let mut res = Input { input, curr: None };
        res.next();
        res
    }

    fn read_char(&mut self) -> Option<u8> {
        let mut byte = [0_u8];
        match self.input.read_exact(&mut byte) {
            Ok(()) => Some(byte[0]),
            _ => None,
        }
    }

    pub fn peek(&mut self) -> Option<u8> {
        self.curr
    }

    pub fn next(&mut self) -> Option<u8> {
        let old = self.curr;
        self.curr = self.read_char();
        old
    }
}
