use std::{collections::HashMap, fs::{self, File}, io::Write};

#[allow(dead_code)] // TODO: remove
#[repr(u8)]
pub enum Instruction {
    Nop = 0x00,
    Br(i16),
    BrTrue(i16),
    BrFalse(i16),
    Exit,

    Call(i16) = 0x06,
    Ret,
    Pop,
    LocalGet(i16),
    LocalSet(i16),

    I8Const(i8) = 0x0C,
    I16Const(i16),
    I32Const(i32),
    I64Const(i64),

    I32Eqz = 0x10,
    I32Eq,
    I32Ne,
    I32Lt,
    U32Lt,
    I32Gt,
    U32GT,
    I32Le,
    U32Le,
    I32Ge,
    U32Ge,

    I64Eq = 0x1C,
    I64Ne,
    I64Lt,
    U64Lt,
    I64Gt,
    U64GT,
    I64Le,
    U64Le,
    I64Ge,
    U64Ge,
    
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    U32Div,
    I32Rem,
    U32Rem,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32Shr,
    U32Shr,

    I64Add,
    I64Sub,
    I64Mul,
    I64Div,
    U64Div,
    I64Rem,
    U64Rem,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64Shr,
    U64Shr,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,

    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
}

impl Instruction {
    fn tag(&self) -> u8 {
        unsafe { *(self as *const Instruction as *const u8) }
    }

    fn write_to(&self, buffer: &mut VarIntBuffer) {
        buffer.write_u8(self.tag());

        match self {
            Instruction::Br(offset) => buffer.write_i16(*offset),
            Instruction::BrTrue(offset) => buffer.write_i16(*offset),
            Instruction::BrFalse(offset) => buffer.write_i16(*offset),
            Instruction::Call(index) => buffer.write_i16(*index),
            Instruction::LocalGet(index) => buffer.write_i16(*index),
            Instruction::LocalSet(index) => buffer.write_i16(*index),
            Instruction::I8Const(value) => buffer.write_i8(*value),
            Instruction::I16Const(value) => buffer.write_i16(*value),
            Instruction::I32Const(value) => buffer.write_i32(*value),
            Instruction::I64Const(value) => buffer.write_i64(*value),
            _ => {}
        }
    }
}

pub struct Module<'a> {
    pub funcs: Vec<&'a Func>
}

impl<'a> Module<'a> {
    pub fn write_to(&self, path: &str) {
        let mut buffer = VarIntBuffer::new();
        buffer.extend(&[b'r', b'e', b'd', b'y', b'l', b'a', b'n', b'g']);// magic number
        buffer.write_u64(0); // version
        buffer.write_u16(self.funcs.len() as u16);

        buffer.advance_by(self.funcs.len() * 8);

        let mut func_offset: u32 = 0;

        for (i, func) in self.funcs.iter().enumerate() {
            let start = i * 8 + 18;
            buffer.write_u16_at(func.args_count, start);
            buffer.write_u16_at(func.locals_count, start + 2);
            buffer.write_u32_at(func_offset, start + 4);

            let len = buffer.len();
            func.write_to(&mut buffer);
            func_offset += (buffer.len() - len) as u32;
        }
        let mut file = File::create(path).unwrap();
        file.write_all(buffer.as_ref()).unwrap();
    }
}


pub struct Func {
    pub args_count: u16,
    pub locals_count: u16,
    pub instructions: Vec<Instruction>,
}

impl Func {
    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn write_to(&self, buffer: &mut VarIntBuffer) {
        for instruction in &self.instructions {
            instruction.write_to(buffer);
        }
    }
}

struct VarIntBuffer {
    buffer: Vec<u8>,
}

#[allow(dead_code)]
impl VarIntBuffer {
    fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    fn write_u8(&mut self, value: u8) {
        self.buffer.push(value);
    }

    fn write_u16(&mut self, value: u16) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_u32(&mut self, value: u32) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_u64(&mut self, value: u64) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_i8(&mut self, value: i8) {
        self.buffer.push(value.to_le_bytes()[0]);
    }

    fn write_i16(&mut self, value: i16) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_i32(&mut self, value: i32) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_i64(&mut self, value: i64) {
        self.buffer.extend(value.to_le_bytes());
    }

    fn write_u16_at(&mut self, value: u16, index: usize) {
        self.buffer[index..index + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn write_u32_at(&mut self, value: u32, index: usize) {
        self.buffer[index..index + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn advance_by(&mut self, amount: usize) {
        self.buffer.resize(self.buffer.len() + amount, 0);
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buffer.extend(bytes);
    }

    fn len(&self) -> usize {
        self.buffer.len()
    }
}

impl AsRef<[u8]> for VarIntBuffer {
    fn as_ref(&self) -> &[u8] {
        &self.buffer
    }
}