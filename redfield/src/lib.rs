use std::collections::HashMap;
type Result<T> = std::result::Result<T, Error>;
#[derive(Debug)]
pub struct Error(ErrorKind);

#[derive(Debug, Clone, Copy)]
pub enum WireType {
    OneByte,
    TwoByte,
    FourByte,
    EightByte,
    LengthPrefix,
    Null,
}

pub const WIRE_TYPE_MASK: u8 = 0b111;

#[derive(Debug)]
pub struct FieldNumber(pub u16);

impl WireType {
    fn from_byte(b: u8) -> Result<WireType> {
        match b {
            0 => Ok(WireType::OneByte),
            1 => Ok(WireType::TwoByte),
            2 => Ok(WireType::FourByte),
            3 => Ok(WireType::EightByte),
            4 => Ok(WireType::LengthPrefix),
            5 => Ok(WireType::Null),
            _ => Err(Error(ErrorKind::InvalidWireType(b))),
        }
    }
}

pub fn read_tag(s: &[u8]) -> Result<(FieldNumber, WireType, &[u8])> {
    match *s {
        [a, b, ref rest @ ..] => {
            let raw_tag = u16::from_le_bytes([a, b]);
            let field_number = raw_tag >> 3;
            let wire_type = raw_tag as u8 & WIRE_TYPE_MASK;
            let wt = WireType::from_byte(wire_type)?;
            Ok((FieldNumber(field_number), wt, rest))
        }
        _ => Err(Error(ErrorKind::NotEnoughBytes)),
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum UnknownValue {
    Null,
    OneByte(u8),
    TwoByte(u16),
    FourByte(u32),
    EightByte(u64),
    LengthPrefix(Vec<u8>),
}

impl Encode for UnknownFields {
    fn encode_to_buf(&self, buf: &mut Vec<u8>) -> Result<()> {
        Ok(for (tag, value) in self.0.iter() {
            write_u16(*tag, buf);
            match value {
                UnknownValue::Null => {}
                UnknownValue::OneByte(b) => write_u8(*b, buf),
                UnknownValue::TwoByte(b) => write_u16(*b, buf),
                UnknownValue::FourByte(b) => write_u32(*b, buf),
                UnknownValue::EightByte(b) => write_u64(*b, buf),
                UnknownValue::LengthPrefix(b) => write_bytes(b.as_slice(), buf),
            }
        })
    }
}
/// A container for fields that were present in the encoded form
/// but are not known by the decoder.
#[derive(Debug)]
pub struct UnknownFields(HashMap<u16, UnknownValue>);
impl UnknownFields {
    pub fn insert<'a>(&mut self, field_number: u16, wt: WireType, s: &'a [u8]) -> Result<&'a [u8]> {
        let tag = field_number << 3 & wt as u16;
        let (value, rest) = match wt {
            WireType::OneByte => {
                let (b, rest) = read_u8(s)?;
                (UnknownValue::OneByte(b), rest)
            }
            WireType::TwoByte => {
                let (b, rest) = read_u16(s)?;
                (UnknownValue::TwoByte(b), rest)
            }
            WireType::FourByte => {
                let (b, rest) = read_u32(s)?;
                (UnknownValue::FourByte(b), rest)
            }
            WireType::EightByte => {
                let (b, rest) = read_u64(s)?;
                (UnknownValue::EightByte(b), rest)
            }
            WireType::LengthPrefix => {
                let (len, rest) = read_u32(s)?;
                let (bytes, rest) = read_n(rest, len as usize)?;
                (UnknownValue::LengthPrefix(bytes.to_vec()), rest)
            }
            WireType::Null => (UnknownValue::Null, s),
        };
        self.0.insert(tag, value);
        Ok(rest)
    }
}

pub fn read_bool(s: &[u8]) -> Result<(bool, &[u8])> {
    let (&b, rest) = s.split_first().ok_or(Error(ErrorKind::NotEnoughBytes))?;
    if b == 0 {
        Ok((false, rest))
    } else if b == 1 {
        Ok((true, rest))
    } else {
        Err(Error(ErrorKind::InvalidBool))
    }
}

pub fn write_bool(b: bool, buf: &mut Vec<u8>) {
    buf.push(b as u8);
}

pub fn read_i8(s: &[u8]) -> Result<(i8, &[u8])> {
    const N: usize = std::mem::size_of::<i8>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((i8::from_le_bytes(buf), rest))
}

pub fn write_i8(n: i8, buf: &mut Vec<u8>) {
    buf.push(n as u8);
}

pub fn read_u8(s: &[u8]) -> Result<(u8, &[u8])> {
    const N: usize = std::mem::size_of::<i8>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((u8::from_le_bytes(buf), rest))
}

pub fn write_u8(b: u8, buf: &mut Vec<u8>) {
    buf.push(b);
}

pub fn read_i16(s: &[u8]) -> Result<(i16, &[u8])> {
    const N: usize = std::mem::size_of::<i16>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((i16::from_le_bytes(buf), rest))
}

pub fn write_i16(n: i16, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_u16(s: &[u8]) -> Result<(u16, &[u8])> {
    const N: usize = std::mem::size_of::<u16>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((u16::from_le_bytes(buf), rest))
}

pub fn write_u16(n: u16, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_i32(s: &[u8]) -> Result<(i32, &[u8])> {
    const N: usize = std::mem::size_of::<i32>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((i32::from_le_bytes(buf), rest))
}

pub fn write_i32(n: i32, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_u32(s: &[u8]) -> Result<(u32, &[u8])> {
    const N: usize = std::mem::size_of::<u32>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((u32::from_le_bytes(buf), rest))
}

pub fn write_u32(n: u32, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_f32(s: &[u8]) -> Result<(f32, &[u8])> {
    const N: usize = std::mem::size_of::<f32>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((f32::from_le_bytes(buf), rest))
}

pub fn write_f32(n: f32, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_i64(s: &[u8]) -> Result<(i64, &[u8])> {
    const N: usize = std::mem::size_of::<i64>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((i64::from_le_bytes(buf), rest))
}

pub fn write_i64(n: i64, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_u64(s: &[u8]) -> Result<(u64, &[u8])> {
    const N: usize = std::mem::size_of::<u64>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((u64::from_le_bytes(buf), rest))
}

pub fn write_u64(n: u64, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_f64(s: &[u8]) -> Result<(f64, &[u8])> {
    const N: usize = std::mem::size_of::<f64>();
    let (b, rest) = s
        .split_at_checked(N)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    let mut buf: [u8; N] = Default::default();
    buf.copy_from_slice(b);
    Ok((f64::from_le_bytes(buf), rest))
}

pub fn write_f64(n: f64, buf: &mut Vec<u8>) {
    buf.extend(n.to_le_bytes())
}

pub fn read_str(s: &[u8], len: usize) -> Result<(&str, &[u8])> {
    let (b, rest) = read_n(s, len)?;
    let s = std::str::from_utf8(b).map_err(|_| Error(ErrorKind::InvalidUtf8))?;
    Ok((s, rest))
}

pub fn write_bytes(s: &[u8], buf: &mut Vec<u8>) {
    if s.len() > u32::MAX as usize {
        panic!("length exceeds u32::MAX");
    }
    let len = s.len() as u32;
    write_u32(len, buf);
    buf.extend(s.iter().copied())
}

pub fn read_n(s: &[u8], len: usize) -> Result<(&[u8], &[u8])> {
    let (b, rest) = s
        .split_at_checked(len)
        .ok_or(Error(ErrorKind::NotEnoughBytes))?;
    Ok((b, rest))
}

#[derive(Debug)]
pub enum ErrorKind {
    WrongWireType(WireType),
    InvalidWireType(u8),
    InvalidBool,
    NotEnoughBytes,
    TooManyBytes,
    InvalidUtf8,
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ErrorKind::WrongWireType(t) => write!(f, "unexpected wire type: got {:?}", t)?,
            ErrorKind::InvalidWireType(b) => write!(f, "invalid wire type: got {}", b)?,
            ErrorKind::InvalidBool => write!(f, "invalid value for bool: must be 1 or 0")?,
            ErrorKind::NotEnoughBytes => write!(f, "input too short")?,
            ErrorKind::TooManyBytes => write!(f, "input too long")?,
            ErrorKind::InvalidUtf8 => write!(f, "invalid utf8")?,
        };
        Ok(())
    }
}

pub trait Encode {
    fn encode_to_buf(&self, buf: &mut Vec<u8>) -> Result<()>;
    fn encode(&self) -> Result<Vec<u8>> {
        let mut buf = vec![];
        self.encode_to_buf(&mut buf)?;
        Ok(buf)
    }
}

pub trait Decode: Sized {
    fn decode(s: &[u8]) -> Result<Self>;
}
