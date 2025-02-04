use std::io::{Error as IoError, Read, Write};

type Result<T> = std::result::Result<T, Error>;
#[derive(Debug)]
pub struct Error(ErrorKind);

#[derive(strum_macros::FromRepr, Debug, Clone, Copy)]
#[repr(u8)]
pub enum WireType {
    /// Types that are one byte long: `u8`, `i8`, and `bool`.
    OneByte = 0,
    /// Types that are two bytes long: `u16` and `i16`.
    TwoByte = 1,
    /// Types that are four bytes long: `u32`, `i32` and `f32`.
    FourByte = 2,
    /// Types that are eight bytes long: `u64`, `i64`, and `f64`.
    EightByte = 3,
    /// Bytes or Strings that whose length fits in a `u8`.
    BytesShort = 4,
    /// Bytes or Strings that whose length fits in a `u16`.
    BytesMedium = 5,
    /// Bytes or Strings that whose length fits in a `u32`.
    BytesLong = 6,
    /// Lists whose length fits in a `u8`.
    ListShort = 7,
    /// Lists whose length fits in a `u16`.
    ListMedium = 8,
    /// Lists whose length fits in a `u32`.
    ListLong = 9,
    /// Messages with an amonut of fields that fits in a `u8`.
    MessageShort = 10,
    /// Messages with an amonut of fields that fits in a `u16`.
    MessageLong = 11,
    /// One tag-value pair.
    OneOf = 12,
    /// Null.
    Null = 13,
}

pub const WIRE_TYPE_MASK: u8 = 0b1111;

#[derive(Debug)]
pub struct FieldNumber(pub u16);

impl WireType {
    fn from_byte(b: u8) -> Result<WireType> {
        WireType::from_repr(b).ok_or(Error(ErrorKind::InvalidWireType(b)))
    }
}

pub fn read_tag<R: Read>(mut r: R) -> Result<(FieldNumber, WireType)> {
    let mut buf = [0u8; 2];
    r.read_exact(&mut buf)
        .map_err(|io| Error(ErrorKind::Io(io)))?;
    let tag = u16::from_le_bytes(buf);
    let field_number = tag >> 4;
    let wt = tag as u8 & WIRE_TYPE_MASK;
    let wt = WireType::from_byte(wt)?;
    Ok((FieldNumber(field_number), wt))
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

macro_rules! read_write_as_le {
    ($($ty:ty),* $(,)?) => {
        paste::paste! {
            $(
                pub fn [<write_ $ty>]<W: Write>(n: $ty, mut w: W) -> Result<()> {
                    let b = $ty::to_le_bytes(n);
                    w.write_all(&b).map_err(|e| Error(ErrorKind::Io(e)))?;
                    Ok(())
                }
                pub fn [<read_ $ty>]<R: std::io::Read>(mut r: R) -> Result<$ty> {
                    let mut buf = [0u8; std::mem::size_of::<$ty>()];
                    r.read_exact(&mut buf).map_err(|e| Error(ErrorKind::Io(e)))?;
                    Ok($ty::from_le_bytes(buf))
                }
            )*
        }
    };
}

read_write_as_le!(u8, i8, u16, i16, u32, i32, f32, u64, i64, f64);

pub fn read_str(s: &[u8], len: usize) -> Result<(&str, &[u8])> {
    let (b, rest) = read_n(s, len)?;
    let s = std::str::from_utf8(b).map_err(|_| Error(ErrorKind::InvalidUtf8))?;
    Ok((s, rest))
}

pub fn write_bytes<W: Write>(s: &[u8], mut w: W) -> Result<()> {
    if s.len() > u32::MAX as usize {
        panic!("length exceeds u32::MAX");
    }
    let len = s.len();
    if len < (u8::MAX as usize + 1) {
        let len = len as u8;
        write_u8(len, &mut w)?;
    } else if len < (u16::MAX as usize) + 1 {
        let len = len as u16;
        write_u16(len, &mut w)?;
    } else {
        write_u32(len as u32, &mut w)?;
    }
    w.write_all(s).map_err(|e| Error(ErrorKind::Io(e)))?;
    Ok(())
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
    Io(IoError),
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
            ErrorKind::Io(ref e) => write!(f, "io error: {}", e)?,
        };
        Ok(())
    }
}

pub trait Encode {
    fn encode<W: Write>(&self, w: W) -> Result<()>;
    fn encode_to_buf(&self, buf: &mut Vec<u8>) -> Result<()> {
        Encode::encode(self, buf)?;
        Ok(())
    }
    fn encode_to_vec(&self) -> Result<Vec<u8>> {
        let mut buf = vec![];
        self.encode_to_buf(&mut buf)?;
        Ok(buf)
    }
}

pub trait Decode: Sized {
    fn decode<R: Read>(r: R) -> Result<Self>;
    fn decode_from_buf(s: &[u8]) -> Result<Self> {
        let c = std::io::Cursor::new(s);
        Ok(Decode::decode(c)?)
    }
}

impl Encode for redfield_parse::Document {
    fn encode<W: Write>(&self, w: W) -> Result<()> {
        todo!()
    }
}
