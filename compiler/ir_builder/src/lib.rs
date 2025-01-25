use std::{
    cell::RefCell,
    fmt::{self, Display},
    iter::Peekable,
    num::ParseIntError,
    sync::Arc,
};

use gxhash::{HashMap, HashSet, HashSetExt};

pub type ImutStr = Arc<str>;
pub type StructPool = HashMap<String, (Vec<String>, Vec<Argument>)>;

pub static VOID_POINTER_ID: &str = "__void_ptr__";
pub static POINTER_ID: &str = "__ptr__";
pub static GENERIC_POINTER: &str = "2"; // Pointer to another type
pub static GENERIC_UNKNOWN: &str = "3"; // Unknown type T
pub static GENERIC_IDENTIFIER: &str = "0"; // Start of a generic
pub static GENERIC_END: &str = "1"; // Allowing for nested generic structs
pub static EQUALS_CONSTANT: &str = "__equals__";
pub static GC_NOOP: &str = "__internal_gc_noop";

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub name: String,
    pub ty: Type,
    pub manual: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Linkage {
    pub exported: bool,
    pub section: Option<ImutStr>,
    pub secflags: Option<ImutStr>,
}

impl Linkage {
    pub fn private() -> Linkage {
        Linkage {
            exported: false,
            section: None,
            secflags: None,
        }
    }

    pub fn public() -> Linkage {
        Linkage {
            exported: true,
            section: None,
            secflags: None,
        }
    }
}

impl Display for Linkage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.exported {
            write!(f, "export ")?;
        }

        if let Some(section) = &self.section {
            write!(f, "section \"{}\"", section)?;

            if let Some(secflags) = &self.secflags {
                write!(f, "\"{}\"", secflags)?;
            }

            write!(f, " ")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Block {
    pub label: ImutStr,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn add_instruction(&mut self, instr: Instruction) {
        self.statements.push(Statement::Volatile(instr));
    }

    pub fn assign_instruction(&mut self, temp: &Value, ty: &Type, instr: Instruction) {
        self.statements.push(Statement::Assign(
            temp.to_owned(),
            ty.to_owned().into_abi(),
            instr,
        ));
    }

    pub fn assign_instruction_front(&mut self, temp: &Value, ty: &Type, instr: Instruction) {
        self.statements.insert(
            0,
            Statement::Assign(temp.to_owned(), ty.to_owned().into_abi(), instr),
        );
    }

    pub fn jumps(&self) -> bool {
        let last = self.statements.last();

        if let Some(Statement::Volatile(instr)) = last {
            matches!(
                instr,
                Instruction::Ret(_) | Instruction::Jmp(_) | Instruction::Jnz(..) | Instruction::Hlt
            )
        } else {
            false
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "@{}", self.label)?;

        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(
                    |instr| if let Statement::Assign(val, ty, ins) = instr.clone() {
                        if matches!(ins, Instruction::Copy(_) | Instruction::Load(_, _)) {
                            Statement::Assign(val, ty.into_base(), ins)
                        } else {
                            instr.clone()
                        }
                    } else {
                        instr.clone()
                    }
                )
                .map(|instr| format!("\t{}", instr))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(Value, Type, Instruction),
    Volatile(Instruction),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign(temp, ty, instr) => {
                assert!(matches!(temp, Value::Temp(_)));
                write!(f, "{} ={} {}", temp, ty, instr)
            }
            Self::Volatile(instr) => write!(f, "{instr}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Function {
    pub linkage: Linkage,
    pub name: ImutStr,
    pub parameters: Vec<(Type, Value)>,
    pub return_type: Option<Type>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn add_block(&mut self, label: impl Into<String>) -> &mut Block {
        self.blocks.push(Block {
            label: Into::into(label.into()),
            statements: vec![],
        });

        self.blocks.last_mut().unwrap()
    }

    pub fn last_block(&self) -> &Block {
        self.blocks.last().unwrap()
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .add_instruction(instruction);
    }

    pub fn assign_instruction(&mut self, temp: &Value, ty: &Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .assign_instruction(temp, ty, instruction);
    }

    pub fn assign_instruction_front(&mut self, temp: &Value, ty: &Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .assign_instruction_front(temp, ty, instruction);
    }

    pub fn returns_at_all(&self) -> bool {
        self.blocks.iter().any(|block| {
            block.statements.iter().any(|i| {
                matches!(
                    i,
                    Statement::Volatile(Instruction::Ret(_))
                        | Statement::Volatile(Instruction::Hlt)
                )
            })
        })
    }

    pub fn returns(&self) -> bool {
        let last = self.last_block().statements.last();

        last.is_some_and(|i| matches!(i, Statement::Volatile(Instruction::Ret(_))))
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}function", self.linkage)?;

        if let Some(ty) = &self.return_type {
            write!(f, " {}", ty.clone().into_abi())?;
        }

        let params = self
            .parameters
            .iter()
            .map(|(ty, temp)| format!("{} {}", ty, temp))
            .collect::<Vec<String>>()
            .clone();

        writeln!(
            f,
            " ${name}({args}) {{",
            name = self.name,
            args = params.join(", ")
        )?;

        for blk in self.blocks.iter() {
            writeln!(f, "{}", blk)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// `ub` -- Unsigned 8-bit integer
    UnsignedByte,
    /// `uh` -- Unsigned 16-bit integer
    UnsignedHalfword,
    /// `uw` -- Unsigned 32-bit integer
    UnsignedWord,
    /// `ul` -- Unsigned 64-bit integer
    UnsignedLong,
    /// `b` -- Signed 8-bit integer
    Byte,
    /// `h` -- Signed 16-bit integer
    Halfword,
    /// `bool` - Aggregate type?
    Boolean,
    /// `w` -- Signed 32-bit integer
    Word,
    /// `l` -- Signed 64-bit integer
    Long,
    /// `s` -- 32-bit floating point number
    Single,
    /// `d` -- 64-bit floating point number
    Double,
    Char,
    Void,
    Null,
    /// `string` or `{TY}*`
    Pointer(Box<Type>),
    /// `{name}<{}>`
    Struct(ImutStr),
    /// `{name}` -- Unknown type
    Unknown(ImutStr),
    /// `fn({})` or `<unknown function>`
    Function(Box<Option<Function>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Struct {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
        members: Vec<Argument>,
        ignore_empty: bool,
    },
    Function {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        variadic: bool,
        manual: bool,
        external: bool,
        builtin: bool,
        volatile: bool,
        format: bool,
        unaliased: Option<String>,
        generics: Vec<String>,
        arguments: Vec<Argument>,
        r#return: Option<Type>,
    },
    Constant {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        r#type: Option<Type>,
    },
}

impl Type {
    pub fn display(&self) -> ImutStr {
        match self {
            Self::Byte => "i8".into(),
            Self::UnsignedByte => "u8".into(),
            Self::Char => "char".into(),
            Self::Halfword => "i16".into(),
            Self::UnsignedHalfword => "u16".into(),
            Self::Boolean => "bool".into(),
            Self::Word => "i32".into(),
            Self::UnsignedWord => "u32".into(),
            Self::Long => "i64".into(),
            Self::UnsignedLong => "u64".into(),
            Self::Pointer(inner) => {
                if *inner.as_ref() == Type::Char {
                    "string".into()
                } else {
                    format!("{}*", inner.display()).into()
                }
            }
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            // TODO: generic structs
            Self::Struct(td, ..) => Arc::clone(td),
            Self::Function(inner) => {
                if let Some(inner) = *inner.to_owned() {
                    format!(
                        "fn({})",
                        inner
                            .parameters
                            .iter()
                            .map(|arg| arg.0.clone().display())
                            .collect::<Vec<ImutStr>>()
                            .join(", ")
                    )
                    .into()
                } else {
                    "<unknown function>".into()
                }
            }
            Self::Unknown(name) => Arc::clone(name),
        }
    }

    pub fn id(&self) -> ImutStr {
        match self {
            Self::Char => "char".into(),
            Self::Boolean => "bool".into(),
            Self::Byte => "i8".into(),
            Self::Halfword => "i16".into(),
            Self::Word => "i32".into(),
            Self::Long => "i64".into(),
            Self::UnsignedByte => "u8".into(),
            Self::UnsignedHalfword => "u16".into(),
            Self::UnsignedWord => "u32".into(),
            Self::UnsignedLong => "u64".into(),
            Self::Pointer(inner) => {
                if *inner.as_ref() == Type::Char {
                    "string".into()
                } else {
                    format!("{}*", (*inner).clone().id()).into()
                }
            }
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Struct(td, ..) => Arc::clone(td),
            Self::Function(_) => self.display(),
            _ => "".into(),
        }
    }

    pub fn strict_id(&self) -> ImutStr {
        match self {
            x if x.is_string() => "string".into(),
            x if x.is_void_pointer() => VOID_POINTER_ID.into(),
            Type::Pointer(_) => POINTER_ID.into(),
            _ => self.id(),
        }
    }

    pub fn to_internal_id(&self) -> ImutStr {
        let num: u8 = match self {
            Type::UnsignedByte => 4,
            Type::UnsignedHalfword => 5,
            Type::UnsignedWord => 6,
            Type::UnsignedLong => 7,
            Type::Byte => 8,
            Type::Halfword => 9,
            Type::Boolean => 10,
            Type::Word => 11,
            Type::Long => 12,
            Type::Single => 13,
            Type::Double => 14,
            Type::Char => 15,
            Type::Void => 16,
            Type::Null => 17,
            Type::Function(_) => 18,
            _ => 100, // Invalid
        };

        match self {
            Type::Pointer(inner) => format!("{GENERIC_POINTER}.{}", inner.to_internal_id()).into(),
            Type::Struct(name) => Arc::clone(name),
            Type::Unknown(name) => format!("{GENERIC_UNKNOWN}.{name}").into(),
            _ => num.to_string().into(),
        }
    }

    pub fn from_internal_id(id: ImutStr) -> (ImutStr, Vec<Type>) {
        fn is_num_id(id: String) -> Result<u8, ParseIntError> {
            if [
                GENERIC_IDENTIFIER,
                GENERIC_END,
                GENERIC_POINTER,
                GENERIC_UNKNOWN,
            ]
            .contains(&id.as_str())
            {
                "-1".parse::<u8>() // Throw an artificial error
            } else {
                id.parse::<u8>()
            }
        }

        fn id_to_ty(id: String) -> Type {
            match id.parse::<u8>() {
                Ok(inner) => match inner {
                    4 => Type::UnsignedByte,
                    5 => Type::UnsignedHalfword,
                    6 => Type::UnsignedWord,
                    7 => Type::UnsignedLong,
                    8 => Type::Byte,
                    9 => Type::Halfword,
                    10 => Type::Boolean,
                    11 => Type::Word,
                    12 => Type::Long,
                    13 => Type::Single,
                    14 => Type::Double,
                    15 => Type::Char,
                    16 => Type::Void,
                    17 => Type::Null,
                    18 => Type::Function(Box::new(None)),
                    _ => todo!("{}", id),
                },
                Err(_) => Type::Struct(id.into()),
            }
        }

        fn internal_match<T>(parts: &mut Peekable<T>) -> Option<Type>
        where
            T: Iterator<Item = String>,
        {
            parts.peek()?;

            let mut part = parts.next().unwrap();
            match is_num_id(part.clone()) {
                Ok(_) => Some(id_to_ty(part)),
                Err(_) => {
                    if part == GENERIC_POINTER {
                        let mut nesting = 0;

                        while part == GENERIC_POINTER {
                            parts.peek()?;

                            nesting += 1;

                            if parts.peek().is_some_and(|next| next != GENERIC_POINTER) {
                                break;
                            }

                            part = parts.next().unwrap();
                        }

                        let res = internal_match(parts);

                        if let Some(mut res) = res {
                            for _ in 0..nesting {
                                res = Type::Pointer(Box::new(res));
                            }

                            Some(res)
                        } else {
                            None
                        }
                    } else if part == GENERIC_UNKNOWN {
                        Some(Type::Unknown(parts.next().unwrap().into()))
                    } else if part == GENERIC_END {
                        internal_match(parts)
                    } else {
                        Some(Type::Struct(
                            if parts.peek().is_some_and(|part| part == GENERIC_IDENTIFIER) {
                                let mut res = vec![];
                                res.push(parts.next().unwrap());
                                let mut nesting = 0;

                                loop {
                                    if parts.peek().is_some_and(|part| part == GENERIC_IDENTIFIER) {
                                        nesting += 1;
                                    }

                                    if parts.peek().is_some_and(|part| part == GENERIC_END) {
                                        if nesting > 0 {
                                            nesting -= 1;
                                        } else {
                                            parts.next();
                                            break;
                                        }
                                    }

                                    res.push(parts.next().unwrap());
                                }

                                format!("{part}.{}.{GENERIC_END}", res.join(".")).into()
                            } else {
                                part.into()
                            },
                        ))
                    }
                }
            }
        }

        let mut parts = id
            .split('.')
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>();

        let name = parts.remove(0);
        assert_eq!(parts.remove(0), GENERIC_IDENTIFIER.to_string());

        let mut res = vec![];
        let mut iter = parts.iter().cloned().peekable();

        while iter.peek().is_some() {
            if let Some(x) = internal_match(&mut iter) {
                res.push(x);
            } else {
                break;
            }
        }

        (name.into(), res)
    }

    pub fn get_pointer_inner(&self) -> Option<Type> {
        match self {
            Self::Pointer(ty) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn get_struct_inner(&self) -> Option<ImutStr> {
        match self.clone() {
            Self::Struct(val, ..) => Some(val),
            _ => None,
        }
    }

    pub fn get_unknown_inner(&self) -> Option<ImutStr> {
        match self.clone() {
            Self::Unknown(val) => Some(val),
            _ => None,
        }
    }

    pub fn get_function_inner(&self) -> Option<Option<Function>> {
        match self.clone() {
            Self::Function(val) => Some(*val),
            _ => None,
        }
    }

    pub fn into_abi(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            other => other,
        }
    }

    pub fn into_base(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            Self::Struct(..) => Self::Long,
            other => other,
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Single | Self::Double)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn is_int(&self) -> bool {
        !self.is_float()
    }

    pub fn is_strictly_number(&self) -> bool {
        !self.is_string() && !self.is_void_pointer() && !self.is_struct() && !self.is_function()
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(..))
    }

    pub fn is_void_pointer(&self) -> bool {
        match self {
            Self::Pointer(inner) => *inner.as_ref() == Self::Void,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Pointer(inner) => *inner.as_ref() == Self::Char,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown(_))
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(inner) => inner.is_some(),
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    pub fn is_pointer_like(&self) -> bool {
        matches!(self, Self::Pointer(_) | Self::Long)
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Pointer(x) if matches!(**x, Self::Struct(_)) => false,
            Self::Pointer(x) if matches!(**x, Self::Char) => false,
            Self::Struct(_) => false,
            _ => true,
        }
    }

    pub fn is_map_to_int(&self) -> bool {
        matches!(
            self,
            Self::Byte
                | Self::UnsignedByte
                | Self::Halfword
                | Self::UnsignedHalfword
                | Self::UnsignedWord
                | Self::Boolean
                | Self::Char
                | Self::Void
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::UnsignedByte | Self::UnsignedHalfword | Self::UnsignedWord | Self::UnsignedLong
        )
    }

    pub fn weight(&self) -> u8 {
        match self {
            Self::Double => 4,
            Self::Single => 3,
            Self::Long | Self::UnsignedLong | Self::Pointer(..) | Self::Function(..) => 2,
            Self::Word => 1,
            other if other.is_map_to_int() => 1,
            _ => 0,
        }
    }

    pub fn size_base(&self) -> u64 {
        match self {
            Self::UnsignedByte | Self::Byte | Self::Char => 1,
            Self::UnsignedHalfword | Self::Halfword => 2,
            Self::Boolean | Self::UnsignedWord | Self::Word | Self::Single | Self::Void => 4,
            Self::Double => 8,
            // Returns 4 on 32-bit and 8 on 64-bit
            // Functions are just pointers to the start of them
            Self::UnsignedLong | Self::Long | Self::Pointer(..) | Self::Function(..) => {
                std::mem::size_of::<usize>() as u64
            }
            _ => 0,
        }
    }

    /// Returns number of bytes
    pub fn size(&self, module: &RefCell<Module>) -> u64 {
        match self {
            Self::Struct(val, ..) => {
                let size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == val.clone())
                    .unwrap_or_else(|| {
                        panic!("Unable to find aggregate type named '{}'.", self.display())
                    })
                    .size(module) as u64;

                size
            }
            Self::Unknown(..) | Self::Null => 0,
            _ => self.size_base(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte => write!(f, "b"),
            Self::UnsignedByte => write!(f, "ub"),
            Self::Char => write!(f, "b"),
            Self::Halfword => write!(f, "h"),
            Self::UnsignedHalfword => write!(f, "uh"),
            Self::Boolean => write!(f, "w"),
            Self::Word => write!(f, "w"),
            Self::UnsignedWord => write!(f, "uw"),
            Self::Long => write!(f, "l"),
            Self::UnsignedLong => write!(f, "ul"),
            Self::Pointer(..) => write!(f, "l"),
            Self::Single => write!(f, "s"),
            Self::Double => write!(f, "d"),
            Self::Void => write!(f, "w"),
            Self::Null => write!(f, ""),
            Self::Struct(td) => write!(f, ":{}", td),
            Self::Function(_) => write!(f, "l"),
            Self::Unknown(name) => panic!("Tried to compile with a generic type {name}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum DataItem {
    String(ImutStr),
    Const(f64),
}

impl Display for DataItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(str) => write!(f, "{}", str),
            Self::Const(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub linkage: Linkage,
    pub name: ImutStr,
    pub align: Option<u64>,
    pub items: Vec<(Type, DataItem)>,
}

impl Data {
    pub fn new(
        linkage: Linkage,
        name: ImutStr,
        align: Option<u64>,
        items: Vec<(Type, DataItem)>,
    ) -> Self {
        Self {
            linkage,
            name,
            align,
            items,
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}data ${} = ", self.linkage, self.name)?;

        if let Some(align) = self.align {
            write!(f, "align {}  ", align)?;
        }

        write!(
            f,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, item)| format!("{ty} {item}").into())
                .collect::<Vec<ImutStr>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub name: ImutStr,
    pub align: Option<u64>,
    pub items: Vec<(Type, usize)>,
    pub public: bool,
    pub usable: bool,
    pub imported: bool,
}

impl TypeDef {
    pub fn size(&self, module: &RefCell<Module>) -> usize {
        let mut size = 0;

        for (ty, _) in self.items.iter() {
            if ty.is_struct() {
                let tmp_size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == ty.get_struct_inner().unwrap())
                    .unwrap_or_else(|| {
                        panic!(
                            "Unable to find struct named '{}'",
                            ty.get_struct_inner().unwrap()
                        )
                    })
                    .size(module);

                size += tmp_size
            } else {
                size += ty.size(module) as usize;
            }
        }

        size
    }
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type :{} = ", self.name)?;
        if let Some(align) = self.align {
            write!(f, "align {} ", align)?;
        }

        write!(
            f,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, count)| if *count > 1 {
                    format!(
                        "{} {}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        },
                        count
                    )
                    .into()
                } else {
                    format!(
                        "{}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        }
                    )
                    .into()
                })
                .collect::<Vec<ImutStr>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<TypeDef>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            types: vec![],
            data: vec![],
        }
    }

    pub fn add_function(&mut self, function: Function) -> &mut Function {
        self.functions.push(function);
        self.functions.last_mut().unwrap()
    }

    pub fn add_type(&mut self, ty: TypeDef) -> &mut TypeDef {
        self.types.push(ty);
        self.types.last_mut().unwrap()
    }

    pub fn add_data(&mut self, data: Data) -> &mut Data {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }

    pub fn remove_unused_functions(&mut self, object_output: bool, passes: Option<usize>) {
        let mut passes = passes.unwrap_or(5);

        while passes > 0 {
            passes -= 1;

            let mut used_functions: HashSet<ImutStr> = HashSet::new();

            for func in self.functions.iter() {
                for block in func.blocks.iter() {
                    for statement in block.statements.iter() {
                        match statement {
                            Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                                for other in self.functions.iter() {
                                    if instr.is_global_used(other.name.clone()) {
                                        used_functions.insert(other.name.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }

            used_functions.insert("main".into());

            self.functions
                .retain(|func| used_functions.contains(&func.name) || object_output);
        }
    }

    // doesn't need multiple passes because will run after functions
    pub fn remove_unused_data(&mut self) {
        let mut used_data_sections: HashSet<String> = HashSet::new();

        for func in self.functions.iter() {
            for block in func.blocks.iter() {
                for statement in block.statements.iter() {
                    match statement {
                        Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                            for data in self.data.iter() {
                                if instr.is_global_used(data.name.clone()) {
                                    used_data_sections.insert(data.name.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }

        self.data
            .retain(|data| used_data_sections.contains(&data.name.to_string()));
    }

    pub fn remove_empty_structs(&mut self) {
        self.types.retain(|ty| !ty.items.is_empty())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ty in self.types.iter() {
            writeln!(f, "{ty}")?;
        }

        for data in self.data.iter() {
            writeln!(f, "{data}")?;
        }

        for func in self.functions.iter() {
            writeln!(f, "{}", func)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Cmp {
    /// Checks if first operand is less than second operand. If true returns 1, else
    /// returns 0.
    Lt,
    /// Checks if first operand is less than or equal to the second operand. If true
    /// returns 1, else returns 0.
    Lte,
    /// Checks if first operand is greater than second operand. If true returns 1, else
    /// returns 0.
    Gt,
    /// Checks if first operand is greater than or equal to the second operand. If true
    /// returns 1, else returns 0.
    Gte,
    /// Checks if the first operand is equal to the second operand. If true returns 1,
    /// else returns 0.
    Eq,
    /// Checks if the first operand is not equal to the second operand. If true returns
    /// 1, else returns 0.
    Neq,
}

impl Display for Cmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lt => write!(f, "lt"),
            Self::Lte => write!(f, "le"),
            Self::Gt => write!(f, "gt"),
            Self::Gte => write!(f, "ge"),
            Self::Eq => write!(f, "eq"),
            Self::Neq => write!(f, "ne"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Prefix {
    Double,
    Single,
    None,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Double => write!(f, "d_"),
            Self::Single => write!(f, "s_"),
            Self::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Value {
    Temp(ImutStr),
    Global(ImutStr),
    Const(Prefix, ImutStr),
    Literal(ImutStr),
}

impl Value {
    pub fn get_string_inner(&self) -> String {
        match &self {
            Self::Temp(val) => val.to_string(),
            Self::Global(val) => val.to_string(),
            Self::Literal(val) => val.to_string(),
            _ => panic!("Invalid value type {self}"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Temp(tmp) => write!(f, "%{tmp}"),
            Self::Global(glob) => write!(f, "${glob}"),
            Self::Const(prefix, value) => write!(f, "{prefix}{value}"),
            Self::Literal(value) => write!(f, "{value}"),
        }
    }
}

/// `T` = `wlsd`
/// `I` = `wl`
/// `F` = `sd`
/// `m` stands for the type of pointers on the target; on 64-bit architectures it is the same as `l`
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// `add` -- `T(T,T)`
    /// Adds two value's together
    Add(Value, Value),
    /// `sub` -- `T(T,T)`
    /// Subtracts two values
    Sub(Value, Value),
    /// `mul` -- `T(T,T)`
    /// Multiplies two values
    Mul(Value, Value),
    /// `div` -- `T(T,T)`
    /// Divides two values
    Div(Value, Value),
    /// `rem` -- `I(I,I)`
    /// Calculates the remainder of two values
    Rem(Value, Value),
    /// `udiv` -- `I(I,I)`
    /// Divides two unsigned values
    Udiv(Value, Value),
    /// `urem` -- `I(I,I)`
    /// Calculates the remainder of two unsigned values
    Urem(Value, Value),
    /// `and` -- `I(I,I)`
    /// Performs bitwise and operation on two values
    And(Value, Value),
    /// `or` -- `I(I,I)`
    /// Performs bitwise or operation on two values
    Or(Value, Value),
    /// `xor` -- `I(I,I)`
    /// Performs bitwise xor operation on two values
    Xor(Value, Value),
    /// `not` -- `I(I,I)`
    /// Performs bitwise not operation on two values
    ///
    /// NOTE: Not a QBE arithmetic instruction. Uses XOR to achieve same results
    /// as a bitwise NOT.
    Not(Value),
    /// `neg` -- `T(T)`
    /// Computes the negative of a value
    Neg(Value),
    /// `shl` -- `I(I,ww)`
    /// Performs a left-shift on the first operand by the amount from the second operand.
    ///
    /// NOTE: Shifting left always fills
    /// the freed bits with zeroes.
    Shl(Value, Value),
    /// `shr` -- `I(I,ww)`
    /// Performs a right-shift on the first operand by the amount from the second operand.
    ///
    /// NOTE: Shifting right can either preserve the sign of the value (using `sar`), or
    /// fill the newly freed bits with zeroes (using `shr`). Shifting left always fills
    /// the freed bits with zeroes.
    Shr(Value, Value),
    /// `sar` -- `I(I,ww)`
    /// Performs a right-shift on the first operand by the amount from the second operand.
    ///
    /// NOTE: Shifting right can either preserve the sign of the value (using `sar`), or
    /// fill the newly freed bits with zeroes (using `shr`). Shifting left always fills
    /// the freed bits with zeroes.
    Sar(Value, Value),
    /// `` -- ``
    Cmp(Type, Cmp, Value, Value),
    /// `cast` -- `wlsd(sdwl)`
    ///
    /// Returns the bits of the argument verbatim. `cast` will change an integer into
    /// a floating point of the same width and vice versa.
    ///
    /// Casts can be used to make bitwise operations on the representation of floating
    /// point numbers.  For example the following program will compute the opposite of
    /// the single-precision floating point number `%f` into `%rs`
    /// ```ssa
    /// %b0 =w cast %f
    /// %b1 =w xor 2147483648, %b0 # flip the msb (most significant bit)
    /// %rs =s cast %b1
    /// ```
    Cast(Value),
    /// `copy` -- `T(T)`
    ///
    /// Returns the bits of the argument verbatim. Will not change the type of the value
    /// like `cast` will.
    Copy(Value),
    /// 'ret' [VAL]
    ///
    /// *Function Return*
    /// Terminates the execution of the current function, optionally returning a value
    /// to the caller. The value returned must be of the type given in the function
    /// prototype. If the function prototype does not specify a return type,
    /// no return value can be used.
    Ret(Option<(Type, Value)>),
    /// 'jnz' VAL, @IDENT, @IDENT
    ///
    /// *Conditional Jump*
    /// When its word arugment is `non-zero`, it jumps to its first label argument;
    /// otherwise it jumps to the other label. The argument must be of word type;
    /// because of subtypign a long argument can be passed, but only it's least
    /// significant 32 bits will be compared to 0.
    Jnz(Value, ImutStr, ImutStr),
    /// `jmp` @IDENT
    ///
    /// *Unconditional Jump*
    /// Simply jumps to aother block of the same function.
    Jmp(ImutStr),
    /// 'hlt'
    ///
    /// *Program termination*
    /// Terminates the execution of the program with a target-dependent error.
    /// This instruction can be used when it is expected that the execution
    /// never reaches the end of the block it closes; for example, after having
    /// called a function such as `exit()`.
    Hlt,
    /// [IDENT '=' ABITY] 'call' VAL '(' (ARG), ')'
    /// ARG :=
    ///     ABITY VAL # Regular argument
    ///  | 'env' VAL # Environment argument (first)
    ///  | '...' # Variadic marker
    /// SUBWTY := 'sb' | 'ub' | 'sh' | 'uh' # Sub-word types
    /// ABITY := BASETY | SUBWTY | :IDENT
    ///
    /// *Environemnt Parameter*
    /// An environment parameter can be passed as first argument using the
    /// function does not expect an environment parameter, it will be safely
    /// discarded.
    ///
    /// *Variadic*
    /// When the called function is variadic, there must be a `...` marker
    /// seperating the named and variadic arguments.
    ///
    /// Note: Call instruction requires the type of all its arguments to be given.
    /// Return type can also either be a base type or an aggregate type.
    ///
    /// Note: When an aggregate type is used as argument type or return type, the
    /// value respectively passed or returned needs to be a pointer to a memory
    /// location holding the value.
    ///
    /// Note: Sub-word types are uesd for arguments and return values of width less
    /// than a word. Calls with a sub-word return-type define a temporary of base
    /// type `w` with its most significant bits unspecified.
    Call(Value, Vec<(Type, Value)>),
    /// `vastart` -- `(m)`
    ///
    /// The `vastart` instruction initializes a variable argument list used to access
    /// the extra parameters of the enclosing variadic function. It is safe to call
    /// it multiple times.
    VAStart(Value),
    /// `vaarg` -- `T(mmmm)`
    ///
    /// The vaarg instruction fetches the next argument from a variable argument list.
    /// It is currently limited to fetchign arguments that have a base type. This
    /// instruction is essentially effectful: calling it twice in a row will return
    /// two consecutive arguments from the argument list.
    ///
    /// NOTE: Both `vaarg` and `vastart` take a pointer to a variable argument list as
    /// sole argument. The size and alignment of variable argument lists depend
    /// on the target used. However it is pussible to conservatively use the maximum
    /// size and alignment required by all targets:
    /// ```ssa
    /// type :valist = align 8 { 24 } # For amd64_sysv
    /// type :valist = align 8 { 32 } # For arm64
    /// type :valist = align 8 { 8 } # For rv64
    /// ```
    VAArg(Value),
    /// `alloc4` -- `m(l)`
    /// Allocate a chunk of memory on the stack with `4` allignmen required for
    /// the allocated slot. QBE will make sure that the returned address is a
    /// multiple of that alignment value.
    ///
    /// NOTE: Pointers are stored in `long` temporaries
    Alloc4(Value),
    /// `alloc8` -- `m(l)`
    /// Allocate a chunk of memory on the stack with `8` bit allignmen required for
    /// the allocated slot. QBE will make sure that the returned address is a
    /// multiple of that alignment value.
    ///
    /// NOTE: Pointers are stored in `long` temporaries
    Alloc8(Value),
    /// `alloc16` -- `m(l)`
    /// Allocate a chunk of memory on the stack with `16` bit allignmen required for
    /// the allocated slot. QBE will make sure that the returned address is a
    /// multiple of that alignment value.
    ///
    /// NOTE: Pointers are stored in `long` temporaries
    Alloc16(Value),
    /// `store{TY}` -- `({TY},m)`
    /// Store instructions exist to store a value of any base type and any extended type.
    ///
    /// NOTE: Since halfwords and bytes are not first class in QBE, `storeh` and `storeb`
    /// take a word as argument. Only the first 16 or 8 bits of this word will be stored
    /// in memory at the address specified in the second argument.
    Store(Type, Value, Value),
    /// `load{TY}` -- `{TY}(m[m])`
    ///
    /// Load instructions exist to load a value from the stack.
    /// For types smaller than long, two variants of the load instruction are available:
    /// one will sign extend the loaded value, while the other will zero extend it.
    ///
    /// Note: All loads smaller than long can load to either a long or a word.
    /// Note: load(sw|uw|sh|uh|sb|ub)'s type signature is `load{TY}` -- `I(mm)`
    Load(Type, Value),
    /// Used to write a liteal to the source code. Not a legitimate QBE instruction.
    Literal(Value),
    /// `extsw`, `extuw` -- `l(w)`
    /// `extsh`, `extuh` -- `I(ww)`
    /// `extsb`, `extub` -- `I(ww)`
    /// `exts` -- `d(s)`
    ///
    /// Extends the precision of a temporary. Because QBE types do not specify
    /// the signedness (like in LLVM), extension instructions exist to sign-extend
    /// and zero-extend a value. For example, `extsb` takes a word argument and sign-
    /// extends the 8 least-significant bits to a full word or long, depending on the
    /// return type.
    ///
    ///Note: `exts` (extend single) are provided to change the precision of a floating
    ///point value.
    Ext(Type, Value),
    /// Conversion operators change the representation of a value, possibly modifying
    /// it if the target type cannot hold the value of the source type. Conversions
    /// can extend the precision of a temporary (e.g., from signed 8-bit to 32-bit),
    /// or convert a floating point into an integer and vice versa.
    ///
    /// Converting between signed integers and floating points is done using `stosi`
    /// (single to signed integer), `stoui` (single to unsigned integer), `dtosi` (double
    /// to signed integer), `dtoui` (double to unsigned integer), `swtof` (signed word
    /// to float), `uwtof` (unsigned word to float), `sltof` (signed long to float) and
    /// `ultof` (unsigned long to float).
    ///
    /// Note: because of subtyping, there is no need to have an isntruction
    /// to lower the precision of an integer temporary.
    Cnv(Type, Type, Value),
    /// `truncd` -- `s(d)`
    ///
    /// The instruction `truncd` (truncate double) are provided to change the precision
    /// of a floating point value. When the double argument of `truncd` cannot be
    /// represented as a single-precision point, it is truncated towards zero.
    Tnc(Value),
    /// blit -- `(m,m,w)`
    ///
    /// copies in-memory data from its first address argument to its second address
    /// argument. The third argument is the number of bytes to copy. The source and
    /// destination spans are required to be either non-overlapping, or fully
    /// overlapping (source address identical to the destination address). The byte
    /// count argument must be a nonnegative numeric constant; it cannot be a temporary.
    ///
    /// One blit instruction may generate a number of instructions proportional to its
    /// byte count argument, consequently, it is recommended to keep this argument
    /// relatively small. If large copies are necessary, it is preferrable that
    /// frontends generate calls to a supporting `memcpy` function.
    Blit(Value, Value, u32),

    #[cfg(debug_assertions)]
    Cmt(ImutStr),
}

impl Instruction {
    #[allow(dead_code)]
    fn is_global_used(&self, global_name: ImutStr) -> bool {
        match self {
            Self::Add(v1, v2)
            | Self::Sub(v1, v2)
            | Self::Mul(v1, v2)
            | Self::Div(v1, v2)
            | Self::Rem(v1, v2)
            | Self::Urem(v1, v2)
            | Self::Udiv(v1, v2)
            | Self::And(v1, v2)
            | Self::Or(v1, v2)
            | Self::Xor(v1, v2)
            | Self::Cmp(_, _, v1, v2)
            | Self::Store(_, v1, v2)
            | Self::Blit(v1, v2, _)
            | Self::Shl(v1, v2)
            | Self::Shr(v1, v2)
            | Self::Sar(v1, v2) => {
                matches!(v1, Value::Global(name) if *name == global_name)
                    || matches!(v2, Value::Global(name) if *name == global_name)
            }
            Self::Load(_, v)
            | Self::Cnv(_, _, v)
            | Self::Ext(_, v)
            | Self::Tnc(v)
            | Self::Cast(v)
            | Self::VAArg(v)
            | Self::VAStart(v)
            | Self::Literal(v)
            | Self::Copy(v)
            | Self::Jnz(v, _, _)
            | Self::Alloc4(v)
            | Self::Alloc8(v)
            | Self::Alloc16(v)
            | Self::Neg(v)
            | Self::Not(v) => matches!(v, Value::Global(name) if *name == global_name),
            Self::Ret(val) => match val {
                Some((_, v)) => matches!(v, Value::Global(name) if *name == global_name),
                None => false,
            },
            Self::Call(v, args) => {
                let found = matches!(v, Value::Global(name) if *name == global_name);

                if found {
                    found
                } else {
                    for arg in args.iter().cloned() {
                        if matches!(arg.1, Value::Global(name) if name == global_name) {
                            return true;
                        }
                    }

                    false
                }
            }
            #[cfg(debug_assertions)]
            Self::Cmt(_) => false,
            Self::Jmp(_) => false,
            Self::Hlt => false,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(f, "add {}, {}", lhs, rhs),
            Self::Sub(lhs, rhs) => write!(f, "sub {}, {}", lhs, rhs),
            Self::Div(lhs, rhs) => write!(f, "div {}, {}", lhs, rhs),
            Self::Mul(lhs, rhs) => write!(f, "mul {}, {}", lhs, rhs),
            Self::Rem(lhs, rhs) => write!(f, "rem {}, {}", lhs, rhs),
            Self::And(lhs, rhs) => write!(f, "and {}, {}", lhs, rhs),
            Self::Or(lhs, rhs) => write!(f, "or {}, {}", lhs, rhs),
            Self::Xor(lhs, rhs) => write!(f, "xor {}, {}", lhs, rhs),
            Self::Not(val) => write!(f, "xor {}, -1", val),
            Self::Neg(val) => write!(f, "neg {}", val),
            Self::Copy(val) => write!(f, "copy {}", val),
            Self::VAArg(val) => write!(f, "vaarg {}", val),
            Self::VAStart(val) => write!(f, "vastart {}", val),
            Self::Cmp(ty, cmp, lhs, rhs) => {
                assert!(
                    !matches!(ty, Type::Struct(..)),
                    "Cannot compare struct types"
                );

                write!(
                    f,
                    "c{}{} {}, {}",
                    if ty.is_float() {
                        match cmp {
                            Cmp::Lt => "lt",
                            Cmp::Lte => "le",
                            Cmp::Gt => "gt",
                            Cmp::Gte => "ge",
                            Cmp::Eq => "eq",
                            Cmp::Neq => "ne",
                        }
                    } else if ty.is_unsigned() {
                        match cmp {
                            Cmp::Lt => "ult",
                            Cmp::Lte => "ule",
                            Cmp::Gt => "ugt",
                            Cmp::Gte => "uge",
                            Cmp::Eq => "eq",
                            Cmp::Neq => "ne",
                        }
                    } else {
                        match cmp {
                            Cmp::Lt => "slt",
                            Cmp::Lte => "sle",
                            Cmp::Gt => "sgt",
                            Cmp::Gte => "sge",
                            Cmp::Eq => "eq",
                            Cmp::Neq => "ne",
                        }
                    },
                    ty.clone().into_abi(),
                    lhs,
                    rhs
                )
            }
            Self::Udiv(lhs, rhs) => write!(f, "udiv {}, {}", lhs, rhs),
            Self::Urem(lhs, rhs) => write!(f, "urem {}, {}", lhs, rhs),
            Self::Shl(lhs, rhs) => write!(f, "shl {}, {}", lhs, rhs),
            Self::Shr(lhs, rhs) => write!(f, "shr {}, {}", lhs, rhs),
            Self::Sar(lhs, rhs) => write!(f, "sar {}, {}", lhs, rhs),
            Self::Cast(val) => write!(f, "cast {}", val),
            Self::Ret(val) => match val {
                Some((_, val)) => write!(f, "ret {}", val),
                None => write!(f, "ret"),
            },
            Self::Jnz(val, if_nonzero, if_zero) => {
                write!(f, "jnz {}, @{}, @{}", val, if_nonzero, if_zero)
            }
            Self::Jmp(label) => write!(f, "jmp @{label}"),
            Self::Call(name, args) => write!(
                f,
                "call {name}({})",
                args.iter()
                    .map(|(ty, temp)| match ty {
                        Type::Null => format!("{}", temp).into(),
                        _ => format!("{} {}", ty.clone().into_abi(), temp).into(),
                    })
                    .collect::<Vec<ImutStr>>()
                    .join(", ")
            ),
            Self::Alloc4(val) => write!(f, "alloc4 {}", val),
            Self::Alloc8(val) => write!(f, "alloc8 {}", val),
            Self::Alloc16(val) => write!(f, "alloc16 {}", val),
            Self::Store(ty, dest, value) => {
                write!(
                    f,
                    "store{} {}, {}",
                    if ty.clone() != Type::Char {
                        ty.clone().into_base()
                    } else {
                        ty.clone()
                    },
                    value,
                    dest
                )
            }
            Self::Load(ty, src) => {
                write!(
                    f,
                    "load{} {}",
                    if !ty.is_unsigned() && ty.is_map_to_int() {
                        format!("s{}", ty.clone())
                    } else {
                        if ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        }
                        .to_string()
                    },
                    src
                )
            }
            Self::Literal(val) => write!(f, "{val}"),
            Self::Ext(ty, val) => write!(
                f,
                "ext{} {}",
                if ty.is_float() || ty.is_unsigned() {
                    ty.to_string()
                } else {
                    format!("s{}", ty)
                },
                val
            ),
            Self::Tnc(val) => write!(f, "truncd {}", val),
            Self::Cmt(val) => write!(f, "# {}", val),
            Self::Hlt => write!(f, "hlt"),
            Self::Blit(src, dest, n) => write!(f, "blit {}, {}, {}", src, dest, n),
            Self::Cnv(first, second, val) => write!(
                f,
                "{}to{} {}",
                if first.is_float() {
                    first.to_string()
                } else {
                    format!("s{}", first.clone().into_abi())
                },
                if second.is_float() { "f" } else { "si" },
                val
            ),
        }
    }
}
