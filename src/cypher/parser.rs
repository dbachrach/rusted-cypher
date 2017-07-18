// Adapted from rust-serialize
use std::collections::{BTreeMap};
use std::i64;
use std::string;
use std::{char, f64, io, str};

use self::JsonEvent::*;
use self::ErrorCode::*;
use self::ParserError::*;
use self::ParserState::*;
use self::InternalStackElement::*;

use itertools::Itertools;


/// Represents a json value
#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Json {
    I64(i64),
    U64(u64),
    F64(f64),
    String(string::String),
    Boolean(bool),
    Array(self::Array),
    Object(self::Object),
    Null,
}

pub type Array = Vec<Json>;
pub type Object = BTreeMap<string::String, Json>;

/// The errors that can arise while parsing a JSON stream.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorCode {
    InvalidSyntax,
    InvalidNumber,
    EOFWhileParsingObject,
    EOFWhileParsingArray,
    EOFWhileParsingValue,
    EOFWhileParsingString,
    KeyMustBeAString,
    ExpectedColon,
    TrailingCharacters,
    TrailingComma,
    InvalidEscape,
    InvalidUnicodeCodePoint,
    LoneLeadingSurrogateInHexEscape,
    UnexpectedEndOfHexEscape,
    UnrecognizedHex,
    NotFourDigit,
    ControlCharacterInString,
    NotUtf8,
}

#[derive(Debug)]
pub enum ParserError {
    /// msg, line, col
    SyntaxError(ErrorCode, usize, usize),
    IoError(io::Error),
}

impl PartialEq for ParserError {
    fn eq(&self, other: &ParserError) -> bool {
        match (self, other) {
            (&SyntaxError(msg0, line0, col0), &SyntaxError(msg1, line1, col1)) =>
                msg0 == msg1 && line0 == line1 && col0 == col1,
            (&IoError(_), _) => false,
            (_, &IoError(_)) => false,
        }
    }
}

/// The output of the streaming parser.
#[derive(PartialEq, Debug)]
pub enum JsonEvent {
    ObjectStart,
    ObjectEnd,
    ArrayStart,
    ArrayEnd,
    BooleanValue(bool),
    I64Value(i64),
    U64Value(u64),
    F64Value(f64),
    StringValue(string::String),
    NullValue,
    Error(ParserError),
}

#[derive(PartialEq, Debug)]
enum ParserState {
    // Parse a value in an array, true means first element.
    ParseArray(bool),
    // Parse ',' or ']' after an element in an array.
    ParseArrayComma,
    // Parse a key:value in an object, true means first element.
    ParseObject(bool),
    // Parse ',' or ']' after an element in an object.
    ParseObjectComma,
    // Initial state.
    ParseStart,
    // Expecting the stream to end.
    ParseBeforeFinish,
    // Parsing can't continue.
    ParseFinished,
}


/// A Stack represents the current position of the parser in the logical
/// structure of the JSON stream.
/// For example foo.bar[3].x
pub struct Stack {
    stack: Vec<InternalStackElement>,
    pub str_buffer: Vec<u8>,
}

/// StackElements compose a Stack.
/// For example, Key("foo"), Key("bar"), Index(3) and Key("x") are the
/// StackElements compositing the stack that represents foo.bar[3].x
#[derive(PartialEq, Clone, Debug)]
pub enum StackElement<'l> {
    Index(u32),
    Key(&'l str),
}

// Internally, Key elements are stored as indices in a buffer to avoid
// allocating a string for every member of an object.
#[derive(PartialEq, Clone, Debug)]
enum InternalStackElement {
    InternalIndex(u32),
    InternalKey(u16, u16), // start, size
}

impl Stack {
    pub fn new() -> Stack {
        Stack { stack: Vec::new(), str_buffer: Vec::new() }
    }

    /// Returns The number of elements in the Stack.
    pub fn len(&self) -> usize { self.stack.len() }

    /// Returns true if the stack is empty.
    pub fn is_empty(&self) -> bool { self.stack.is_empty() }

    /// Provides access to the StackElement at a given index.
    /// lower indices are at the bottom of the stack while higher indices are
    /// at the top.
    pub fn get<'l>(&'l self, idx: usize) -> StackElement<'l> {
        match self.stack[idx] {
            InternalIndex(i) => StackElement::Index(i),
            InternalKey(start, size) => {
                StackElement::Key(str::from_utf8(
                    &self.str_buffer[start as usize .. start as usize + size as usize]).unwrap())
            }
        }
    }

    pub fn description(&self) -> String {
        self.stack
            .iter()
            .map(|el| {
                match el {
                    &InternalIndex(i) => format!("[{}]", i),
                    &InternalKey(start, size) => {
                        str::from_utf8(
                            &self.str_buffer[start as usize .. start as usize + size as usize]
                        ).unwrap().to_string()
                    }
                }
            })
            .join(".")
    }

    /// Compares this stack with an array of StackElements.
    pub fn is_equal_to(&self, rhs: &[StackElement]) -> bool {
        if self.stack.len() != rhs.len() { return false; }
        for i in 0..rhs.len() {
            if self.get(i) != rhs[i] { return false; }
        }
        return true;
    }

    /// Returns true if the bottom-most elements of this stack are the same as
    /// the ones passed as parameter.
    pub fn starts_with(&self, rhs: &[StackElement]) -> bool {
        if self.stack.len() < rhs.len() { return false; }
        for i in 0..rhs.len() {
            if self.get(i) != rhs[i] { return false; }
        }
        return true;
    }

    /// Returns true if the top-most elements of this stack are the same as
    /// the ones passed as parameter.
    pub fn ends_with(&self, rhs: &[StackElement]) -> bool {
        if self.stack.len() < rhs.len() { return false; }
        let offset = self.stack.len() - rhs.len();
        for i in 0..rhs.len() {
            if self.get(i + offset) != rhs[i] { return false; }
        }
        return true;
    }

    /// Returns the top-most element (if any).
    pub fn top<'l>(&'l self) -> Option<StackElement<'l>> {
        return match self.stack.last() {
            None => None,
            Some(&InternalIndex(i)) => Some(StackElement::Index(i)),
            Some(&InternalKey(start, size)) => {
                Some(StackElement::Key(str::from_utf8(
                    &self.str_buffer[start as usize .. (start+size) as usize]
                ).unwrap()))
            }
        }
    }

    // Used by Parser to insert Key elements at the top of the stack.
    fn push_key(&mut self, key: string::String) {
        self.stack.push(InternalKey(self.str_buffer.len() as u16, key.len() as u16));
        for c in key.as_bytes().iter() {
            self.str_buffer.push(*c);
        }
    }

    // Used by Parser to insert Index elements at the top of the stack.
    fn push_index(&mut self, index: u32) {
        self.stack.push(InternalIndex(index));
    }

    // Used by Parser to remove the top-most element of the stack.
    fn pop(&mut self) {
        assert!(!self.is_empty());
        match *self.stack.last().unwrap() {
            InternalKey(_, sz) => {
                let new_size = self.str_buffer.len() - sz as usize;
                self.str_buffer.truncate(new_size);
            }
            InternalIndex(_) => {}
        }
        self.stack.pop();
    }

    // Used by Parser to test whether the top-most element is an index.
    fn last_is_index(&self) -> bool {
        if self.is_empty() { return false; }
        return match *self.stack.last().unwrap() {
            InternalIndex(_) => true,
            _ => false,
        }
    }

    // Used by Parser to increment the index of the top-most element.
    fn bump_index(&mut self) {
        let len = self.stack.len();
        let idx = match *self.stack.last().unwrap() {
            InternalIndex(i) => { i + 1 }
            _ => { panic!(); }
        };
        self.stack[len - 1] = InternalIndex(idx);
    }
}

/// A streaming JSON parser implemented as an iterator of JsonEvent, consuming
/// an iterator of char.
pub struct Parser<T> {
    rdr: T,
    ch: Option<char>,
    line: usize,
    col: usize,
    // We maintain a stack representing where we are in the logical structure
    // of the JSON stream.
    stack: Stack,
    // A state machine is kept to make it possible to interrupt and resume parsing.
    state: ParserState,
    stored_buffer: String,
    storing_to_buffer: bool,
}

impl<T: Iterator<Item = char>> Iterator for Parser<T> {
    type Item = JsonEvent;

    fn next(&mut self) -> Option<JsonEvent> {
        if self.state == ParseFinished {
            return None;
        }

        if self.state == ParseBeforeFinish {
            self.parse_whitespace();
            // Make sure there is no trailing characters.
            if self.eof() {
                self.state = ParseFinished;
                return None;
            } else {
                return Some(self.error_event(TrailingCharacters));
            }
        }

        return Some(self.parse());
    }
}

impl<T: Iterator<Item = char>> Parser<T> {
    /// Creates the JSON parser.
    pub fn new(rdr: T) -> Parser<T> {
        let mut p = Parser {
            rdr: rdr,
            ch: Some('\x00'),
            line: 1,
            col: 0,
            stack: Stack::new(),
            state: ParseStart,
            stored_buffer: String::new(),
            storing_to_buffer: false,
        };
        p.bump();
        return p;
    }

    pub fn start_storing_buffer(&mut self, c: char) {
        self.storing_to_buffer = true;
        self.stored_buffer.push(c);
        if let Some(c) = self.ch {
            self.stored_buffer.push(c);
        }
    }

    pub fn get_stored_buffer(&mut self) -> String {
        self.stored_buffer.pop();
        self.storing_to_buffer = false;
        let buf = self.stored_buffer.clone();
        self.stored_buffer = String::new();
        buf
    }

    /// Provides access to the current position in the logical structure of the
    /// JSON stream.
    pub fn stack<'l>(&'l self) -> &'l Stack {
        return &self.stack;
    }

    fn eof(&self) -> bool { self.ch.is_none() }
    fn ch_or_null(&self) -> char { self.ch.unwrap_or('\x00') }
    fn bump(&mut self) {
        self.ch = self.rdr.next();

        if self.storing_to_buffer {
            if let Some(c) = self.ch {
                self.stored_buffer.push(c);
            }
        }

        if self.ch_is('\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.bump();
        self.ch
    }
    fn ch_is(&self, c: char) -> bool {
        self.ch == Some(c)
    }

    fn error<E>(&self, reason: ErrorCode) -> Result<E, ParserError> {
        Err(SyntaxError(reason, self.line, self.col))
    }

    fn parse_whitespace(&mut self) {
        while self.ch_is(' ') ||
              self.ch_is('\n') ||
              self.ch_is('\t') ||
              self.ch_is('\r') { self.bump(); }
    }

    fn parse_number(&mut self) -> JsonEvent {
        let mut neg = false;

        if self.ch_is('-') {
            self.bump();
            neg = true;
        }

        let res = match self.parse_u64() {
            Ok(res) => res,
            Err(e) => { return Error(e); }
        };

        if self.ch_is('.') || self.ch_is('e') || self.ch_is('E') {
            let mut res = res as f64;

            if self.ch_is('.') {
                res = match self.parse_decimal(res) {
                    Ok(res) => res,
                    Err(e) => { return Error(e); }
                };
            }

            if self.ch_is('e') || self.ch_is('E') {
                res = match self.parse_exponent(res) {
                    Ok(res) => res,
                    Err(e) => { return Error(e); }
                };
            }

            if neg {
                res *= -1.0;
            }

            F64Value(res)
        } else {
            if neg {
                // Make sure we don't underflow.
                if res > (i64::MAX as u64) + 1 {
                    Error(SyntaxError(InvalidNumber, self.line, self.col))
                } else if res == 0 {
                    I64Value(res as i64)
                } else {
                    I64Value((!res + 1) as i64)
                }
            } else {
                U64Value(res)
            }
        }
    }

    fn parse_u64(&mut self) -> Result<u64, ParserError> {
        let mut accum: u64 = 0;

        match self.ch_or_null() {
            '0' => {
                self.bump();

                // A leading '0' must be the only digit before the decimal point.
                match self.ch_or_null() {
                    '0' ... '9' => return self.error(InvalidNumber),
                    _ => ()
                }
            },
            '1' ... '9' => {
                while !self.eof() {
                    match self.ch_or_null() {
                        c @ '0' ... '9' => {
                            macro_rules! try_or_invalid {
                                ($e: expr) => {
                                    match $e {
                                        Some(v) => v,
                                        None => return self.error(InvalidNumber)
                                    }
                                }
                            }
                            accum = try_or_invalid!(accum.checked_mul(10));
                            accum = try_or_invalid!(accum.checked_add((c as u64) - ('0' as u64)));

                            self.bump();
                        }
                        _ => break,
                    }
                }
            }
            _ => return self.error(InvalidNumber),
        }

        Ok(accum)
    }

    fn parse_decimal(&mut self, mut res: f64) -> Result<f64, ParserError> {
        self.bump();

        // Make sure a digit follows the decimal place.
        match self.ch_or_null() {
            '0' ... '9' => (),
             _ => return self.error(InvalidNumber)
        }

        let mut dec = 1.0;
        let mut frac = 0.0;
        while !self.eof() {
            match self.ch_or_null() {
                c @ '0' ... '9' => {
                    dec /= 10.0;
                    frac += (((c as isize) - ('0' as isize)) as f64) * dec;
                    self.bump();
                }
                _ => break,
            }
        }

        res += frac;

        Ok(res)
    }

    fn parse_exponent(&mut self, mut res: f64) -> Result<f64, ParserError> {
        self.bump();

        let mut exp = 0;
        let mut neg_exp = false;

        if self.ch_is('+') {
            self.bump();
        } else if self.ch_is('-') {
            self.bump();
            neg_exp = true;
        }

        // Make sure a digit follows the exponent place.
        match self.ch_or_null() {
            '0' ... '9' => (),
            _ => return self.error(InvalidNumber)
        }
        while !self.eof() {
            match self.ch_or_null() {
                c @ '0' ... '9' => {
                    exp *= 10;
                    exp += (c as usize) - ('0' as usize);

                    self.bump();
                }
                _ => break
            }
        }

        let exp = 10_f64.powi(exp as i32);
        if neg_exp {
            res /= exp;
        } else {
            res *= exp;
        }

        Ok(res)
    }

    fn decode_hex_escape(&mut self) -> Result<u16, ParserError> {
        let mut i = 0;
        let mut n = 0;
        while i < 4 {
            self.bump();
            n = match self.ch_or_null() {
                c @ '0' ... '9' => n * 16 + ((c as u16) - ('0' as u16)),
                c @ 'a' ... 'f' => n * 16 + (10 + (c as u16) - ('a' as u16)),
                c @ 'A' ... 'F' => n * 16 + (10 + (c as u16) - ('A' as u16)),
                _ => return self.error(InvalidEscape)
            };

            i += 1;
        }

        Ok(n)
    }

    fn parse_str(&mut self) -> Result<string::String, ParserError> {
        let mut escape = false;
        let mut res = string::String::new();

        loop {
            self.bump();
            if self.eof() {
                return self.error(EOFWhileParsingString);
            }

            if escape {
                match self.ch_or_null() {
                    '"' => res.push('"'),
                    '\\' => res.push('\\'),
                    '/' => res.push('/'),
                    'b' => res.push('\x08'),
                    'f' => res.push('\x0c'),
                    'n' => res.push('\n'),
                    'r' => res.push('\r'),
                    't' => res.push('\t'),
                    'u' => match try!(self.decode_hex_escape()) {
                        0xDC00 ... 0xDFFF => {
                            return self.error(LoneLeadingSurrogateInHexEscape)
                        }

                        // Non-BMP characters are encoded as a sequence of
                        // two hex escapes, representing UTF-16 surrogates.
                        n1 @ 0xD800 ... 0xDBFF => {
                            match (self.next_char(), self.next_char()) {
                                (Some('\\'), Some('u')) => (),
                                _ => return self.error(UnexpectedEndOfHexEscape),
                            }

                            let n2 = try!(self.decode_hex_escape());
                            if n2 < 0xDC00 || n2 > 0xDFFF {
                                return self.error(LoneLeadingSurrogateInHexEscape)
                            }
                            let c = (((n1 - 0xD800) as u32) << 10 |
                                     (n2 - 0xDC00) as u32) + 0x1_0000;
                            res.push(char::from_u32(c).unwrap());
                        }

                        n => match char::from_u32(n as u32) {
                            Some(c) => res.push(c),
                            None => return self.error(InvalidUnicodeCodePoint),
                        },
                    },
                    _ => return self.error(InvalidEscape),
                }
                escape = false;
            } else if self.ch_is('\\') {
                escape = true;
            } else {
                match self.ch {
                    Some('"') => {
                        self.bump();
                        return Ok(res);
                    },
                    Some(c) if c <= '\u{1F}' =>
                        return self.error(ControlCharacterInString),
                    Some(c) => res.push(c),
                    None => unreachable!()
                }
            }
        }
    }

    // Invoked at each iteration, consumes the stream until it has enough
    // information to return a JsonEvent.
    // Manages an internal state so that parsing can be interrupted and resumed.
    // Also keeps track of the position in the logical structure of the json
    // stream int the form of a stack that can be queried by the user using the
    // stack() method.
    fn parse(&mut self) -> JsonEvent {
        loop {
            // The only paths where the loop can spin a new iteration
            // are in the cases ParseArrayComma and ParseObjectComma if ','
            // is parsed. In these cases the state is set to (respectively)
            // ParseArray(false) and ParseObject(false), which always return,
            // so there is no risk of getting stuck in an infinite loop.
            // All other paths return before the end of the loop's iteration.
            self.parse_whitespace();

            match self.state {
                ParseStart => {
                    return self.parse_start();
                }
                ParseArray(first) => {
                    return self.parse_array(first);
                }
                ParseArrayComma => {
                    match self.parse_array_comma_or_end() {
                        Some(evt) => { return evt; }
                        None => {}
                    }
                }
                ParseObject(first) => {
                    return self.parse_object(first);
                }
                ParseObjectComma => {
                    self.stack.pop();
                    if self.ch_is(',') {
                        self.state = ParseObject(false);
                        self.bump();
                    } else {
                        return self.parse_object_end();
                    }
                }
                _ => {
                    return self.error_event(InvalidSyntax);
                }
            }
        }
    }

    fn parse_start(&mut self) -> JsonEvent {
        let val = self.parse_value();
        self.state = match val {
            Error(_) => ParseFinished,
            ArrayStart => ParseArray(true),
            ObjectStart => ParseObject(true),
            _ => ParseBeforeFinish,
        };
        return val;
    }

    fn parse_array(&mut self, first: bool) -> JsonEvent {
        if self.ch_is(']') {
            if !first {
                self.error_event(InvalidSyntax)
            } else {
                self.state = if self.stack.is_empty() {
                    ParseBeforeFinish
                } else if self.stack.last_is_index() {
                    ParseArrayComma
                } else {
                    ParseObjectComma
                };
                self.bump();
                ArrayEnd
            }
        } else {
            if first {
                self.stack.push_index(0);
            }
            let val = self.parse_value();
            self.state = match val {
                Error(_) => ParseFinished,
                ArrayStart => ParseArray(true),
                ObjectStart => ParseObject(true),
                _ => ParseArrayComma,
            };
            val
        }
    }

    fn parse_array_comma_or_end(&mut self) -> Option<JsonEvent> {
        if self.ch_is(',') {
            self.stack.bump_index();
            self.state = ParseArray(false);
            self.bump();
            None
        } else if self.ch_is(']') {
            self.stack.pop();
            self.state = if self.stack.is_empty() {
                ParseBeforeFinish
            } else if self.stack.last_is_index() {
                ParseArrayComma
            } else {
                ParseObjectComma
            };
            self.bump();
            Some(ArrayEnd)
        } else if self.eof() {
            Some(self.error_event(EOFWhileParsingArray))
        } else {
            Some(self.error_event(InvalidSyntax))
        }
    }

    fn parse_object(&mut self, first: bool) -> JsonEvent {
        if self.ch_is('}') {
            if !first {
                if self.stack.is_empty() {
                    return self.error_event(TrailingComma);
                } else {
                    self.stack.pop();
                }
            }
            self.state = if self.stack.is_empty() {
                ParseBeforeFinish
            } else if self.stack.last_is_index() {
                ParseArrayComma
            } else {
                ParseObjectComma
            };
            self.bump();
            return ObjectEnd;
        }
        if self.eof() {
            return self.error_event(EOFWhileParsingObject);
        }
        if !self.ch_is('"') {
            return self.error_event(KeyMustBeAString);
        }
        let s = match self.parse_str() {
            Ok(s) => s,
            Err(e) => {
                self.state = ParseFinished;
                return Error(e);
            }
        };
        self.parse_whitespace();
        if self.eof() {
            return self.error_event(EOFWhileParsingObject);
        } else if self.ch_or_null() != ':' {
            return self.error_event(ExpectedColon);
        }
        self.stack.push_key(s);
        self.bump();
        self.parse_whitespace();

        let val = self.parse_value();

        self.state = match val {
            Error(_) => ParseFinished,
            ArrayStart => ParseArray(true),
            ObjectStart => ParseObject(true),
            _ => ParseObjectComma,
        };
        return val;
    }

    fn parse_object_end(&mut self) -> JsonEvent {
        if self.ch_is('}') {
            self.state = if self.stack.is_empty() {
                ParseBeforeFinish
            } else if self.stack.last_is_index() {
                ParseArrayComma
            } else {
                ParseObjectComma
            };
            self.bump();
            ObjectEnd
        } else if self.eof() {
            self.error_event(EOFWhileParsingObject)
        } else {
            self.error_event(InvalidSyntax)
        }
    }

    fn parse_value(&mut self) -> JsonEvent {
        if self.eof() { return self.error_event(EOFWhileParsingValue); }
        match self.ch_or_null() {
            'n' => { self.parse_ident("ull", NullValue) }
            't' => { self.parse_ident("rue", BooleanValue(true)) }
            'f' => { self.parse_ident("alse", BooleanValue(false)) }
            '0' ... '9' | '-' => self.parse_number(),
            '"' => match self.parse_str() {
                Ok(s) => StringValue(s),
                Err(e) => Error(e),
            },
            '[' => {
                self.bump();
                ArrayStart
            }
            '{' => {
                self.bump();
                ObjectStart
            }
            _ => { self.error_event(InvalidSyntax) }
        }
    }

    fn parse_ident(&mut self, ident: &str, value: JsonEvent) -> JsonEvent {
        if ident.chars().all(|c| Some(c) == self.next_char()) {
            self.bump();
            value
        } else {
            Error(SyntaxError(InvalidSyntax, self.line, self.col))
        }
    }

    fn error_event(&mut self, reason: ErrorCode) -> JsonEvent {
        self.state = ParseFinished;
        Error(SyntaxError(reason, self.line, self.col))
    }
}
