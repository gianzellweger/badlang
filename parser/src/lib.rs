#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(map_try_insert)]
#![forbid(unsafe_code)]
#![deny(clippy::pedantic)]
#![deny(clippy::nursery)]
#![forbid(clippy::enum_glob_use)]
#![forbid(clippy::unwrap_used)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]

use std::{
    collections::HashMap,
    path::PathBuf,
    time::{Duration, Instant},
};

use colored::Colorize;
#[cfg(feature = "silly")]
use geocoding::Reverse;
use itertools::Itertools;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

pub fn report_error(string: &str) -> ! {
    panic!("{}: {string}", "ERROR".bold().red());
}

pub fn report_warning(string: &str) {
    eprintln!("{}: {string}", "WARNING".bold().yellow());
}
#[derive(Clone, Debug, PartialEq)]
pub enum StackValue {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

impl std::fmt::Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{int}"),
            Self::Float(float) => write!(f, "{float:.2}"),
            Self::String(string) => write!(f, "{string}"),
            Self::Bool(boolean) => write!(f, "{boolean}"),
        }
    }
}

impl std::ops::Add for StackValue {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            Self::String(string) => Self::String(string + other.to_string().as_str()),
            Self::Integer(int) => match other {
                Self::Integer(int2) => Self::Integer(int + int2),
                Self::Float(float) => Self::Float(int as f64 + float),
                Self::String(string) => Self::String(int.to_string() + string.as_str()),
                Self::Bool(boolean) => Self::String(int.to_string() + boolean.to_string().as_str()),
            },
            Self::Float(float) => match other {
                Self::Integer(int) => Self::Float(float + int as f64),
                Self::Float(float2) => Self::Float(float + float2),
                Self::String(string) => Self::String(self.to_string() + string.as_str()),
                Self::Bool(boolean) => Self::String(self.to_string() + boolean.to_string().as_str()),
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => Self::Integer(i64::from(boolean) + i64::from(boolean2)),
                Self::String(string) => Self::String(boolean.to_string() + string.as_str()),
                Self::Float(_) => Self::String(boolean.to_string() + other.to_string().as_str()),
                Self::Integer(int) => Self::String(boolean.to_string() + int.to_string().as_str()),
            },
        }
    }
}

impl std::ops::Sub for StackValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match self {
            Self::String(string) => match other {
                Self::Integer(int) => {
                    if int < 0 {
                        return Self::String(string + " ".repeat((-int) as usize).as_str());
                    }
                    if int as usize > string.len() {
                        report_error("Tried to subtract int from string where the int is bigger than the strings length");
                    }
                    Self::String(string[..string.len() - int as usize].to_string())
                }
                Self::Float(float) => {
                    let int = float.round() as i64;
                    if int < 0 {
                        return Self::String(string + " ".repeat((-int) as usize).as_str());
                    }
                    if int as usize > string.len() {
                        report_error("Tried to subtract float from string where the float is bigger than the strings length");
                    }
                    Self::String(string[..string.len() - int as usize].to_string())
                }
                Self::Bool(bool) => {
                    let int = i64::from(bool);
                    if int as usize > string.len() {
                        report_error("Tried to subtract bool from string where the bool is bigger than the strings length");
                    }
                    Self::String(string[..string.len() - int as usize].to_string())
                }
                Self::String(string2) => Self::String(string.replace(string2.as_str(), "")),
            },
            Self::Integer(int) => match other {
                Self::Integer(int2) => Self::Integer(int - int2),
                Self::Float(float) => Self::Float(int as f64 - float),
                Self::String(string) => Self::String(int.to_string() + " - " + string.as_str()),
                Self::Bool(boolean) => Self::Integer(int - i64::from(boolean)),
            },
            Self::Float(float) => match other {
                Self::Integer(int) => Self::Float(float - int as f64),
                Self::Float(float2) => Self::Float(float - float2),
                Self::String(string) => Self::String(self.to_string() + " - " + string.as_str()),
                Self::Bool(boolean) => Self::Float(float - f64::from(boolean)),
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => Self::Integer(i64::from(boolean) - i64::from(boolean2)),
                Self::String(string) => Self::String(boolean.to_string() + " - " + string.as_str()),
                Self::Float(float) => Self::Float(f64::from(boolean) - float),
                Self::Integer(int) => Self::Integer(i64::from(boolean) - int),
            },
        }
    }
}

impl std::ops::Mul for StackValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match self {
            Self::String(string) => match other {
                Self::Integer(int) => {
                    let temp = string.repeat(int.unsigned_abs() as usize);
                    Self::String(if int < 0 { temp.chars().rev().collect() } else { temp })
                }
                Self::Float(float) => {
                    let temp = string.repeat(float.abs().floor() as usize) + &string[0..(string.len() as f64 * float.abs().fract()) as usize];
                    Self::String(if float < 0.0 { temp.chars().rev().collect() } else { temp })
                }
                Self::Bool(boolean) => Self::String(if boolean { string } else { String::new() }),
                Self::String(string2) => Self::String(string.chars().interleave(string2.chars()).collect()),
            },
            Self::Integer(int) => match other {
                Self::Integer(int2) => Self::Integer(int * int2),
                Self::Float(float) => Self::Float(int as f64 * float),
                Self::String(string) => Self::String(string) * Self::Integer(int),
                Self::Bool(boolean) => Self::Integer(int * i64::from(boolean)),
            },
            Self::Float(float) => match other {
                Self::Integer(int) => Self::Float(float * int as f64),
                Self::Float(float2) => Self::Float(float * float2),
                Self::String(string) => Self::String(string) * Self::Float(float),
                Self::Bool(boolean) => Self::Float(float * f64::from(boolean)),
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => Self::Integer(i64::from(boolean && boolean2)),
                Self::String(string) => Self::String(string.repeat(usize::from(boolean))),
                Self::Float(float) => Self::Float(f64::from(boolean) * float),
                Self::Integer(int) => Self::Integer(i64::from(boolean) * int),
            },
        }
    }
}

impl std::ops::Div for StackValue {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match self {
            Self::String(string) => match other {
                Self::Integer(int) => Self::String(string) * Self::Float(1.0 / int as f64),
                Self::Float(float) => Self::String(string) * Self::Float(1.0 / float),
                Self::Bool(boolean) => Self::String(if boolean { String::new() } else { string }),
                Self::String(string2) => Self::String(string.split(string2.as_str()).collect::<Vec<_>>().join("/")),
            },
            Self::Integer(int) => match other {
                Self::Integer(int2) => {
                    if int2 == 0 {
                        report_error("Tried to divide by 0");
                    }
                    Self::Float(int as f64 / int2 as f64)
                }
                Self::Float(float) => {
                    if float == 0.0 {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(int as f64 / float)
                }
                Self::String(string) => Self::String(int.to_string() + " / " + string.as_str()),
                Self::Bool(boolean) => Self::Integer(int / i64::from(boolean)),
            },
            Self::Float(float) => match other {
                Self::Integer(int) => {
                    if int == 0 {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(float / int as f64)
                }
                Self::Float(float2) => {
                    if float2 == 0.0 {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(float / float2)
                }
                Self::String(string) => Self::String(float.to_string() + " / " + string.as_str()),
                Self::Bool(boolean) => {
                    if !boolean {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(float / f64::from(boolean))
                }
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => Self::Integer(i64::from(boolean) / i64::from(boolean2)),
                Self::String(string) => Self::String(boolean.to_string() + " / " + string.as_str()),
                Self::Float(float) => {
                    if float == 0.0 {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(f64::from(boolean) / float)
                }
                Self::Integer(int) => {
                    if int == 0 {
                        report_error("Tried to divide by 0")
                    }
                    Self::Float(f64::from(boolean) / int as f64)
                }
            },
        }
    }
}

impl StackValue {
    pub fn loose_equal(&self, other: &Self) -> bool {
        match self {
            Self::String(string) => string == &other.to_string(),
            Self::Integer(int) => match other {
                Self::Integer(int2) => int == int2,
                Self::Float(float) => int == &(float.round() as i64),
                Self::Bool(boolean) => *int == i64::from(*boolean),
                Self::String(_) => other == self,
            },
            Self::Float(float) => match other {
                Self::Float(float2) => float == float2,
                Self::Bool(boolean) => *float == f64::from(*boolean),
                _ => other == self,
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => boolean == boolean2,
                _ => other == self,
            },
        }
    }
}

impl std::cmp::PartialOrd for StackValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Self::String(string) => string.partial_cmp(&other.to_string()),
            Self::Integer(int) => match other {
                Self::Integer(int2) => int.partial_cmp(int2),
                Self::Float(float) => ((*int) as f64).partial_cmp(float),
                Self::Bool(boolean) => int.partial_cmp(&i64::from(*boolean)),
                Self::String(string) => int.to_string().partial_cmp(string),
            },
            Self::Float(float) => match other {
                Self::Float(float2) => float.partial_cmp(float2),
                Self::Integer(int) => float.partial_cmp(&(*int as f64)),
                Self::Bool(boolean) => float.partial_cmp(&(f64::from(*boolean))),
                Self::String(string) => float.to_string().partial_cmp(string),
            },
            Self::Bool(boolean) => match other {
                Self::Bool(boolean2) => boolean.partial_cmp(boolean2),
                Self::Integer(int) => i64::from(*boolean).partial_cmp(int),
                Self::Float(float) => f64::from(*boolean).partial_cmp(float),
                Self::String(string) => boolean.to_string().partial_cmp(string),
            },
        }
    }
}

#[derive(EnumCountMacro, Clone, PartialEq, Debug)]
pub enum Token {
    /// Just a fucking n̶u̶m̶b̶e̶r̶ value✨
    StackValue(StackValue),
    /// Addition. Often used to add numbers.
    Add,
    /// Subtraction. Nothin' more, Nothin' less.
    Subtract,
    /// Multiplication. Very similar to multiplication.
    Multiply,
    /// Division. Quoted in famous works such as "Math".
    Divide,
    /// Used to duplicate things, much like mitosis. a -- a a
    Dup,
    /// Drops the thing, much like I drop depth charges at 55°16'06.9"S
    /// 13°06'37.3"W (for legal reasons, this is a joke). a --
    Drop,
    /// Swaps the two things, much like the process of nurses swapping
    /// babies in hospitals. a b -- b a
    Swap,
    /// Lets the thing jump over. No good jokes here. a b -- a b a
    Over,
    /// Rotates three things, much like my testicles. a b c -- b c a
    Rot,
    /// Prints da thang, no matter what it is (least racist keyword). a --
    Print,
    /// if-condition, often used by white people. The u32 is an offset to
    /// jump to.
    If(usize),
    /// elif is like an elif,
    Elif(usize),
    /// else-condition exists but is non-existent in tokens because
    Else(usize),
    /// fi also exists, but non-existent in tokens because
    /// `MrBeast!` is a mixture between fi and elif:
    /// Once it is reached, one of the if/(el)ifs has already executed,
    /// meaning it will jump to fi.
    MrBeast(usize),
    /// Equality, much like the thing globally not yet reached.
    Eq,
    /// Strict equality, similar to javascripts strict equality thing.
    Seq,
    /// Even stricter equality, stricter than javascripts triple equal.
    Sseq,
    /// Inequality,
    Ineq,
    /// Strict inequality, similar to javascripts strict inequality thing.
    Sineq,
    /// Even stricter inequality, stricter than javascripts double inequal.
    Ssineq,
    /// Greater-than,
    Gt,
    /// Less-than,
    Lt,
    /// Greater-than or equal,
    Ge,
    /// Less-than or equal,
    Le,
    /// Shift right,
    Shr,
    /// Shift left,
    Shl,
    /// Or, both bool and integer
    Or,
    /// And, both bool and integer
    And,
    /// Not, both bool and integer
    Not,
    /// Xor, both bool and integer
    Xor,
    /// Does absolutely nothing, much like this programming language.
    Dummy,
}

pub fn parse_file(path: &PathBuf) -> anyhow::Result<Vec<Token>> {
    let contents = match std::fs::read_to_string(path) {
        Ok(string) => string,
        Err(err) => {
            report_error(format!("File could not be read because {err}").as_str());
        }
    };
    parse_string(contents)
}

pub fn parse_string(mut contents: String) -> anyhow::Result<Vec<Token>> {
    contents += " "; // This prevents a parser bug where the parser ignores the last token if it
                     // isn't followed by whitespace. No fix for this is planned.

    let mut tokens = Vec::new();
    let mut if_statements: Vec<usize> = Vec::with_capacity(4); // People are gonna wanna nest at least four times.
    let mut elif_statements: HashMap<usize, usize> = HashMap::new();
    let mut mr_beast_statements: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut else_statements: Vec<usize> = Vec::with_capacity(3);
    let mut is_commenting = false;
    let mut is_stringing = false;

    let mut word = String::new();
    let mut index = 0;

    let mut chars = contents.chars();
    while let Some(chr) = chars.next() {
        if chr == '\'' {
            if is_stringing {
                tokens.push(Token::StackValue(StackValue::String(word)));
                word = String::new();
                index += 1;
            }
            is_stringing = !is_stringing;
            continue;
        }
        if is_stringing {
            if chr == '\\' {
                word.push(match chars.next() {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('0') => '\0',
                    Some('\'') => '\'',
                    Some(x) => anyhow::bail!(format!("Unexpected escape character '{x}'")),
                    None => anyhow::bail!("Expected escape character, found end of file"),
                });
            } else {
                word.push(chr);
            }
            continue;
        }
        if !chr.is_whitespace() {
            word.push(chr);
            continue;
        }
        if chr.is_whitespace() && word.is_empty() {
            continue;
        }
        if !is_commenting {
            static_assertions::const_assert_eq!(Token::COUNT, 32);
            let token = match word.as_str() {
                "+" => Token::Add,
                "-" => Token::Subtract,
                "*" => Token::Multiply,
                "/" => Token::Divide,
                "=" => Token::Eq,
                "==" => Token::Seq,
                "===" => Token::Sseq,
                "!=" => Token::Ineq,
                "!==" => Token::Sineq,
                "!===" => Token::Ssineq,
                ">" => Token::Gt,
                "<" => Token::Lt,
                "=>" => Token::Ge,
                "<=" => Token::Le,
                ">>" => Token::Shr,
                "<<" => Token::Shl,
                "or" => Token::Or,
                "and" => Token::And,
                "not" => Token::Not,
                "xor" => Token::Xor,
                "if" => {
                    if_statements.push(index);
                    Token::If(usize::MAX)
                }
                "elif" => {
                    let Some(if_index) = if_statements.last() else {
                        anyhow::bail!("Found 'elif' without 'if'");
                    };
                    if elif_statements.try_insert(*if_index, index).is_err() {
                        anyhow::bail!("Found two 'elif's next to eachother without 'MrBeast' between them")
                    }
                    Token::Elif(usize::MAX)
                }
                "MrBeast!" => {
                    let Some(if_index) = if_statements.last() else {
                        anyhow::bail!("Found 'MrBeast' closing an if-statement that doesn't exist");
                    };

                    if let Some(mr_beasts) = mr_beast_statements.get_mut(if_index) {
                        mr_beasts.push(index);
                        if let Some(elif_index) = elif_statements.remove(if_index)
                            && let Some(Token::Elif(jump_addr)) = tokens.get_mut(elif_index)
                        {
                            *jump_addr = index + 1;
                        } else {
                            anyhow::bail!("This is embarrassing");
                        }
                    } else {
                        mr_beast_statements.insert(*if_index, vec![index]);
                        if let Some(Token::If(jump_addr)) = tokens.get_mut(*if_index) {
                            *jump_addr = index + 1;
                        } else {
                            anyhow::bail!("This is embarrassing");
                        }
                    }
                    Token::MrBeast(usize::MAX)
                }
                "else" => {
                    let Some(if_index) = if_statements.last() else {
                        anyhow::bail!("Found 'else' without match 'if'");
                    };
                    if let Some(elif_index) = elif_statements.remove(if_index) {
                        if let Some(Token::Elif(jump_addr)) = tokens.get_mut(elif_index) {
                            *jump_addr = index + 1;
                        } else {
                            anyhow::bail!("This is embarrassing");
                        }
                    } else if let Some(Token::If(jump_addr)) = tokens.get_mut(*if_index) {
                        *jump_addr = index + 1;
                    } else {
                        anyhow::bail!("This is embarrassing");
                    }

                    else_statements.push(index);
                    Token::Else(usize::MAX)
                }
                "fi" => {
                    let Some(if_index) = if_statements.pop() else {
                        anyhow::bail!("Found 'fi' closing an if-statement that doesn't exist");
                    };
                    if let Some(mr_beasts) = mr_beast_statements.remove(&if_index) {
                        for mr_beast_index in mr_beasts {
                            if let Some(Token::MrBeast(jump_addr)) = tokens.get_mut(mr_beast_index) {
                                *jump_addr = index;
                            }
                        }
                    } else if let Some(Token::If(jump_addr)) = tokens.get_mut(if_index) {
                        if *jump_addr == usize::MAX {
                            *jump_addr = index;
                        }
                    } else {
                        anyhow::bail!("This is embarrassing");
                    }

                    if let Some(else_index) = else_statements.pop() {
                        let Some(Token::Else(else_statement)) = tokens.get_mut(else_index) else {
                            anyhow::bail!("This is embarrassing");
                        };
                        *else_statement = index;
                    }
                    Token::Dummy
                }

                "dup" => Token::Dup,
                "drop" => Token::Drop,
                "swap" => Token::Swap,
                "over" => Token::Over,
                "rot" => Token::Rot,
                "print" => Token::Print,
                "comment" => {
                    is_commenting = true;
                    Token::Dummy // This will just chill in the tokens
                }
                x if let Ok(int) = x.parse::<i64>() => Token::StackValue(StackValue::Integer(int)),
                x if let Ok(float) = x.parse::<f64>() => Token::StackValue(StackValue::Float(float)),
                x if let Ok(boolean) = x.parse::<bool>() => Token::StackValue(StackValue::Bool(boolean)),
                x if x.len() == 3 && x.chars().next().is_some_and(|c| c == '"') && x.chars().last().is_some_and(|c| c == '"') => {
                    Token::StackValue(StackValue::Integer(x[1..x.len() - 1].chars().next().expect("This should work") as i64))
                }
                unrecognized => {
                    anyhow::bail!(format!("Unrecognized token {unrecognized}"));
                }
            };
            tokens.push(token);
        }
        if word == "no_comment" {
            is_commenting = false;
        }
        word.clear();
        index += 1;
    }
    if !if_statements.is_empty() {
        anyhow::bail!("Unclosed if-statement");
    }
    if !else_statements.is_empty() {
        anyhow::bail!("This shouldn't happen: Dangling else-statement");
    }
    if !mr_beast_statements.is_empty() {
        anyhow::bail!("This shouldn't happen: Dangling MrBeast-statements");
    }
    if is_commenting {
        anyhow::bail!("Unclosed comment");
    }

    Ok(tokens)
}

pub fn execute_tokens<T: std::io::Write>(tokens: &[Token], #[cfg(feature = "silly")] out_of_free_runs: bool, writable: &mut T, time_limit: Option<Duration>) -> anyhow::Result<Vec<StackValue>> {
    #[cfg(feature = "silly")]
    if out_of_free_runs {
        let local_ip = local_ip_address::local_ip().expect("I'm so done").to_string();
        let info = geolocation::find(local_ip.as_str()).expect("What");
        let (longitude, latitude) = (info.longitude.parse::<f64>().unwrap_or(180.0), info.latitude.parse::<f64>().unwrap_or(0.0));
        let openstreetmap = geocoding::Openstreetmap::new();
        let location = openstreetmap.reverse(&geocoding::Point::new(-longitude, 180.0 - latitude));
        println!(
            "Connecting to our servers in {}, our datacenter that is nearest to you!",
            location.unwrap_or_else(|_| Some("Antarctica".to_string())).unwrap_or_else(|| "Antarctica".to_string())
        );
        std::thread::sleep(std::time::Duration::from_secs(1));
    }

    let mut stack: Vec<StackValue> = Vec::new();
    let mut i: usize = 0;

    let start = Instant::now();
    while let Some(token) = tokens.get(i) {
        #[cfg(feature = "silly")]
        if out_of_free_runs {
            std::thread::sleep(std::time::Duration::from_millis(300));
        }
        if let Some(limit) = time_limit
            && start - Instant::now() > limit
        {
            anyhow::bail!("Exceeded time limit!");
        }
        static_assertions::const_assert_eq!(Token::COUNT, 32);
        // println!("{token:?}, {i}");
        match token {
            Token::Dummy => {}
            Token::StackValue(x) => stack.push(x.clone()),
            Token::Add => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(a + b);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for an addition to be made");
                }
            }
            Token::Subtract => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(b - a);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for a subtraction to be made");
                }
            }
            Token::Multiply => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(a * b);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for a multiplication to be made");
                }
            }
            Token::Divide => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(b / a);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for a division to be made");
                }
            }
            Token::Eq => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(a.loose_equal(&b)));
                } else {
                    anyhow::bail!("The stack must contain at least two elements fo them to be compared");
                }
            }
            Token::Ineq => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(!a.loose_equal(&b)));
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Seq => todo!("Impl strict equal"),
            Token::Sineq => todo!("Impl strict equal"),
            Token::Sseq => todo!("Impl strict strict equal"),
            Token::Ssineq => todo!("Impl strict strict equal"),
            Token::Gt => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(b > a));
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Lt => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(b < a));
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Ge => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(b >= a));
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Le => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(b <= a));
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Shr => todo!("Impl shr"),
            Token::Shl => todo!("Impl shl"),
            Token::Or => todo!("Impl or"),
            Token::And => todo!("Impl and"),
            Token::Not => todo!("Impl not"),
            Token::Xor => todo!("Impl xor"),
            Token::Dup => {
                if let Some(a) = stack.last() {
                    stack.push(a.clone());
                } else {
                    anyhow::bail!("The stack must contain at least one element for it to be duplicated");
                }
            }
            Token::Drop => {
                if stack.is_empty() {
                    anyhow::bail!("The stack must contain at least one element for it to be dropped");
                }
                let _ = stack.pop();
            }
            Token::Swap => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(a);
                    stack.push(b);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be swapped");
                }
            }
            Token::Over => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(b.clone());
                    stack.push(a);
                    stack.push(b);
                } else {
                    anyhow::bail!("The stack must contain at least two elements for them to be overed");
                }
            }
            Token::Rot => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                    && let Some(c) = stack.pop()
                {
                    stack.push(b);
                    stack.push(a);
                    stack.push(c);
                } else {
                    anyhow::bail!("The stack must contain at least three elements for them to be roted");
                }
            }
            Token::Print => {
                if let Some(a) = stack.pop() {
                    if write!(writable, "{a}").is_err() {
                        anyhow::bail!("Couldn't write to writable");
                    };
                } else {
                    anyhow::bail!("The stack must contain at least one element for it to be printed");
                }
            }
            Token::If(jump_addr) | Token::Elif(jump_addr) => {
                if let Some(StackValue::Bool(boolean)) = stack.pop() {
                    if !boolean {
                        i = *jump_addr;
                        continue;
                    }
                } else {
                    anyhow::bail!("If needs one boolean to be on the stack");
                }
            }
            Token::MrBeast(jump_addr) | Token::Else(jump_addr) => {
                i = *jump_addr;
                continue;
            } // token => todo!("Not yet impld {token:?}"),
        }
        i += 1;
    }
    println!();
    Ok(stack)
}
