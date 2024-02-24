#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(map_try_insert)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
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
    collections::{HashMap, HashSet},
    io::Write,
    path::PathBuf,
    sync::Mutex,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};
use colored::Colorize;
use geocoding::Reverse;
use inquire::{validator::Validation, CustomUserError};
use itertools::Itertools;
use rand::distributions::Distribution;
use savefile::prelude::*;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

#[macro_use]
extern crate savefile_derive;

fn report_error(string: &str) -> ! {
    panic!("{}: {string}", "ERROR".bold().red());
}

fn report_warning(string: &str) {
    eprintln!("{}: {string}", "WARNING".bold().yellow());
}

#[derive(Clone, Debug)]
enum StackValue {
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

impl std::cmp::PartialEq for StackValue {
    fn eq(&self, other: &Self) -> bool {
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

impl std::cmp::Eq for StackValue {}

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
enum Token {
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
    /// filif is a mixture between fi and elif:
    /// Once it is reached, one of the if/(el)ifs has already executed,
    /// meaning it will jump to fi.
    Filif(usize),
    /// Equality, much like the thing globally not yet reached.
    Eq,
    /// Strict equality, similar to javascripts strict equality thing.
    Seq,
    /// Even stricter equality, stricter than JavaScripts triple equal.
    Sseq,
    /// Inequality,
    Ineq,
    /// Strict inequality, similar to javascripts strict inequality thing.
    Sineq,
    /// Even stricter inequality, stricter than JavaScripts double inequal.
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

fn parse_file(path: &PathBuf) -> Vec<Token> {
    let contents = match std::fs::read_to_string(path) {
        Ok(string) => string,
        Err(err) => {
            report_error(format!("File could not be read because {err}").as_str());
        }
    };
    let mut tokens = Vec::new();
    let mut if_statements: Vec<usize> = Vec::with_capacity(4); // People are gonna wanna nest at least four times.
    let mut elif_statements: HashMap<usize, usize> = HashMap::new();
    let mut filif_statements: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut else_statements: Vec<usize> = Vec::with_capacity(3);
    let mut is_commenting = false;
    for (index, word) in contents.split_whitespace().enumerate() {
        if !is_commenting {
            static_assertions::const_assert_eq!(Token::COUNT, 32);
            let token = match word {
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
                        report_error("Found 'elif' without 'if'");
                    };
                    if elif_statements.try_insert(*if_index, index).is_err() {
                        report_error("Found two 'elif's next to eachother without 'filif' between them")
                    }
                    Token::Elif(usize::MAX)
                }
                "filif" => {
                    let Some(if_index) = if_statements.last() else {
                        report_error("Found 'filif' closing an if-statement that doesn't exist");
                    };

                    if let Some(filifs) = filif_statements.get_mut(if_index) {
                        filifs.push(index);
                        if let Some(elif_index) = elif_statements.remove(if_index)
                            && let Some(Token::Elif(jump_addr)) = tokens.get_mut(elif_index)
                        {
                            *jump_addr = index + 1;
                        } else {
                            report_error("This is embarrassing");
                        }
                    } else {
                        filif_statements.insert(*if_index, vec![index]);
                        if let Some(Token::If(jump_addr)) = tokens.get_mut(*if_index) {
                            *jump_addr = index + 1;
                        } else {
                            report_error("This is embarrassing");
                        }
                    }
                    Token::Filif(usize::MAX)
                }
                "else" => {
                    let Some(if_index) = if_statements.last() else {
                        report_error("Found 'else' without match 'if'");
                    };
                    if let Some(elif_index) = elif_statements.remove(if_index) {
                        if let Some(Token::Elif(jump_addr)) = tokens.get_mut(elif_index) {
                            *jump_addr = index + 1;
                        } else {
                            report_error("This is embarrassing")
                        }
                    } else {
                    }
                    else_statements.push(index);
                    Token::Else(usize::MAX)
                }
                "fi" => {
                    let Some(if_index) = if_statements.pop() else {
                        report_error("Found 'fi' closing an if-statement that doesn't exist");
                    };
                    if let Some(filifs) = filif_statements.remove(&if_index) {
                        filifs.iter().for_each(|filif_index| {
                            if let Some(Token::Filif(jump_addr)) = tokens.get_mut(*filif_index) {
                                *jump_addr = index;
                            }
                        });
                    }
                    if let Some(else_index) = else_statements.pop() {
                        let Some(Token::Else(else_statement)) = tokens.get_mut(else_index) else {
                            report_error("This is embarrassing");
                        };
                        *else_statement = index; // TODO: check if
                                                 // off-by-one
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
                x if x.chars().next().is_some_and(|c| c == '\'') && x.chars().last().is_some_and(|c| c == '\'') => Token::StackValue(StackValue::String(x[1..x.len() - 1].to_string())),
                x if x.len() == 3 && x.chars().next().is_some_and(|c| c == '"') && x.chars().last().is_some_and(|c| c == '"') => {
                    Token::StackValue(StackValue::Integer(x[1..x.len() - 1].chars().next().expect("This should work") as i64))
                }
                unrecognized => {
                    report_error(format!("Unrecognized token {unrecognized}").as_str());
                }
            };
            tokens.push(token);
        }
        if word == "no_comment" {
            is_commenting = false;
        }
    }
    if !if_statements.is_empty() {
        report_error("Unclosed if-statement");
    }
    if !else_statements.is_empty() {
        report_error("This shouldn't happen: Dangling else-statement");
    }
    if !filif_statements.is_empty() {
        report_error("This shouldn't happen: Dangling filif-statements");
    }

    tokens
}

fn execute_tokens(tokens: &[Token], out_of_free_runs: bool) -> Vec<StackValue> {
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
    while let Some(token) = tokens.get(i) {
        if out_of_free_runs {
            std::thread::sleep(std::time::Duration::from_millis(300));
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
                    report_error("The stack must contain at least two elements for an addition to be made");
                }
            }
            Token::Subtract => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(b - a);
                } else {
                    report_error("The stack must contain at least two elements for a subtraction to be made");
                }
            }
            Token::Multiply => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(a * b);
                } else {
                    report_error("The stack must contain at least two elements for a multiplication to be made");
                }
            }
            Token::Divide => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(b / a);
                } else {
                    report_error("The stack must contain at least two elements for a division to be made");
                }
            }
            Token::Eq => {
                if let Some(a) = stack.pop()
                    && let Some(b) = stack.pop()
                {
                    stack.push(StackValue::Bool(a == b));
                } else {
                    report_error("The stack must contain at least two elements for them to be compared");
                }
            }
            Token::Dup => {
                if let Some(a) = stack.last() {
                    stack.push(a.clone());
                } else {
                    report_error("The stack must contain at least one element for it to be duplicated");
                }
            }
            Token::Drop => {
                if stack.is_empty() {
                    report_error("The stack must contain at least one element for it to be dropped");
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
                    report_error("The stack must contain at least two elements for them to be swapped");
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
                    report_error("The stack must contain at least two elements for them to be overed");
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
                    report_error("The stack must contain at least three elements for them to be roted");
                }
            }
            Token::Print => {
                if let Some(a) = stack.pop() {
                    print!("{a}");
                } else {
                    report_error("The stack must contain at least one element for it to be printed");
                }
            }
            Token::If(jump_addr) | Token::Elif(jump_addr) => {
                if let Some(StackValue::Bool(boolean)) = stack.pop() {
                    if !boolean {
                        i = *jump_addr;
                        continue;
                    }
                } else {
                    report_error("If needs one boolean to be on the stack");
                }
            }
            Token::Filif(jump_addr) | Token::Else(jump_addr) => {
                i = *jump_addr;
                continue;
            }
            token => todo!("Not yet impld {token:?}"),
        }
        i += 1;
    }
    println!();
    stack
}

// This part of the program serves absolutely no reason is just here
// because I find it incredibly funny. More about the motivation can be read here: https://github.com/gianzellweger/badlang/blob/release/MOTIVATION.md

// List of "features" and a roadmap can be found at https://github.com/gianzellweger/badlang/issues/3

#[derive(EnumCountMacro, Debug, PartialEq, Eq)]
enum Advertisement {
    Temu,
    Shein,
    BetterHelp,
    Nestle,
    JohnsonJohnson,
    CocaCola,
    McDonalds,
}

// This is satire, but mostly based in fact. I encourage you to correct me
// if any newer information surfaces. I also encourage you to include more
// examples if there are more current ones.
impl std::fmt::Display for Advertisement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Temu => "You support slave labor and genocide? Buy from our sponsor Temu!",
            Self::Shein => "You support exploiting small designers, genocide and slave labor? Buy clothes from our sponsor Shein!",
            Self::BetterHelp => "You want Facebook to have your sensitive medical records? Get online therapy from our sponsor BetterHelp!",
            Self::Nestle => "You enjoy stealing the water of African villages, bottling it and then selling it to them? Buy from our sponsor Nestlé!",
            Self::JohnsonJohnson => "You enjoy getting even richer by letting people in third-world countries die? Buy drugs from our sponsor Johnson & Johnson!",
            Self::CocaCola => "You enjoy infesting our seas with microplastics? Buy a refreshing beverage from our sponsor Coca-Cola!",
            Self::McDonalds => "You support McDonalds? Go shop at McDonalds!",
        })
    }
}

impl Distribution<Advertisement> for rand::distributions::Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Advertisement {
        static_assertions::const_assert_eq!(Advertisement::COUNT, 7); // If the match below isn't updated when a new Advertisement is added, it
                                                                      // won't be added to the rotation
        match rng.gen_range(0..Advertisement::COUNT) {
            0 => Advertisement::Temu,
            1 => Advertisement::Shein,
            2 => Advertisement::BetterHelp,
            3 => Advertisement::Nestle,
            4 => Advertisement::JohnsonJohnson,
            5 => Advertisement::CocaCola,
            6 => Advertisement::McDonalds,
            _ => unreachable!(),
        }
    }
}

#[allow(dead_code)]
#[allow(clippy::needless_pass_by_value)]
#[tauri::command]
fn tauri_handler<R: tauri::Runtime>(window: tauri::Window<R>) {
    static VELOCITY: Mutex<(i32, i32)> = Mutex::new((4, 4));
    static POSITION: Mutex<(i32, i32)> = Mutex::new((0, 0));

    let (screen_x, screen_y) = window
        .current_monitor()
        .ok()
        .flatten()
        .map(|monitor| *monitor.size())
        .map_or((1920, 1080), |pos| (pos.height as i32, pos.width as i32));

    let mut position = POSITION.lock().expect("Unreachable");
    let mut velocity = VELOCITY.lock().expect("Unreachable");
    if position.0 > screen_x || position.0 < 0 {
        velocity.0 = -velocity.0;
    }
    if position.1 > screen_y || position.1 < 0 {
        velocity.1 = -velocity.1;
    }

    position.0 += velocity.0;
    position.1 += velocity.1;
    drop(velocity);

    let _ = window.set_position(tauri::Position::Physical((*position).into()));
    drop(position);

    let _ = window.set_focus();
}

#[derive(Savefile, Clone, Debug)]
struct Account {
    name:               String,
    // Yes I am actually taking a programming account (that doesn't do anything) serious enough to actually use encryption.
    password_hash:      String,
    version:            String,
    google_auth_secret: String,
}

#[derive(Savefile, Clone, Debug, Default)]
struct SaveData {
    account:           Option<Account>,
    runs_so_far:       usize,
    last_update:       u64,  // This is in seconds since UNIX_EPOCH
    dialogs_displayed: bool, // It makes sense to display them only once per device, as this is how it works in serious applications.
}

// These files are used to measure download speed. There are multiple
// because there are actual server outages and I don't want people to miss
// out on this one just because of such a tiny problem
const TEST_FILE: [(&str, u64); 4] = [
    ("https://speed.hetzner.de/100MB.bin", 100_000_000),
    ("https://ash-speed.hetzner.com/100MB.bin", 100_000_000),
    ("https://hel1-speed.hetzner.com/100MB.bin", 100_000_000),
    ("https://fsn1-speed.hetzner.com/100MB.bin", 100_000_000),
];

// The cookies don't actually do anything. Yet.
const TYPES_OF_COOKIES: [&str; 25] = [
    "Authentication Cookies",
    "Personalization Cookies",
    "Shopping Cart Cookies",
    "Analytics Cookies",
    "Ad Targeting Cookies",
    "Session Management Cookies",
    "Security Cookies",
    "Performance Cookies",
    "Social Media Integration Cookies",
    "Localization Cookies",
    "Predictive Preference Cookies",
    "Enhanced Virtual Reality Cookies",
    "Celebrity Influence Cookies",
    "Historical Context Cookies",
    "Multiverse Browsing Cookies",
    "Visual Communication Cookies",
    "Extraterrestrial Inspiration Cookies",
    "Mood Enhancement Cookies",
    "Future Trend Cookies",
    "Personalized Insights Cookies",
    "Predictive Health Cookies",
    "Behavioral Employment Cookies",
    "Government Compliance Cookies",
    "Mood Manipulation Cookies",
    "Social Credit Cookies",
];

// These Terms of Service are not actually legally binding, refer to the
// license for the actual legalese, don't sue me, etc.
const TERMS_OF_SERVICE: &str = r#"Terms of Service

1. Acceptance of Terms
By continuing to read these musings, you agree to acknowledge the lighthearted nature of the content. If, however, you find yourself questioning the validity of our statements, consider this your cue to exit stage left.

2. Changes to Terms
We reserve the right to evolve these "Terms" at our discretion. Changes may occur without notice, and users will navigate the subtle shifts in the landscape unassisted.

3. User Accounts
Creating a user account involves a process known only to a select few. Your password is stored in a secure vault, accessible only to those who possess the secret passphrase.

4. Content
You can't post any content on BadLang™. Please exercise discretion, as some forms of expression may be subject to interpretation by our team of resident enigmatologists.

5. Intellectual Property
BadLang™ and its features belong to the collective imagination. Attempts to assert ownership may lead to a journey through the labyrinthine corridors of paradoxes and riddles.

6. Termination
We reserve the right to limit access to BadLang™. Banishment may lead you to discover other realms, where the unknown becomes the known.

7. Governing Law
These "Terms" are subject to the laws of Wakanda. In the event of any disputes, resolutions may be found in the concealed archives of ancient wisdom.

8. Contact Us
For inquiries about these enigmatic Terms, submit your questions through channels familiar to those initiated into the subtleties of subtle communication.

ChatGPT
BadLang™ Legal Team

To accept, please type: 
I solemnly declare that I've thoroughly read and understood the Terms of Service, and I'm committed to adhering to its provisions
"#;

// Note: this is distinct from the text above in one very important way:
// These spaces are actually spaces, while the ones above are the Unicode
// character U+2002, which looks identical. This is to prevent copying.
const ACCEPTANCE_PHRASE: &str = "I solemnly declare that I've thoroughly read and understood the Terms of Service, and I'm committed to adhering to its provisions";

const CHANCE_OF_SERVER_MAINTENANCE: f64 = 0.1; // Also known as 10%

const FREE_RUNS: usize = 5;

const UPDATE_SIZE: u64 = 1_000_000_000; // 1GB
const UPDATE_VARIATION: u64 = 300_000_000; // 300MB
const DOWNLOAD_SPEED_VARIATION: f64 = 0.8; // 80%
const DOWNLOAD_UPDATE_INTERVAL: f64 = 0.5; // This is in seconds

const FIRST_OPTION: &str = "Yes, proceed to login";
const SECOND_OPTION: &str = "No, proceed to signup";

fn fetch_data(url: &str) -> Option<Vec<String>> {
    // Make a blocking GET request
    let response = reqwest::blocking::get(url).ok()?;

    // Check if the request was successful (status code 200 OK)
    if response.status().is_success() {
        // Read the response body as a string
        let body = response.text().ok()?;

        // Split the data by new-line
        let lines: Vec<String> = body.lines().map(String::from).collect();

        Some(lines)
    } else {
        // If the request was not successful, return an error
        None
    }
}

#[allow(clippy::unnecessary_wraps)]
fn password_validator(password: &str) -> Result<Validation, CustomUserError> {
    static TOP_100_PASSWORDS: Mutex<Option<Vec<String>>> = Mutex::new(None);

    // Yes this is very much a stolen idea from the Password game. I thought
    // it's a nice nod to the game after basically copying half its
    // concept
    let todays_wordle_answer = reqwest::blocking::get(format!("https://www.nytimes.com/svc/wordle/v2/{}.json", chrono::offset::Local::now().date_naive().format("%Y-%m-%d"))).map_or(None, |res| {
        match res.json::<serde_json::Value>() {
            Ok(serde_json::Value::Object(map)) => match map.get("solution") {
                Some(serde_json::Value::String(solution)) => Some(solution.clone()),
                _ => None,
            },
            _ => None,
        }
    });

    let mut top_100_passwords = TOP_100_PASSWORDS.lock().expect("Unreachable");

    if top_100_passwords.is_none() {
        *top_100_passwords =
            Some(fetch_data("https://raw.githubusercontent.com/danielmiessler/SecLists/master/Passwords/Common-Credentials/10-million-password-list-top-10000.txt").unwrap_or_default());
    };

    let password_list = if top_100_passwords.as_ref().is_some_and(Vec::is_empty) {
        None
    } else {
        top_100_passwords.as_ref()
    };

    Ok(if password.is_empty() {
        Validation::Invalid("The password is required".into())
    } else if password.chars().count() < 18 {
        Validation::Invalid("Your password needs to be at least 18 characters long".into())
    } else if password.chars().count() > 26 {
        Validation::Invalid("Your password cannot exceed 26 characters".into())
    } else if password.chars().any(char::is_whitespace) {
        Validation::Invalid("Your password may not contain any whitespace".into())
    } else if !password.chars().any(char::is_uppercase) {
        Validation::Invalid("Your password must contain an uppercase letter".into())
    } else if !password.chars().any(char::is_lowercase) {
        Validation::Invalid("Your password must contain a lowercase letter".into())
    } else if !password.chars().any(|c| c.is_ascii_digit()) {
        Validation::Invalid("Your password must contain a number".into())
    } else if password.chars().all(char::is_alphanumeric) {
        Validation::Invalid("Your password must contain a special character".into())
    } else if !password.chars().any(|c| unic_emoji_char::is_emoji(c) && !c.is_ascii()) {
        Validation::Invalid("Your password must contain an emoji".into())
    } else if password.chars().collect::<HashSet<_>>().len() < password.chars().count() {
        Validation::Invalid("Your password may not contain duplicate characters".into())
    } else if password.contains("123") || password.contains("69") || password.contains("420") || password.to_lowercase().contains("password") {
        Validation::Invalid("Your password may not contain any well known sequences".into())
    } else if let Some(wordle_answer) = todays_wordle_answer.as_ref()
        && !password.to_lowercase().contains(wordle_answer.to_lowercase().as_str())
        && wordle_answer.chars().collect::<HashSet<_>>().len() == wordle_answer.len()
    {
        Validation::Invalid("Your password must contain today's wordle answer".into())
    } else if let Some(passwords) = password_list
        && let Some(part) = passwords.iter().find(|&pw| password.contains(pw))
        && let Some(wordle_answer) = todays_wordle_answer
        && !passwords.contains(&wordle_answer)
    {
        Validation::Invalid(format!("Your password contains one of the top 100 most common passwords, '{part}', making it insecure").into())
    } else {
        Validation::Valid
    })
}

fn sillyness(save_data: &mut SaveData) {
    // This macos version panics for some reason currently. This code should
    // theoretically work, tauri is just buggy. #[cfg(target_os = "macos")]
    // {
    //     println!(
    //         "Because you're on MacOS, the Video Player sadly cannot run
    // on another thread. You need to quit it to continue!"
    //     );
    //     let mut app = tauri::Builder::default()
    //         .invoke_handler(tauri::generate_handler!(tauri_handler))
    //         .build(tauri::generate_context!())
    //         .expect("error while building tauri application");

    //     loop {
    //         let iteration = app.run_iteration();
    //         if iteration.window_count == 0 {
    //             tauri::api::process::kill_children();
    //             break;
    //         }
    //     }
    // }
    #[cfg(any(target_os = "linux", target_os = "windows"))]
    jod_thread::spawn(|| {
        tauri::Builder::default()
            .any_thread()
            .invoke_handler(tauri::generate_handler!(tauri_handler))
            .build(tauri::generate_context!())
            .expect("error while building tauri application")
            .run(|_app_handle, event| {
                if let tauri::RunEvent::ExitRequested { api, .. } = event {
                    api.prevent_exit();
                }
            });
    })
    .detach();

    let has_internet = reqwest::blocking::get("https://google.com").is_ok(); // Googles servers are always up so I'm using them
    if !has_internet {
        report_error("To use this programming language, you need an internet connection!");
    }

    if fastrand::f64() <= CHANCE_OF_SERVER_MAINTENANCE {
        report_error("Our servers are currently experiencing outages, but we are working hard to get them back online!");
    }

    let save_data_clone = save_data.clone();
    #[cfg(not(all(target_os = "macos", not(debug_assertions))))] // The dialogs currently segfault on intel macs
    jod_thread::spawn(move || {
        if !save_data_clone.dialogs_displayed {
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to access your contacts. Allow?"#)
                .show_confirm();
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to access your location. Allow?"#)
                .show_confirm();
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to make and receive phone calls on your behalf. Allow?"#)
                .show_confirm();
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to manage incoming network connections. Allow?"#)
                .show_confirm();
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to access your passwords. Allow?"#)
                .show_confirm();
            let _ = native_dialog::MessageDialog::new()
                .set_type(native_dialog::MessageType::Warning)
                .set_title("BadLang™")
                .set_text(r#""BadLang™" wants to access your liver. Allow?"#)
                .show_confirm();
        }
    })
    .detach();

    if !save_data.dialogs_displayed {
        save_data.dialogs_displayed = true;
    }

    let _ = notify_rust::Notification::new()
        .summary("Do you want to subscribe to our mailing list?")
        .body("Shoot an email to mailinglist@badlang.dev and you will automatically be added to the mailing list!")
        .appname("Mailing List Subscriber")
        .auto_icon()
        .sound_name("alarm-clock-elapsed")
        .timeout(0)
        .show(); // This notification will not go away unless you dismiss it.

    let random_advertisement: Advertisement = rand::random();

    println!("{}", random_advertisement.to_string().yellow().on_purple().bold());

    let accepted_cookies = match inquire::Select::new("This programming language uses cookies.", vec!["Accept all", "Customize"])
        .without_help_message()
        .prompt()
        .expect("What")
    {
        "Customize" => inquire::MultiSelect::new("Select the kind of cookies you want", TYPES_OF_COOKIES.to_vec())
            .without_help_message()
            .with_default((0..25).collect::<Vec<_>>().as_slice())
            .prompt()
            .expect("No"),
        _ => TYPES_OF_COOKIES.to_vec(),
    };

    if !accepted_cookies.is_empty() {
        println!("The types of cookies you accepted are:");
        for cookie in accepted_cookies {
            println!("- {cookie}");
        }
    }

    println!();

    let argon2 = Argon2::default();

    match inquire::Select::new("To use this programming language, you need a BadLang™ Account. Do you already have one?", vec![
        FIRST_OPTION,
        SECOND_OPTION,
    ])
    .without_help_message()
    .prompt()
    .expect("What")
    {
        SECOND_OPTION => {
            let name = inquire::Text::new("Enter your username: ")
                .with_validator(inquire::required!())
                .with_validator(inquire::min_length!(16))
                .with_validator(|name: &str| {
                    Ok(if name.chars().all(|c| c.is_lowercase() || c.is_ascii_digit() || c == '.' || c == '_') {
                        Validation::Valid
                    } else {
                        Validation::Invalid("Usernames may only contain lowercase letters, numbers, underscores and dashes".into())
                    })
                })
                .prompt()
                .expect("Enter your username");

            let password = inquire::Password::new("Enter your password: ")
                .without_confirmation()
                .with_validator(password_validator)
                .prompt()
                .expect("Enter a password");
            let password_repetition = inquire::Password::new("Repeat your password: ").without_confirmation().prompt().expect("Enter a password");
            let password_repetition2 = inquire::Password::new("Repeat your password again: ").without_confirmation().prompt().expect("Enter a password");

            if !(password == password_repetition && password_repetition == password_repetition2 && password == password_repetition2) {
                report_error("Your passwords do not match!");
            }

            let salt = SaltString::generate(&mut OsRng);
            let password_hash = argon2.hash_password(password.as_bytes(), &salt).expect("What happened? Why did the hasher fail?").to_string();

            let ga = google_authenticator::GoogleAuthenticator::new();
            let secret = ga.create_secret(32);
            let mut qr_code_url = ga.qr_code_url(secret.as_str(), name.as_str(), "Badlang™", 500, 500, google_authenticator::ErrorCorrectionLevel::High);
            qr_code_url = qr_code_url.replace('|', "%7C");
            let _ = open::that(qr_code_url);

            print!("A QR-code with your Google Authenticator code just opened in your browser. Do scan it, because it will never ever be available again! Press enter as soon as you're ready ");
            let _ = std::io::stdout().flush();
            {
                let mut buffer = String::new();
                let stdin = std::io::stdin();
                let _ = stdin.read_line(&mut buffer);
            }

            inquire::Text::new(TERMS_OF_SERVICE)
                .with_validator(|v: &str| {
                    if v == ACCEPTANCE_PHRASE {
                        Ok(Validation::Valid)
                    } else if v == ACCEPTANCE_PHRASE.replace(' ', " ") {
                        Ok(Validation::Invalid("You can't just copy-paste and expect it to work!".into()))
                    } else {
                        Ok(Validation::Invalid("Incorrect text!".into()))
                    }
                })
                .prompt()
                .expect("Enter the phrase");

            save_data.account = Some(Account {
                name,
                password_hash,
                version: semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("WTF cargo").to_string(),
                google_auth_secret: secret,
            });
            println!("{}! Account saved!", "SUCCESS".green());
        }
        FIRST_OPTION => {
            let Some(account) = save_data.account.as_ref() else {
                report_error("You do in fact not have an account");
            };

            let name = inquire::Text::new("Enter your username: ")
                .with_validator(inquire::required!())
                .with_validator(inquire::min_length!(16))
                .prompt()
                .expect("Enter your username");

            let password = inquire::Password::new("Enter your password: ").without_confirmation().prompt().expect("Enter a password");

            let confirm_signs = ["✅", "✅", "✔️", "✓", "✔"];
            let cancel_signs = [
                "𝕏", "✗", "✘", "×", "Χ", "χ", "Х", "х", "╳", "☓", "✕", "✖", "❌", "❎", "⨉", "⨯", "🗙", "🗴", "🞨", "🞩", "🞪", "🞫", "🞬", "🞭", "🞮",
            ];
            let bool_formatter: inquire::formatter::BoolFormatter = &|boolean| if boolean { confirm_signs.as_slice().join("/") } else { cancel_signs.as_slice().join("/") };
            let bool_parser: inquire::parser::BoolParser = &|string| {
                if confirm_signs.as_slice().contains(&string) {
                    Ok(true)
                } else if cancel_signs.as_slice().contains(&string) {
                    Ok(false)
                } else {
                    Err(())
                }
            };
            // I want to draw your attention to the fact that this does in fact do
            // nothing at all, like so many checkboxes of this type
            inquire::Confirm::new("Remember password?")
                .with_formatter(bool_formatter)
                .with_parser(bool_parser)
                .with_error_message(
                    format!(
                        "Type one of {} to accept and one of {} to decline",
                        confirm_signs.as_slice().join("/"),
                        cancel_signs.as_slice().join("/")
                    )
                    .as_str(),
                )
                .prompt()
                .expect("Confirm or cancel");

            let parsed_hash = PasswordHash::new(&account.password_hash).expect("Oh no");
            if !(name == account.name && argon2.verify_password(password.as_bytes(), &parsed_hash).is_ok()) {
                report_error("Either your name or password were wrong. Try again!");
            }

            let auth_code = inquire::Text::new("Enter your Google Authenticator code:")
                .with_validator(inquire::required!())
                .with_validator(inquire::length!(6))
                .prompt()
                .expect("Enter your auth code");

            let ga = google_authenticator::GoogleAuthenticator::new();

            if !ga.verify_code(account.google_auth_secret.as_str(), auth_code.as_str(), 0, 0) {
                report_error("Your auth code was wrong!");
            }

            println!("{}! Logged into your account!", "SUCCESS".green().bold());
        }
        _ => unreachable!(),
    }

    println!(
        "{}",
        "Follow us on Instagram and Twitter @badlang_dev and be sure to also give us a Github star ⭐️⭐️⭐️⭐️⭐️"
            .bold()
            .green()
            .on_magenta()
    );

    if SystemTime::now().duration_since(UNIX_EPOCH).expect("Damn bro what kinda system you running").as_secs() - save_data.last_update > (24 * 60 * 60) {
        // These are for testing because this part of the code likes to break.
        // if false {
        // if true {
        let update_size = fastrand::u64((UPDATE_SIZE - UPDATE_VARIATION)..(UPDATE_SIZE + UPDATE_VARIATION));
        let (download_time, content_length) = {
            let start = Instant::now();
            let mut content_length = u64::MAX;
            for test_file in TEST_FILE {
                content_length = match reqwest::blocking::get(test_file.0) {
                    Ok(ok) => {
                        if ok.status().is_server_error() {
                            continue;
                        }
                        let temp = ok.content_length().unwrap_or(test_file.1);
                        let _ = std::hint::black_box(ok.text().unwrap_or_else(|_| String::new()));
                        // let _ = std::fs::write("/dev/null", ok.text().unwrap_or_else(|_|
                        // String::new())); // This write is stupid because it shouldn't need to be
                        // here. It just won't measure any download speed
                        // otherwise and since the file is "empty" it
                        // doesn't actually print anything. But I'm leaving it here because I feel
                        // it fits with the feel of the program
                        temp
                    }
                    Err(_err) => {
                        continue;
                    }
                };
                break;
            }
            (start.elapsed(), content_length)
        };
        if content_length == u64::MAX {
            report_error("There is an actual server error. For real this time");
        }
        let download_speed = content_length as f64 / download_time.as_secs_f64();
        let mut progress = 0;
        let progress_bar = indicatif::ProgressBar::new(update_size);
        let mut iteration: usize = 0;
        while progress < update_size {
            let increment = fastrand::u64(
                (((1.0 - DOWNLOAD_SPEED_VARIATION) * download_speed / DOWNLOAD_UPDATE_INTERVAL) as u64)..(((1.0 + DOWNLOAD_SPEED_VARIATION) * download_speed / DOWNLOAD_UPDATE_INTERVAL) as u64),
            );
            progress_bar.set_style(
                indicatif::ProgressStyle::with_template(
                    format!(
                        "Downloading update{:<4} {{wide_bar}} {{bytes}}/{{total_bytes}} [{}/s]",
                        ".".repeat((iteration % 3) + 1),
                        humansize::format_size(increment, humansize::BINARY)
                    )
                    .as_str(),
                )
                .expect("This shouldn't fail"),
            );
            iteration += 1;
            progress_bar.inc(increment);
            progress += increment;
            std::thread::sleep(std::time::Duration::from_secs_f64(DOWNLOAD_UPDATE_INTERVAL));
        }
        progress_bar.finish();
        println!("Applying update...");
        std::thread::sleep(std::time::Duration::from_secs(10)); // It's literally just constant.
        save_data.last_update = SystemTime::now().duration_since(UNIX_EPOCH).expect("Damn bro what kinda system you running").as_secs();
    }

    let exec_name = std::env::args().next().expect("How did you manage this one again?");
    save_data.runs_so_far += 1;
    if save_data.runs_so_far < FREE_RUNS {
        report_warning(
            format!(
                "You have used {} out of your {FREE_RUNS} free runs. Afterwards, the program will run on our bronze tier server infrastructure, unless you subscribe to either our gold or platinum \
                 subscription tier. You can open the subscription plans using `{exec_name} -s` or `{exec_name} --subscribe`",
                save_data.runs_so_far
            )
            .as_str(),
        );
    } else {
        report_warning(
            format!(
                "You have used up all your free runs and your program will now run on our bronze tier server infrastructure. Subscribe to our gold or platinum tier to run your programs on your \
                 device or on our gold tier server infrastructure. You can open the subscription plans using `{exec_name} -s` or `{exec_name} --subscribe`"
            )
            .as_str(),
        );
    }
}

// The silly part is over. Thank god

fn main() {
    // Using this removes lots of fluff from panic messages
    std::panic::set_hook(Box::new(|panic_bundle| {
        eprintln!("{}", panic_bundle.payload().downcast_ref::<String>().map_or_else(|| panic_bundle.to_string(), std::clone::Clone::clone));
    }));

    let matches = clap::command!()
        .arg(clap::arg!(<file> "The file to run").required(false).value_parser(clap::value_parser!(PathBuf))) // Yes it is required but it kinda isn't because subscribe.
        .arg(
            clap::arg!(-s --subscribe "Open the subscription plans in a browser")
                .required(false)
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(clap::arg!(--notroll).hide(true).required(false).value_parser(clap::value_parser!(bool))) // This argument isn't really needed and potentially defeats the purpose of the program but it is here so I can keep my sanity.
        .get_matches();

    if let Some(subscribe) = matches.get_one::<bool>("subscribe")
        && *subscribe
    {
        let _ = open::that("https://www.youtube.com/watch?v=dQw4w9WgXcQ");
        report_error("I can't currently open a subscription page without doing some tax-evasion in-case somebody actually donates. Maybe later :/");
    }

    let out_of_free_runs = if let Some(no_troll) = matches.get_one::<bool>("notroll")
        && !(*no_troll)
    {
        let mut savefile_path = home::home_dir().expect("Couldn't locate your home directory, aborting");
        savefile_path.push(".config");
        savefile_path.push("badlang");
        savefile_path.push("badlang.bin");

        let mut save_data = match load_file::<SaveData, &PathBuf>(&savefile_path, 0) {
            Ok(sd) => {
                if sd
                    .account
                    .as_ref()
                    .is_some_and(|acc| acc.version == semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("WTF cargo").to_string())
                {
                    sd
                } else {
                    report_warning("Because the version your account was created on doesn't match your current version, your account was invalidated. Create a new one.");
                    SaveData::default()
                }
            }
            Err(_) => SaveData::default(),
        };

        sillyness(&mut save_data);

        if let Some(parent_dir) = savefile_path.parent() {
            if let Err(err) = std::fs::DirBuilder::new().recursive(true).create(parent_dir) {
                report_error(format!("Couldn't create savefile because {err}").as_str());
            }
        }

        save_file(savefile_path, 0, &save_data).expect("Couldn't save damn");

        save_data.runs_so_far >= FREE_RUNS
    } else {
        false
    };

    let tokens = matches.get_one::<PathBuf>("file").map_or_else(|| report_error("Please provide a path to run!"), parse_file);

    execute_tokens(&tokens, out_of_free_runs);
}

// Because I hate code splitting (not really, it just doesn't fit the feel
// of the project), tests also go in this file
mod tests {
    #[test]
    fn test_math() {
        use crate::Token as T;
        let tokens = vec![
            T::Number(10),
            T::Number(5),
            T::Add,
            T::Number(10),
            T::Number(5),
            T::Subtract,
            T::Number(10),
            T::Number(5),
            T::Multiply,
            T::Number(10),
            T::Number(5),
            T::Divide,
        ];
        let stack = crate::execute_tokens(&tokens, false);
        assert!(stack.get(0).is_some_and(|v| *v == 15));
        assert!(stack.get(1).is_some_and(|v| *v == 5));
        assert!(stack.get(2).is_some_and(|v| *v == 50));
        assert!(stack.get(3).is_some_and(|v| *v == 2));
    }

    #[test]
    fn test_stack_manip() {
        use crate::Token as T;
        let tokens = vec![
            T::Number(1),
            T::Number(2),
            T::Number(3), // 1 2 3
            T::Swap,      // 1 3 2
            T::Over,      // 1 3 2 3
            T::Dup,       // 1 3 2 3 3
            T::Rot,       // 1 3 3 3 2
        ];
        let stack = crate::execute_tokens(&tokens, false);
        assert!(stack.get(0).is_some_and(|v| *v == 1));
        assert!(stack.get(1).is_some_and(|v| *v == 3));
        assert!(stack.get(2).is_some_and(|v| *v == 3));
        assert!(stack.get(3).is_some_and(|v| *v == 3));
        assert!(stack.get(4).is_some_and(|v| *v == 2));
    }
}
