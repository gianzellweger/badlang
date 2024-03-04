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
    collections::{HashMap},
//     io::Write,
    path::PathBuf,
//     sync::Mutex,
//     time::{Instant, SystemTime, UNIX_EPOCH},
};

// use argon2::{
//     password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
//     Argon2,
// };
use colored::Colorize;
use geocoding::Reverse;
// use inquire::{validator::Validation, CustomUserError};
use itertools::Itertools;
// use rand::distributions::Distribution;
use savefile::prelude::*;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

#[macro_use]
extern crate savefile_derive;

mod microservices;
mod tutorial;

use microservices as ms;

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
    parse_string(contents)
}

fn parse_string(contents: String) -> Vec<Token> {
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

fn sillyness(save_data: &mut ms::SaveData) {
    // This macos version panics for some reason currently. This code should
    // theoretically work, tauri is just buggy. #[cfg(target_os = "macos")]
    // {
    //     println!(
    //         "Because you're on MacOS, the Video Player sadly cannot run
    // on another thread. You need to quit it to continue!"
    //     );
    //     let mut app = tauri::Builder::default()
    //         .invoke_handler(tauri::generate_handler!(ms::tauri_handler))
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
            .invoke_handler(tauri::generate_handler!(ms::tauri_handler))
            .build(tauri::generate_context!())
            .expect("error while building tauri application")
            .run(|_app_handle, event| {
                if let tauri::RunEvent::ExitRequested { api, .. } = event {
                    api.prevent_exit();
                }
            });
    })
    .detach();

    ms::has_internet();
    ms::server_outage();

    let dialogs_displayed = save_data.dialogs_displayed;
    #[cfg(not(all(target_os = "macos", not(debug_assertions))))] // The dialogs currently segfault on intel macs
    jod_thread::spawn(move || {
        if !dialogs_displayed {
            ms::show_dialogs();
        }
    })
    .detach();

    if !save_data.dialogs_displayed {
        save_data.dialogs_displayed = true;
    }

    ms::mailing_list_notification();
    ms::advertisement();
    ms::cookies();
    ms::login(&mut save_data.account);
    ms::self_promotion();
    ms::update(&mut save_data.last_update);
    ms::trial_message(&mut save_data.runs_so_far);
}

// The silly part is over. Thank god

fn main() {
    // Using this removes lots of fluff from panic messages
    std::panic::set_hook(Box::new(|panic_bundle| {
        eprintln!("{}", panic_bundle.payload().downcast_ref::<String>().map_or_else(|| panic_bundle.to_string(), std::clone::Clone::clone));
    }));

    let matches = clap::command!()
        .subcommands([
            clap::Command::new("run")
                .about("Run a program")
                .arg(clap::arg!(<file> "The file to run").value_parser(clap::value_parser!(PathBuf)))
                .arg(clap::arg!(--notroll).hide(true).required(false).value_parser(clap::value_parser!(bool))), /* This argument isn't really needed and potentially defeats the purpose of the
                                                                                                                 * program but it is here so I can keep my sanity. */
            clap::Command::new("tutorial")
                .about("Unlock new language features")
                .long_about("Unlock handy new features of the language in a playful sandbox where you learn to use them!"),
        ])
        .arg(
            clap::arg!(-s --subscribe "Open the subscription plans in a browser")
                .required(false)
                .value_parser(clap::value_parser!(bool)),
        )
        .get_matches();

    if let Some(subscribe) = matches.get_one::<bool>("subscribe")
        && *subscribe
    {
        let _ = open::that("https://www.youtube.com/watch?v=dQw4w9WgXcQ");
        report_error("I can't currently open a subscription page without doing some tax-evasion in-case somebody actually donates. Maybe later :/");
    }

    match matches.subcommand() {
        Some(("run", run_matches)) => {
            let out_of_free_runs = if let Some(no_troll) = run_matches.get_one::<bool>("notroll")
                && !(*no_troll)
            {
                let mut savefile_path = home::home_dir().expect("Couldn't locate your home directory, aborting");
                savefile_path.push(".config");
                savefile_path.push("badlang");
                savefile_path.push("badlang.bin");

                let mut save_data = match load_file::<ms::SaveData, &PathBuf>(&savefile_path, 0) {
                    Ok(sd) => {
                        if sd
                            .account
                            .as_ref()
                            .is_some_and(|acc| acc.version == semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("WTF cargo").to_string())
                        {
                            sd
                        } else {
                            report_warning("Because the version your account was created on doesn't match your current version, your account was invalidated. Create a new one.");
                            ms::SaveData::default()
                        }
                    }
                    Err(_) => ms::SaveData::default(),
                };

                sillyness(&mut save_data);

                if let Some(parent_dir) = savefile_path.parent() {
                    if let Err(err) = std::fs::DirBuilder::new().recursive(true).create(parent_dir) {
                        report_error(format!("Couldn't create savefile because {err}").as_str());
                    }
                }

                save_file(savefile_path, 0, &save_data).expect("Couldn't save damn");

                save_data.runs_so_far >= ms::FREE_RUNS
            } else {
                false
            };

            let tokens = run_matches.get_one::<PathBuf>("file").map_or_else(|| report_error("Please provide a path to run!"), parse_file);

            execute_tokens(&tokens, out_of_free_runs);
        }

        Some(("tutorial", _)) => tutorial::tutorial(0),
        Some((_, _)) => report_error("Invalid subcommand!"),
        None => report_error("No subcommand"),
    }
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
