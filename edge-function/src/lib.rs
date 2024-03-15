#![forbid(unsafe_code)]
#![deny(clippy::pedantic)]
#![deny(clippy::nursery)]
#![forbid(clippy::enum_glob_use)]
#![forbid(clippy::unwrap_used)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::future_not_send)]

use std::time::Duration;

use parser::StackValue;
use serde_json as sj;
use worker::{event, Env, Request, Response, Result};

#[event(fetch)]
pub async fn main(req: Request, _env: Env, _ctx: worker::Context) -> Result<Response> {
    let mut path = req.url()?.path().to_string();
    if path == "/" {
        return Ok(Response::from_html(
            "<html>
                <head>
                </head>
                <body>
                    <h1>Hello World!</h1>
                </body>
             </html>",
        )?
        .with_status(200));
    };

    let mut status = 200;
    let mut error: Option<String> = None;

    path = path.replace('/', " ");
    let words = percent_encoding::percent_decode(path.as_bytes()).decode_utf8_lossy();

    let tokens = match parser::parse_string(words.to_string()) {
        Ok(toks) => toks,
        Err(err) => {
            status = 400;
            error = Some(err.to_string());
            vec![]
        }
    };
    let mut buffer: Vec<u8> = vec![];
    let stack = match parser::execute_tokens(&tokens, &mut buffer, Some(Duration::from_millis(20))) {
        Ok(stk) => stk,
        Err(err) => {
            status = 400;
            error = Some(err.to_string());
            vec![]
        }
    };

    let output = String::from_utf8(buffer).unwrap_or_else(|_| {
        status = 400;
        error = Some("Invalid character".to_string());
        String::new()
    });

    let mut json_map = sj::Map::new();

    json_map.insert(
        "stack".to_string(),
        sj::Value::Array(
            stack
                .iter()
                .map(|v| match v {
                    StackValue::Integer(num) => sj::Value::Number((*num).into()),
                    StackValue::Float(num) => sj::Value::Number(sj::value::Number::from_f64(*num).unwrap_or_else(|| {
                        error = Some("Invalid number (infinity or NaN) on stack".to_string());
                        sj::value::Number::from(0)
                    })),
                    StackValue::String(string) => sj::Value::String(string.to_string()),
                    StackValue::Bool(bool) => sj::Value::Bool(*bool),
                })
                .collect(),
        ),
    );
    json_map.insert("output".to_string(), sj::Value::String(output));
    json_map.insert("error".to_string(), error.as_ref().map_or(sj::Value::Null, |err| sj::Value::String(err.clone())));

    let Ok(json) = sj::to_string(&sj::Value::from(json_map)) else {
        return Response::error("Couldn't stringify", 400);
    };

    Ok(Response::from_json(&json)?.with_status(status))
}
