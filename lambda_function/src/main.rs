#![forbid(unsafe_code)]
#![deny(clippy::pedantic)]
#![deny(clippy::nursery)]
#![forbid(clippy::enum_glob_use)]
#![forbid(clippy::unwrap_used)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::cognitive_complexity)]
// #![allow(clippy::cast_precision_loss)]
// #![allow(clippy::cast_possible_truncation)]
// #![allow(clippy::cast_sign_loss)]
// #![allow(clippy::cast_possible_wrap)]

use std::time::Duration;

use lambda_http::{run, service_fn, tracing, Body, Error, Request, Response};
use parser::StackValue;
use serde_json as sj;

async fn function_handler(event: Request) -> Result<Response<Body>, Error> {
    let Some(mut query) = event.uri().query().map(str::to_string) else {
        return Ok(Response::builder()
            .status(200)
            .header("content-type", "text/html")
            .body(
                "<html>
                    <head>
                    </head>
                    <body>
                        <h1>Hello World!</h1>
                    </body>
                </html>"
                    .into(),
            )
            .map_err(Box::new)?);
    };
    // return Ok(Response::builder().body(query.into()).map_err(Box::new)?);

    let mut status = 200;
    let mut error: Option<String> = None;

    if query.ends_with("=") {
        query = query[..query.len() - 1].to_string();
    }
    query = query.replace('+', " ");
    let words = percent_encoding::percent_decode(query.as_bytes()).decode_utf8_lossy();

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
        return Ok(Response::builder()
            .status(400)
            .header("content-type", "text/plain")
            .body("Couldn't stringify".into())
            .map_err(Box::new)?);
    };

    Ok(Response::builder().status(status).header("content-type", "text/json").body(json.into()).map_err(Box::new)?)
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    tracing::init_default_subscriber();

    run(service_fn(function_handler)).await
}
