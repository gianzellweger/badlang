use badlang_parser as pa;
use worker::*;

#[event(fetch)]
pub async fn main(mut req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    let mut buf: Vec<u8> = vec![];
    match pa::execute_tokens(&pa::parse_string("'Hello World' println".to_string()).unwrap(), &mut buf, Some(std::time::Duration::from_millis(10))).await {
        Ok(_) => Response::ok("Ok"),
        Err(_) => Response::error("oh oh", 403),
    }
}
