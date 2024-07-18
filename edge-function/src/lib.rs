use std::time::Duration;

use worker::*;

#[event(fetch)]
async fn fetch(_req: Request, _env: Env, _ctx: Context) -> Result<Response> {
    tokio::time::sleep(Duration::from_millis(200));
    console_error_panic_hook::set_once();
    Response::ok("Hello World!")
}
