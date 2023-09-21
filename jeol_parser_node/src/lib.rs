use jeol_parser_core::parse_from_filepath;
use neon::{prelude::*, types::buffer::TypedArray};

fn parse(mut cx: FunctionContext) -> JsResult<JsValue> {
    let path = cx.argument::<JsBuffer>(0)?;
    let value = path.as_slice(&mut cx).to_vec();
    let res = parse_from_filepath(value).or_else(|e| cx.throw_error(e.to_string()))?;
    Ok(neon_serde3::to_value(&mut cx, &res).or_else(|e| cx.throw_error(e.to_string()))?)
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("parse", parse)?;
    Ok(())
}
