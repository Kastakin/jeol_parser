use std::path::{Path, PathBuf};

use jeol_parser_core::parse_from_filepath;
use pyo3::prelude::*;
use pythonize::pythonize;

#[derive(FromPyObject)]
enum InputTypes {
    #[pyo3(transparent, annotation = "bytes")]
    Bytes(Vec<u8>),
    #[pyo3(transparent, annotation = "str")]
    Path(PathBuf),
}

/// Formats the sum of two numbers as string.
#[pyfunction]
fn parse(py: Python<'_>, input: InputTypes) -> PyResult<Py<PyAny>> {
    let res = match input {
        InputTypes::Bytes(bytes) => parse_from_filepath(bytes).unwrap(),
        InputTypes::Path(path_string) => {
            let path = Path::new(&path_string);
            parse_from_filepath(path).unwrap()
        }
    };
    let obj = pythonize(py, &res)?;
    Ok(obj)
}

/// A Python module implemented in Rust.
#[pymodule]
fn jeol_parser(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    Ok(())
}
