use anyhow::{anyhow, Result};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

use binrw::io::BufReader;
use binrw::{binread, BinRead, BinResult, Error, VecArgs};
use std::collections::HashMap;
use std::convert::Infallible;
use std::fs::File;
use std::io::{Cursor, SeekFrom};
use std::path::Path;
use std::vec;
use struct_iterable::Iterable;

lazy_static! {
    static ref ENDIANNESS: HashMap<u8, &'static str> =
        HashMap::from([(0, "bigEndian"), (1, "littleEndian")]);
    static ref DATA_TYPE: HashMap<u8, &'static str> = HashMap::from([
        (0, "64Bit Float"),
        (1, "32Bit Float"),
        (2, "Reserved"),
        (3, "Reserved")
    ]);
    static ref INSTRUMENT_TABLE: HashMap<i8, &'static str> = {
        HashMap::from([
            (0, "NONE"),
            (1, "GSX"),
            (2, "ALPHA"),
            (3, "ECLIPSE"),
            (4, "MASS_SPEC"),
            (5, "COMPILER"),
            (6, "OTHER_NMR"),
            (7, "UNKNOWN"),
            (8, "GEMINI"),
            (9, "UNITY"),
            (10, "ASPECT"),
            (11, "UX"),
            (12, "FELIX"),
            (13, "LAMBDA"),
            (14, "GE_1280"),
            (15, "GE_OMEGA"),
            (16, "CHEMAGNETICS"),
            (17, "CDFF"),
            (18, "GALACTIC"),
            (19, "TRIAD"),
            (20, "GENERIC_NMR"),
            (21, "GAMMA"),
            (22, "JCAMP_DX"),
            (23, "AMX"),
            (24, "DMX"),
            (25, "ECA"),
            (26, "ALICE"),
            (27, "NMR_PIPE"),
            (28, "SIMPSON"),
        ])
    };
    static ref DATA_FORMAT: HashMap<u8, &'static str> = {
        HashMap::from([
            (1, "One_D"),
            (2, "Two_D"),
            (3, "Three_D"),
            (4, "Four_D"),
            (5, "Five_D"),
            (6, "Six_D"),
            (7, "Seven_D"),
            (8, "Eight_D"),
            (9, "not for NMR data formats"),
            (10, "not for NMR data formats"),
            (11, "not for NMR data formats"),
            (12, "Small_Two_D"),
            (13, "Small_Three_D"),
            (14, "Small_Four_D"),
        ])
    };
    static ref DATA_AXIS_TYPE: HashMap<u8, &'static str> = {
        HashMap::from([
            (0, "None"), // Axis not used
            (1, "Real"), // Axis has real data only, no imaginary.
            (2, "TPPI"),
            (3, "Complex"),
            (4, "Real_Complex"),
            /* Axis should be accessed as complex when it is the major axis,
            accessed as real otherwise.  This is only valid when all axes in
            use have this setting.*/
            (5, "Envelope"),
            /* Behaves the same way as a Real_Complex dimension but the data
            has different meaning.  Instead of being treated as real and
            imaginary parts of a complex number, the data should be treated as minimum and maximum parts of a projection.  This is used
            for the data that results from an envelope projection.*/
        ])
    };
    static ref PREFIX: HashMap<i8, &'static str> = HashMap::from([
        (-8, "Yotta"),
        (-6, "Exa"),
        (-7, "Zetta"),
        (-5, "Pecta"),
        (-4, "Tera"),
        (-3, "Giga"),
        (-2, "Mega"),
        (-1, "Kilo"),
        (0, "None"),
        (1, "Milli"),
        (2, "Micro"),
        (3, "Nano"),
        (4, "Pico"),
        (5, "Femto"),
        (6, "Atto"),
        (7, "Zepto"),
    ]);
    static ref UNIT_PREFIX: HashMap<&'static str, i8> = HashMap::from([
        ("Yotta", 24),
        ("Exa", 21),
        ("Zetta", 18),
        ("Pecta", 15),
        ("Tera", 12),
        ("Giga", 9),
        ("Mega", 6),
        ("Kilo", 3),
        ("None",0 ),
        ("Milli", -3),
        ("Micro", -6),
        ("Nano", -9),
        ("Pico", -12),
        ("Femto", -15),
        ("Atto", -18),
        ("Zepto", -21),
    ]);
    static ref BASE: HashMap<u8, &'static str> = HashMap::from([
        (0,"None"),
        (1,"Abundance"),
        (2,"Ampere"),
        (3,"Candela"),
        (4,"Celsius"),
        (5,"Coulomb"),
        (6,"Degree"),
        (7,"Electronvolt"),
        (8,"Farad"),
        (9,"Sievert"),
        (10,"Gram"),
        (11,"Gray"),
        (12,"Henry"),
        (13,"Hertz"),
        (14,"Kelvin"),
        (15,"Joule"),
        (16,"Liter"),
        (17,"Lumen"),
        (18,"Lux"),
        (19,"Meter"),
        (20,"Mole"),
        (21,"Newton"),
        (22,"Ohm"),
        (23,"Pascal"),
        (24,"Percent"),
        (25,"Point"),
        (26,"Ppm"),
        (27,"Radian"),
        (28,"Second"),
        (29,"Siemens"),
        (30,"Steradian"),
        (31,"Tesla"),
        (32,"Volt"),
        (33,"Watt"),
        (34,"Weber"),
        (35,"Decibel"),
        (36,"Dalton"),
        (37,"Thompson"),
        (38,"Ugeneric"), // Treated as None, but never displayed',
        (39,"LPercent "), // Treated as percent for display, but different for comparison',
        (40,"PPT"), // Parts per trillion (Private, do not use)',
        (41,"PPB "), // Parts per billion (Private, do not use)',
        (42,"Index"),
    ]);
    static ref DATA_AXIS_RANGED: HashMap<u8, &'static str> = {
        HashMap::from([
            (0, "Ranged"),
            /* The ruler for the axis ranges from Data_Axis_Start[n] to
                  Data_Axis_Stop[n] with a step function of
                      (Data_Axis_Stop[n] - Data_Axis_Start[n]) /
                      (Data_Offset_Stop[n] - Data_Offset_Start[n]) */
            (1, "Listed"), // (deprecated)
            /* The ruler for the axis is a list of doubles stored in the
                  List Section.  Values in the ruler may be anything.*/
            (2, "Sparse"),
            /*The ruler for the axis is a list of doubles stored in the
                  List Section.  Values in the rulers must be strictly monotonically
                  increasing or decreasing.*/
            (3, "Listed"),
            /* The ruler for the axis is a list of doubles stored in the
                  List Section.  Values in the rulers do not fit definition of Sparse.*/
        ])
    };
    static ref VALUE_TYPE: HashMap<u8, &'static str> = {
        HashMap::from([
            (0, "String"),
            (1, "Integer"),
            (2, "Float"),
            (3, "Complex"),
            (4, "Infinity"),
        ])
    };

}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum ParamValue {
    String(String),
    Integer(i32),
    Float(f64),
    Complex(ComplexNumber),
    Infinity(i32),
}
impl ParamValue {
    fn value(&self) -> f64 {
        match *self {
            ParamValue::Integer(v) => v.into(),
            ParamValue::Float(v) => v,
            _ => 0.0,
        }
    }
}

fn read_string(input: Vec<u8>, to_lower: bool) -> String {
    let res = String::from_utf8(input)
        .unwrap()
        .trim_end_matches(char::from(0))
        .trim_end_matches(" ")
        .replace(" ", "")
        .to_string();

    if to_lower {
        res.to_lowercase().to_string()
    } else {
        res
    }
}

#[binrw::parser(reader, endian)]
fn read_param_value(value_type: &str) -> BinResult<ParamValue> {
    let res = match value_type {
        "String" => ParamValue::String(read_string(
            Vec::<u8>::read_options(
                reader,
                endian,
                VecArgs {
                    count: 16,
                    inner: (),
                },
            )?,
            false,
        )),

        "Integer" => ParamValue::Integer(i32::read_options(reader, endian, ())?),
        "Float" => ParamValue::Float(f64::read_options(reader, endian, ())?),
        "Complex" => ParamValue::Complex(ComplexNumber::read_options(reader, endian, ())?),
        "Infinity" => ParamValue::Integer(i32::read_options(reader, endian, ())?),
        &_ => ParamValue::String("".to_string()),
    };

    Ok(res)
}

#[binrw::parser(reader, endian)]
fn read_vector(data_type: &str, size: &u32) -> BinResult<Vec<f64>> {
    if data_type == "32Bit Float" {
        Ok(Vec::<f32>::read_options(
            reader,
            endian,
            VecArgs {
                count: *size as usize,
                inner: (),
            },
        )?
        .into_iter()
        .map(|x| x.into())
        .collect())
    } else if data_type == "64Bit Float" {
        Ok(Vec::<f64>::read_options(
            reader,
            endian,
            VecArgs {
                count: *size as usize,
                inner: (),
            },
        )?)
    } else {
        return Err(Error::AssertFail {
            pos: reader.stream_position()?,
            message: format!(
                "Data type {} is not supported, only 32Bit Float and 64Bit Float are supported.",
                data_type
            ),
        });
    }
}

#[binrw::parser(reader, endian)]
fn parse_data<'a>(
    data_count: &'a HashMap<&'a str, u32>,
    data_format: &str,
    data_type: &str,
    data_points: &Vec<u32>,
) -> BinResult<HashMap<&'a str, Vec<Vec<f64>>>> {
    let mut result = HashMap::new();
    let mut re = Vec::<Vec<f64>>::new();
    let mut im = Vec::<Vec<f64>>::new();
    let mut re_im = Vec::<Vec<f64>>::new();
    let mut im_re = Vec::<Vec<f64>>::new();

    let data_section_count = *data_count.get("data_section_count").unwrap();

    if data_format == "One_D" {
        for s in 0..data_section_count {
            let section = read_vector(reader, endian, (data_type, data_points.get(0).unwrap()))?;

            if s == 0 {
                im.extend(vec![section])
            } else if s == 1 {
                re.extend(vec![section])
            } else {
            }
        }
    } else if data_format == "Two_D" {
        let me = 32;
        let dim1 = data_points.get(0).unwrap();
        let dim2 = data_points.get(1).unwrap();

        let num_sections = dim2 / me;
        let num_blocks = dim1 / me;

        for s in 0..data_section_count {
            let mut section = Vec::<Vec<f64>>::new();
            for i in 0..num_sections {
                let mut row: Vec<Vec<f64>> = (0..me).map(|_| Vec::<f64>::new()).collect();
                for _b in 0..num_blocks {
                    for k in 0..me {
                        let vec = row.get_mut(k as usize).unwrap();
                        vec.extend(read_vector(reader, endian, (data_type, &me))?);
                    }
                }
                if i == 0 {
                    section = row;
                } else {
                    section.extend(row);
                }
            }
            match data_section_count {
                2 => match s {
                    0 => {
                        re.extend(section);
                    }
                    1 => {
                        im.extend(section);
                    }
                    _ => (),
                },
                4 => match s {
                    0 => {
                        re.extend(section);
                    }
                    1 => {
                        re_im.extend(section);
                    }
                    2 => {
                        im.extend(section);
                    }
                    3 => {
                        im_re.extend(section);
                    }
                    _ => (),
                },
                _ => {
                    return Err(Error::AssertFail {
                        pos: reader.stream_position()?,
                        message: format!(
                            "Data section count with value {} is not supported for Two_D kind, only 2 and 4 are supported.",
                            data_format
                        ),
                    });
                }
            }
        }
    } else {
        return Err(Error::AssertFail {
            pos: reader.stream_position()?,
            message: format!(
                "Data format {} is not supported, only One_D and Two_D are supported.",
                data_format
            ),
        });
    }
    result.insert("im", im);
    result.insert("re", re);
    result.insert("im_re", im_re);
    result.insert("re_im", re_im);
    Ok(result)
}

fn get_par_value(params: &Parameters, field_name: &str) -> ParamValue {
    if let Some(param) = get_par(params, field_name) {
        param.value.clone()
    } else {
        ParamValue::String("".to_string())
    }
}

fn get_par<'a>(params: &'a Parameters, field_name: &str) -> Option<&'a Param> {
    if let Some(found) = params.param_array.iter().find(|p| p.name == field_name) {
        Some(found)
    } else {
        None
    }
}

fn get_magnitude(params: &Parameters, field_name: &str) -> UnitMagnitude {
    if let Some(param) = get_par(params, field_name) {
        let val = param.value.value()
            * 10.0_f64.powf(
                *UNIT_PREFIX
                    .get(&(*param.unit.get(0).unwrap().prefix))
                    .unwrap() as f64,
            );

        UnitMagnitude {
            magnitude: val,
            unit: param.unit.get(0).unwrap().base.clone(),
        }
    } else {
        UnitMagnitude {
            magnitude: 0.0,
            unit: "NA".to_string(),
        }
    }
}

fn get_param_string(params: &Parameters) -> String {
    iterable_to_str_representation(
        params
            .param_array
            .iter()
            .map(|item| item.name.to_string())
            .collect::<Vec<String>>(),
    )
}

fn iterable_to_str_representation<I, D>(iterable: I) -> String
where
    I: IntoIterator<Item = D>,
    D: std::fmt::Display,
{
    let mut iterator = iterable.into_iter();

    let head = match iterator.next() {
        None => return String::from("[]"),
        Some(x) => format!("[\"{}", x),
    };
    let body = iterator.fold(head, |a, v| format!("{}\",\"{}", a, v));
    format!("{}\"]", body)
}

fn get_digital_filter(params: &Parameters) -> Option<f64> {
    let orders = params.param_array.iter().find(|&x| x.name == "orders");
    let factors = params.param_array.iter().find(|&x| x.name == "factors");
    let sweep = params.param_array.iter().find(|&x| x.name == "x_sweep");
    let acq_time = params.param_array.iter().find(|&x| x.name == "x_acq_time");
    let n_points = params.param_array.iter().find(|&x| x.name == "x_points");

    if vec![orders, factors, sweep, acq_time, n_points]
        .iter()
        .any(|&x| x.is_none())
        == true
    {
        return None;
    }

    let orders_value = match &orders.unwrap().value {
        ParamValue::String(v) => v,
        _ => return None,
    };
    let factors_value = match &factors.unwrap().value {
        ParamValue::String(v) => v,
        _ => return None,
    };
    let sweep_value = match &sweep.unwrap().value {
        ParamValue::Float(v) => v,
        _ => return None,
    };
    let acq_time_value = match &acq_time.unwrap().value {
        ParamValue::Float(v) => v,
        _ => return None,
    };
    let n_points_value = match &n_points.unwrap().value {
        ParamValue::Integer(v) => v,
        _ => return None,
    };

    let s = orders_value[..1].parse::<i32>().unwrap();
    let jump = orders_value[1..].len() / s as usize;

    let mut arg: f64 = 0.0;
    let mut factor_number = vec![0, s];

    let mut offset_o = 1;
    let mut offset_f = 0;

    for i in 0..s {
        println!("{}, {}, {}", factors_value, offset_f, factors_value[offset_f..(offset_f + 1)].to_string());
        factor_number[i as usize] = factors_value[offset_f..(offset_f + 1)]
            .parse::<i32>()
            .unwrap();
        offset_f += 1;
    }

    for i in 0..s {
        let mut productorial = 1;
        for j in i..s {
            productorial *= factor_number[j as usize];
        }
        arg += (orders_value[offset_o..(offset_o + jump)]
            .parse::<i32>()
            .unwrap()
            - 1) as f64
            / productorial as f64;
        offset_o += jump;
    }
    arg /= 2.0;

    let delay_sec = arg / sweep_value;
    return Some((delay_sec / acq_time_value) * (n_points_value - 1) as f64);
}

fn extract_info(headers: &Headers, params: &Parameters, data: &Data) -> Info {
    let mut nucleus = Vec::<ParamValue>::new();
    let mut acquisition_time = Vec::<UnitMagnitude>::new();
    let mut spectral_width = Vec::<UnitMagnitude>::new();
    let mut spectral_width_clipped = Vec::<UnitMagnitude>::new();
    let mut resolution = Vec::<UnitMagnitude>::new();
    let mut origin_frequency = Vec::<UnitMagnitude>::new();
    let mut frequency_offset = Vec::<UnitMagnitude>::new();
    let mut data_units = Vec::<String>::new();

    if (headers.data_format.as_str() == "One_D") | (headers.data_format.as_str() == "Two_D") {
        nucleus.push(get_par_value(params, "x_domain"));
        acquisition_time.push(get_magnitude(params, "x_acq_time"));
        spectral_width.push(get_magnitude(params, "x_sweep"));
        spectral_width_clipped.push(get_magnitude(params, "x_sweep_clipped"));
        resolution.push(get_magnitude(params, "x_resolution"));
        origin_frequency.push(get_magnitude(params, "x_freq"));
        frequency_offset.push(get_magnitude(params, "x_offset"));
        data_units.push(headers.data_units.get(0).unwrap().base.to_string());
    };
    if headers.data_format.as_str() == "Two_D" {
        nucleus.push(get_par_value(params, "y_domain"));
        acquisition_time.push(get_magnitude(params, "y_acq_time"));
        spectral_width.push(get_magnitude(params, "y_sweep"));
        resolution.push(get_magnitude(params, "y_resolution"));
        origin_frequency.push(get_magnitude(params, "y_freq"));
        frequency_offset.push(get_magnitude(params, "y_offset"));
        data_units.push(headers.data_units.get(1).unwrap().base.to_string());
    };

    Info {
        sample_name: get_par_value(params, "sample_id"),
        creation_time: headers.creation_time,
        revision_time: headers.creation_time,
        author: headers.author.clone(),
        comment: headers.comment.clone(),
        solvent: get_par_value(params, "solvent"),
        temperature: get_magnitude(params, "temp_get"),
        probe_name: get_par_value(params, "probe_id"),
        field_strength: get_magnitude(params, "field_strength"),
        experiment: get_par_value(params, "experiment"),
        dimension: headers.data_dimension_number,
        nucleus: nucleus,
        pulse_strength90: get_magnitude(params, "x90"),
        number_of_scans: get_par_value(params, "scans"),
        relaxation_time: get_magnitude(params, "relaxation_delay"),
        data_points: headers
            .data_points
            .get(0..headers.data_dimension_number as usize)
            .unwrap()
            .to_vec(),
        data_offset_start: headers.data_offset_start.clone(),
        data_offset_stop: headers.data_offset_stop.clone(),
        data_units: data_units,
        data_sections: data.iter().map(|(k, _)| k.to_string()).collect(),
        origin_frequency: origin_frequency,
        frequency_offset: frequency_offset,
        acquisition_time: acquisition_time,
        spectral_width: spectral_width,
        spectral_width_clipped: spectral_width_clipped,
        data_axis_start: headers.data_axis_start.clone(),
        data_axis_stop: headers.data_axis_stop.clone(),
        resolution: resolution,
        decimation_rate: get_par_value(params, "decimation_rate"),
        param_list: get_param_string(params),
        // digital_filter: None,
        digital_filter: get_digital_filter(params),
    }
}

fn get_data_section_count(input: &Vec<String>) -> HashMap<&str, u32> {
    let mut data_section_count = 1;
    let mut real_complex = 0;
    for v in input {
        if (v == "Real_Complex") & (real_complex == 0) {
            data_section_count += 1;
            real_complex += 1
        } else if v == "Complex" {
            data_section_count *= 2;
        }
    }
    let mut result = HashMap::new();
    result.insert("data_section_count", data_section_count);
    result.insert("real_complex", real_complex);

    result
}

fn read_bool_vector_from_byte(input: u8) -> Vec<bool> {
    (0..8)
        .rev()
        .map(|n: usize| ((input >> n) & 1) != 0)
        .collect()
}

fn read_bool_vector(input: Vec<u8>) -> Vec<bool> {
    input.iter().map(|&v| v != 0).collect()
}

fn read_data_axis_ranged(input: Vec<u8>) -> Vec<String> {
    let mut result: Vec<String> = Vec::new();
    for value in input {
        let v1 = value >> 4;
        let v2 = value & 0b00001111;

        result.append(&mut vec![
            DATA_AXIS_RANGED.get(&v1).unwrap().to_string(),
            DATA_AXIS_RANGED.get(&v2).unwrap().to_string(),
        ]);
    }
    result
}

fn read_data_axis_names(input: Vec<u8>) -> Vec<String> {
    let mut result: Vec<String> = Vec::new();
    for value in (0..(8 * 32)).step_by(32) {
        result.append(&mut vec![read_string(
            input[value..value + 32].to_vec(),
            false,
        )])
    }
    result
}

#[derive(BinRead, Debug, Serialize, Deserialize, Copy, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ComplexNumber {
    pub real: f64,
    pub imag: f64,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnitMagnitude {
    pub magnitude: f64,
    pub unit: String,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Unit {
    #[br(temp)]
    temp_byte: i8,

    #[br(calc = PREFIX.get(&((temp_byte >> 4) as i8)).unwrap().to_string())]
    pub prefix: String,
    #[br(calc = temp_byte as u8 & 0b00001111)]
    pub power: u8,
    #[br(map = |x: i8| BASE.get(&(x as u8)).unwrap().to_string())]
    pub base: String,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[br(big)]
pub struct CompoundUnit {
    pub scaler: i16,
    #[br(count = 5)]
    pub unit: Vec<i16>,
}

#[binread]
#[derive(Debug, Serialize, Deserialize, Copy, Clone)]
#[serde(rename_all = "camelCase")]
#[br(big)]
pub struct Date {
    #[br(count = 4, temp)]
    temp_bytes: Vec<u8>,

    #[br(calc = 1990 + (temp_bytes[0] >> 1) as u16)]
    pub year: u16,
    #[br(calc = ((temp_bytes[0] << 3) & 0b00001000) + (temp_bytes[1] >> 5))]
    pub month: u8,
    #[br(calc = temp_bytes[2] & 0b00011111)]
    pub day: u8,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Param {
    #[br(pad_before = 4)]
    pub scaler: i16,
    #[br(count = 5)]
    pub unit: Vec<Unit>,

    #[br(pad_before = 16, map = |x: i32| VALUE_TYPE.get(&(x as u8)).unwrap().to_string())]
    value_type: String,

    #[br(seek_before = SeekFrom::Current(-20), pad_size_to = 20, parse_with=read_param_value, args(&value_type))]
    value: ParamValue,

    #[br(count=28, map=|x: Vec<u8>| read_string(x, true))]
    name: String,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[br(big)]
pub struct Headers {
    #[br(count = 8,map =|x: Vec<u8>| read_string(x, false))]
    pub file_identifier: String,

    #[br(map = |x: u8| ENDIANNESS.get(&x).unwrap().to_string())]
    pub endian: String,

    pub major_version: u8,
    pub minor_version: u16,

    pub data_dimension_number: u8,

    #[br(map = read_bool_vector_from_byte)]
    pub data_dimension_exist: Vec<bool>,

    #[br(temp)]
    temp_byte: u8,

    #[br(calc = DATA_TYPE.get(&(temp_byte >> 6)).unwrap().to_string())]
    pub data_type: String,

    #[br(calc = DATA_FORMAT.get(&(temp_byte & 0b00111111)).unwrap().to_string(), assert((data_format == "One_D") | (data_format == "Two_D"), "Only One_D and Two_D data formats are implemented yet, received {} instead ", data_format))]
    pub data_format: String,

    #[br(map = |x: i8| INSTRUMENT_TABLE.get(&x).unwrap().to_string())]
    pub data_instrument: String,

    #[br(count = 8)]
    pub translate: Vec<u8>,

    #[br(count=8, map=|x: Vec<u8>| x.iter().map(|value| DATA_AXIS_TYPE.get(value).unwrap().to_string()).collect())]
    pub data_axis_type: Vec<String>,

    #[br(count = 8)]
    pub data_units: Vec<Unit>,

    #[br(count = 124,map = |x: Vec<u8>| read_string(x, false))]
    pub title: String,

    #[br(count = 4, map =read_data_axis_ranged)]
    pub data_axis_ranged: Vec<String>,

    #[br(count = 8)]
    pub data_points: Vec<u32>,
    #[br(count = 8)]
    pub data_offset_start: Vec<u32>,
    #[br(count = 8)]
    pub data_offset_stop: Vec<u32>,
    #[br(count = 8)]
    pub data_axis_start: Vec<f64>,
    #[br(count = 8)]
    pub data_axis_stop: Vec<f64>,

    pub creation_time: Date,
    pub revision_time: Date,

    #[br(count = 16, map = |x: Vec<u8>| read_string(x, false))]
    pub node_name: String,
    #[br(count = 128, map = |x: Vec<u8>| read_string(x, false))]
    pub site: String,
    #[br(count = 128, map = |x: Vec<u8>| read_string(x, false))]
    pub author: String,
    #[br(count = 128, map = |x: Vec<u8>| read_string(x, false))]
    pub comment: String,

    #[br(count = 32 * 8, map =read_data_axis_names)]
    pub data_axis_titles: Vec<String>,

    #[br(count = 8)]
    pub base_freq: Vec<f64>,
    #[br(count = 8)]
    pub zero_point: Vec<f64>,
    #[br(count = 8, map = read_bool_vector, pad_after = 3)]
    pub reversed: Vec<bool>,
    #[br(map = |x: u8| (x >> 7) != 0 )]
    pub annotation_ok: bool,
    pub history_used: u32,
    pub history_length: u32,
    pub param_start: u32,
    pub param_length: u32,

    #[br(count = 8)]
    pub list_start: Vec<u32>,
    #[br(count = 8)]
    pub list_length: Vec<u32>,

    pub data_start: u32,

    // TODO: check if correct
    #[br(count = 2, map = |values: Vec<u32>| (0 | values[1]))]
    pub data_length: u32,
    // TODO: check if correct
    #[br(count = 2, map = |values: Vec<u32>| (0 | values[1]))]
    pub context_start: u32,
    pub context_length: u32,
    // TODO: check if correct
    #[br(count = 2, map = |values: Vec<u32>| (0 | values[1]))]
    pub annote_start: u32,
    pub annote_length: u32,
    // TODO: check if correct
    #[br(count = 2, map = |values: Vec<u32>| (0 | values[1]))]
    pub total_size: u32,
    #[br(count = 8)]
    pub unit_location: Vec<u8>,

    #[br(count = 2)]
    pub compound_unit: Vec<CompoundUnit>,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Parameters {
    parameters_size: u32,
    low_index: u32,
    high_index: u32,
    total_size: u32,

    #[br(count = high_index)]
    param_array: Vec<Param>,
}

#[derive(Debug, Serialize, Deserialize, Iterable)]
struct ImRe {
    im: DataField,
    re: DataField,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum DataField {
    Two(Vec<Vec<f64>>),
    Four {
        im: Vec<Vec<f64>>,
        re: Vec<Vec<f64>>,
    },
}

fn store_data_result(
    data_results: &HashMap<&str, Vec<Vec<f64>>>,
    data_count: &HashMap<&str, u32>,
    data_type: &str,
) -> DataField {
    let data_section_count = data_count.get("data_section_count").unwrap();
    match data_section_count {
        2 => match data_type {
            "im" => DataField::Two(data_results.get("im").unwrap().clone()),
            "re" => DataField::Two(data_results.get("re").unwrap().clone()),
            _ => unreachable!(),
        },
        4 => match data_type {
            "im" => DataField::Four {
                im: data_results.get("im").unwrap().clone(),
                re: data_results.get("im_re").unwrap().clone(),
            },
            "re" => DataField::Four {
                im: data_results.get("re").unwrap().clone(),
                re: data_results.get("re_im").unwrap().clone(),
            },
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

#[binread]
#[derive(Debug, Serialize, Deserialize, Iterable)]
#[serde(rename_all = "camelCase")]
#[br(import(data_axis_type: Vec<String>, data_type: String, data_format: String, data_points: Vec<u32>))]
pub struct Data {
    #[br(temp, calc = get_data_section_count(&data_axis_type))]
    data_count: HashMap<&str, u32>,
    #[br(parse_with = parse_data, temp, args(&data_count, &data_format, &data_type, &data_points))]
    data_results: HashMap<&str, Vec<Vec<f64>>>,

    #[br(calc=store_data_result(&data_results, &data_count, "im"))]
    im: DataField,
    #[br(calc=store_data_result(&data_results, &data_count, "re"))]
    re: DataField,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Info {
    pub sample_name: ParamValue,
    pub creation_time: Date,
    pub revision_time: Date,
    pub author: String,
    pub comment: String,
    pub solvent: ParamValue,
    pub temperature: UnitMagnitude,
    pub probe_name: ParamValue,
    pub field_strength: UnitMagnitude,
    pub experiment: ParamValue,
    pub dimension: u8,
    pub nucleus: Vec<ParamValue>,
    pub pulse_strength90: UnitMagnitude,
    pub number_of_scans: ParamValue,
    pub relaxation_time: UnitMagnitude,
    pub data_points: Vec<u32>,
    pub data_offset_start: Vec<u32>,
    pub data_offset_stop: Vec<u32>,
    pub data_units: Vec<String>,
    pub data_sections: Vec<String>,
    pub origin_frequency: Vec<UnitMagnitude>,
    pub frequency_offset: Vec<UnitMagnitude>,
    pub acquisition_time: Vec<UnitMagnitude>,
    pub spectral_width: Vec<UnitMagnitude>,
    pub spectral_width_clipped: Vec<UnitMagnitude>,
    pub data_axis_start: Vec<f64>,
    pub data_axis_stop: Vec<f64>,
    pub resolution: Vec<UnitMagnitude>,
    pub decimation_rate: ParamValue,
    pub param_list: String,
    pub digital_filter: Option<f64>,
}

#[binread]
#[derive(Debug, Serialize, Deserialize)]
#[br(big)]
#[serde(rename_all = "camelCase")]
pub struct JeolDataFile {
    pub headers: Headers,

    #[br(seek_before = SeekFrom::Start(headers.param_start as u64), is_little = headers.endian == "littleEndian")]
    pub parameters: Parameters,

    #[br(seek_before = SeekFrom::Start(headers.data_start as u64), is_little = headers.endian == "littleEndian", args(headers.data_axis_type.clone(), headers.data_type.clone(), headers.data_format.clone(), headers.data_points.clone()))]
    pub data: Data,

    #[br(calc = extract_info(&headers, &parameters, &data))]
    pub info: Info,
}

pub trait DataContainer {
    type Opened: std::io::Read + std::io::Seek;
    type Error;

    fn load_data(self) -> Result<Self::Opened, Self::Error>;
}

impl DataContainer for Vec<u8> {
    type Opened = Cursor<Self>;
    type Error = Infallible;

    fn load_data(self) -> Result<Cursor<Vec<u8>>, Infallible> {
        let cursor = Cursor::new(self);
        Ok(cursor)
    }
}

impl DataContainer for &Path {
    type Opened = BufReader<File>;
    type Error = std::io::Error;

    fn load_data(self) -> std::io::Result<BufReader<File>> {
        let file = File::open(self)?;
        let buffer = BufReader::new(file);
        Ok(buffer)
    }
}

pub fn parse_from_filepath<T: DataContainer>(container: T) -> Result<JeolDataFile> {
    let mut data = container
        .load_data()
        .map_err(|_| anyhow!(format!("Error reading the data!")))?;
    let result = JeolDataFile::read(&mut data).unwrap();

    Ok(result)
}
