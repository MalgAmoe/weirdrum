use web_sys::AudioContext;
use wasm_bindgen::prelude::*;

pub mod kick;
pub mod snare;

pub trait Sound {
  fn play(&self, ctx: &AudioContext, time_delta: f64, offset: f64) -> Result<(), JsValue>;
}

pub fn wave_string_to_osc(wave: &str) -> web_sys::OscillatorType {
  match wave {
      "triangle" => web_sys::OscillatorType::Triangle,
      _ => web_sys::OscillatorType::Sine,
  }
}