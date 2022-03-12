use web_sys::AudioContext;
use wasm_bindgen::prelude::*;

pub mod kick;
pub mod snare;

pub trait Sound {
  fn play(&self, ctx: &AudioContext, params: Option<super::SoundParams>, time_delta: f64, offset: f64) -> Result<(), JsValue>;
  fn update(&mut self, params: SoundParams);
}

#[derive(Copy, Clone)]
pub enum SoundParams {
  Kick(kick::KickParams),
  Snare(snare::SnareParams)
}

pub fn wave_string_to_osc(wave: &str) -> web_sys::OscillatorType {
  match wave {
      "triangle" => web_sys::OscillatorType::Triangle,
      _ => web_sys::OscillatorType::Sine,
  }
}