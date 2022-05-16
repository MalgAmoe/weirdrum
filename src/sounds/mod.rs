use web_sys::AudioContext;
use wasm_bindgen::prelude::*;

pub mod kick;
pub mod snare;
pub mod hat;

pub trait Sound {
  fn play(&self, ctx: &AudioContext, params: Option<super::SoundParams>, time_delta: f64, offset: f64) -> Result<(), JsValue>;
  fn update(&mut self, params: SoundParams);
  fn update_volume(&mut self, ctx: &AudioContext, volume: f32)-> Result<(), JsValue>;
}

#[derive(Copy, Clone)]
pub enum SoundParams {
  Kick(kick::KickParams),
  Snare(snare::SnareParams),
  Hat(hat::HatParams),
}

pub fn wave_string_to_osc(wave: &str) -> web_sys::OscillatorType {
  match wave {
      "triangle" => web_sys::OscillatorType::Triangle,
      _ => web_sys::OscillatorType::Sine,
  }
}
