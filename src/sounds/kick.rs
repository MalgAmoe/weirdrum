use wasm_bindgen::prelude::*;
use web_sys::AudioContext;
use serde::{Deserialize, Serialize};

pub struct Kick {
  pub freq: f32,
  pub pitch: f32,
  pub wave: web_sys::OscillatorType,
  pub decay: f32,
  pub punch: f32,
  pub volume: f32,
}

impl Default for Kick {
  fn default() -> Kick {
      Kick {
          freq: 40.0,
          pitch: 1.0,
          wave: web_sys::OscillatorType::Sine,
          decay: 0.1,
          punch: -30.0 * 0.0,
          volume: 0.7,
      }
  }
}

impl super::Sound for Kick {
  fn play(&self, ctx: &AudioContext, time_delta: f64, offset: f64) -> Result<(), JsValue> {
      let time = time_delta + offset + 0.05;
      let osc = ctx.create_oscillator()?;
      osc.set_type(self.wave);
      let gain = ctx.create_gain()?;
      let compressor = ctx.create_dynamics_compressor()?;
      compressor.threshold().set_value(-30.0 * self.punch);
      compressor.knee().set_value(1.0);
      compressor.ratio().set_value(5.0);
      compressor.attack().set_value(0.1);
      compressor.release().set_value(0.1);
      osc.connect_with_audio_node(&gain)?;
      gain.connect_with_audio_node(&compressor)?;
      compressor.connect_with_audio_node(&ctx.destination())?;
      osc.frequency()
          .set_value_at_time(self.freq + self.freq * self.pitch, time)?;
      osc.frequency()
          .exponential_ramp_to_value_at_time(self.freq, time + 0.02)?;
      gain.gain().set_value(0.0);
      gain.gain()
          .set_target_at_time(0.25 * self.volume, time, 0.0005)?;
      let decay = (self.decay * 0.5) as f64;
      gain.gain().set_target_at_time(0.0, time + decay, decay)?;
      osc.start()?;
      osc.stop_with_when(time + 4.0)?;
      Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct KickValues {
    pub freq: f32,
    pub pitch: f32,
    pub wave: String,
    pub decay: f32,
    pub punch: f32,
    pub volume: f32,
    pub step_type: String,
}