use rand::Rng;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use web_sys::{AudioBuffer, AudioContext};

pub struct Snare {
  pub nodes: SnareNodes,
  pub params: SnareParams,
}

#[derive(Copy, Clone)]
pub struct SnareParams {
  pub freq: f32,
  pub blend: f32,
  pub decay: f32,
  pub punch: f32,
  pub volume: f32,
}

pub struct SnareNodes {
  noise_buffer: AudioBuffer,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SnareValues {
  pub freq: f32,
  pub blend: f32,
  pub decay: f32,
  pub punch: f32,
  pub volume: f32,
  pub step_type: String,
}

impl Default for SnareParams {
  fn default() -> SnareParams {
    SnareParams {
      freq: 120.0,
      blend: 0.5,
      decay: 0.1,
      punch: 0.5,
      volume: 0.7,
    }
  }
}

impl Snare {
  pub fn new(ctx: &AudioContext) -> Result<Self, JsValue> {
    let sr = ctx.sample_rate();
    let noise_buffer = ctx.create_buffer(1, sr as u32, sr)?;
    let noise_output = &mut noise_buffer.get_channel_data(0)?;
    let mut rng = rand::thread_rng();
    for i in 0..sr as usize {
      noise_output[i] = 2.0 * rng.gen::<f32>() - 1.0;
    }
    noise_buffer.copy_to_channel(noise_output, 0)?;
    Ok(Snare {
      params: SnareParams::default(),
      nodes: SnareNodes { noise_buffer },
    })
  }
}

impl super::Sound for Snare {
  fn update(&mut self, params: super::SoundParams) {
    if let super::SoundParams::Snare(snare_params) = params {
      self.params = snare_params
    }
  }
  fn play(
    &self,
    ctx: &AudioContext,
    snare_params: Option<super::SoundParams>,
    time_delta: f64,
    offset: f64,
  ) -> Result<(), JsValue> {
    let params = match snare_params {
      Some(super::SoundParams::Snare(params)) => params,
      _ => self.params,
    };
    let time = time_delta + offset + 0.05;
    let osc = ctx.create_oscillator()?;

    let white_noise = ctx.create_buffer_source()?;
    white_noise.set_buffer(Some(&self.nodes.noise_buffer));
    white_noise.set_loop(true);

    let noise_gain = ctx.create_gain()?;
    let osc_gain = ctx.create_gain()?;
    noise_gain.gain().set_value(params.blend);
    osc_gain.gain().set_value(1.0 - params.blend);

    let gain = ctx.create_gain()?;
    let compressor = ctx.create_dynamics_compressor()?;
    compressor.threshold().set_value(-30.0 * params.punch);
    compressor.knee().set_value(1.0);
    compressor.ratio().set_value(5.0);
    compressor.attack().set_value(0.1);
    compressor.release().set_value(0.1);

    osc.connect_with_audio_node(&osc_gain)?;
    white_noise.connect_with_audio_node(&noise_gain)?;
    osc_gain.connect_with_audio_node(&gain)?;
    noise_gain.connect_with_audio_node(&gain)?;
    gain.connect_with_audio_node(&compressor)?;
    compressor.connect_with_audio_node(&ctx.destination())?;

    osc
      .frequency()
      .exponential_ramp_to_value_at_time(params.freq, time)?;
    gain.gain().set_value(0.0);
    gain
      .gain()
      .set_target_at_time(0.25 * params.volume, time, 0.0005)?;
    let decay = (params.decay * 0.5) as f64;
    gain.gain().set_target_at_time(0.0, time + decay, decay)?;
    osc.start()?;
    osc.stop_with_when(time + 4.0)?;
    white_noise.start()?;
    white_noise.stop_with_when(time + 4.0)?;
    Ok(())
  }
}
