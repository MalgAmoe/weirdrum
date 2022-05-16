use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use web_sys::AudioContext;

#[derive(Copy, Clone)]
pub struct KickParams {
    pub freq: f32,
    pub pitch: f32,
    pub wave: web_sys::OscillatorType,
    pub decay: f32,
    pub punch: f32,
    pub volume: f32,
}

pub struct Kick {
    nodes: KickNodes,
    pub params: KickParams,
}

struct KickNodes {
    volume: web_sys::GainNode
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

impl Default for KickParams {
    fn default() -> KickParams {
        KickParams {
            freq: 40.0,
            pitch: 1.0,
            wave: web_sys::OscillatorType::Sine,
            decay: 0.1,
            punch: -30.0 * 0.0,
            volume: 0.7,
        }
    }
}

impl Kick {
    pub fn new(ctx: &AudioContext) -> Result<Self, JsValue> {
        let volume = ctx.create_gain()?;
        volume.gain().set_value(0.9);
        volume.connect_with_audio_node(&ctx.destination())?;
        Ok(Kick {
            params: KickParams::default(),
            nodes: KickNodes {
                volume
            },
        })
    }
}

impl super::Sound for Kick {
    fn update_volume(&mut self, ctx: &AudioContext, volume: f32) -> Result<(), JsValue> {
        self.nodes.volume.gain().linear_ramp_to_value_at_time(volume, ctx.current_time() + 0.01)?;
        Ok(())
    }
    fn update(&mut self, params: super::SoundParams) {
        if let super::SoundParams::Kick(kick_params) = params {
            self.params = kick_params
        }
    }
    fn play(
        &self,
        ctx: &AudioContext,
        kick_params: Option<super::SoundParams>,
        time_delta: f64,
        offset: f64,
    ) -> Result<(), JsValue> {
        let params = match kick_params {
            Some(super::SoundParams::Kick(params)) => params,
            _ => self.params,
        };
        let time = time_delta + offset + 0.05;
        let osc = ctx.create_oscillator()?;
        osc.set_type(params.wave);
        let gain = ctx.create_gain()?;
        let compressor = ctx.create_dynamics_compressor()?;
        compressor.knee().set_value(1.0);
        compressor.ratio().set_value(5.0);
        compressor.attack().set_value(0.1);
        compressor.release().set_value(0.1);
        compressor.connect_with_audio_node(&self.nodes.volume)?;
        compressor.threshold().set_value(-30.0 * params.punch);

        osc.connect_with_audio_node(&gain)?;
        gain.connect_with_audio_node(&compressor)?;
        osc.frequency()
            .set_value_at_time(params.freq + params.freq * params.pitch, time)?;
        osc.frequency()
            .exponential_ramp_to_value_at_time(params.freq, time + 0.02)?;
        gain.gain().set_value(0.0);
        gain.gain()
            .set_target_at_time(0.25 * params.volume, time, 0.0005)?;
        let decay = (params.decay * 0.5) as f64;
        gain.gain().set_target_at_time(0.0, time + decay, decay)?;
        osc.start()?;
        osc.stop_with_when(time + 4.0)?;
        Ok(())
    }
}
