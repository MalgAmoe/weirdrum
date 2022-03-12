use rand::Rng;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use web_sys::{AudioBuffer, AudioContext, BiquadFilterType};

pub struct Hat {
    pub nodes: HatNodes,
    pub params: HatParams,
}

#[derive(Copy, Clone)]
pub struct HatParams {
    pub freq: f32,
    pub decay: f32,
    pub punch: f32,
    pub volume: f32,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct HatValues {
    pub freq: f32,
    pub decay: f32,
    pub punch: f32,
    pub volume: f32,
    pub step_type: String,
}

pub struct HatNodes {
    noise_buffer: AudioBuffer,
}

impl Default for HatParams {
    fn default() -> HatParams {
        HatParams {
            freq: 4000.0,
            decay: 0.1,
            punch: -30.0 * 0.0,
            volume: 0.7,
        }
    }
}

impl Hat {
    pub fn new(ctx: &AudioContext) -> Result<Self, JsValue> {
        let sr = ctx.sample_rate();
        let noise_buffer = ctx.create_buffer(1, sr as u32, sr)?;
        let noise_output = &mut noise_buffer.get_channel_data(0)?;
        let mut rng = rand::thread_rng();
        for i in 0..sr as usize {
            noise_output[i] = 2.0 * rng.gen::<f32>() - 1.0;
        }
        noise_buffer.copy_to_channel(noise_output, 0)?;
        Ok(Hat {
            params: HatParams::default(),
            nodes: HatNodes { noise_buffer },
        })
    }
}

impl super::Sound for Hat {
    fn update(&mut self, params: super::SoundParams) {
        match params {
            super::SoundParams::Hat(hat_params) => self.params = hat_params,
            _ => {}
        }
    }
    fn play(
        &self,
        ctx: &AudioContext,
        hat_params: Option<super::SoundParams>,
        time_delta: f64,
        offset: f64,
    ) -> Result<(), JsValue> {
        let params = match hat_params {
            Some(super::SoundParams::Hat(params)) => params,
            _ => self.params,
        };
        let time = time_delta + offset + 0.05;
        let white_noise = ctx.create_buffer_source()?;
        white_noise.set_buffer(Some(&self.nodes.noise_buffer));
        white_noise.set_loop(true);

        let gain = ctx.create_gain()?;
        let compressor = ctx.create_dynamics_compressor()?;
        let filter = ctx.create_biquad_filter()?;
        filter.set_type(BiquadFilterType::Highpass);
        filter.frequency().set_value(params.freq);
        
        compressor.threshold().set_value(-30.0 * params.punch);
        compressor.knee().set_value(1.0);
        compressor.ratio().set_value(5.0);
        compressor.attack().set_value(0.1);
        compressor.release().set_value(0.1);

        white_noise.connect_with_audio_node(&filter)?;
        filter.connect_with_audio_node(&gain)?;
        gain.connect_with_audio_node(&compressor)?;
        compressor.connect_with_audio_node(&ctx.destination())?;

        gain.gain().set_value(0.0);
        gain.gain()
            .set_target_at_time(0.25 * params.volume, time, 0.0005)?;
        let decay = (params.decay * 0.5) as f64;
        gain.gain().set_target_at_time(0.0, time + decay, decay)?;
        white_noise.start()?;
        white_noise.stop_with_when(time + 4.0)?;
        Ok(())
    }
}
