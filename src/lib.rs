mod utils;

use wasm_bindgen::prelude::*;
use web_sys::console;
use web_sys::AudioContext;

mod sounds;
use sounds::snare::{Snare};
use sounds::kick::{Kick, KickValues};
use sounds::{Sound, wave_string_to_osc};

mod sequencer;
use sequencer::{Sequencer, get_sequencer_steps, Trigger};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Audio {
    ctx: AudioContext,
    schedule_interval: f32,
    kick_sequencer: Sequencer,
    default_kick: Kick,
    default_snare: Snare,
    tempo: f32,
}

#[wasm_bindgen]
impl Audio {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<Audio, JsValue> {
        let ctx = web_sys::AudioContext::new()?;
        let mut kick_sequencer = Sequencer::new(90.0);
        kick_sequencer.sequence = Default::default();

        Ok(Audio {
            ctx,
            schedule_interval: 0.04,
            kick_sequencer: kick_sequencer,
            default_kick: Kick::default(),
            default_snare: Snare::default(),
            tempo: 90.0,
        })
    }

    fn get_sequencer(&mut self) -> &mut Sequencer {
        &mut self.kick_sequencer
    }

    #[wasm_bindgen]
    pub fn update(
        &mut self,
        freq: f32,
        pitch: f32,
        wave_str: &str,
        decay: f32,
        punch: f32,
        volume: f32,
    ) -> Result<(), JsValue> {
        let wave = wave_string_to_osc(wave_str);
        let kick = Kick {
            freq,
            pitch,
            wave,
            decay,
            punch: punch,
            volume,
        };
        self.default_kick = kick;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn start(&mut self) -> Result<(), JsValue> {
        // let l = format!("{:?}", self.ctx.current_time());
        // console::log_1(&l.into());
        self.kick_sequencer.play(&self.ctx);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn stop(&mut self) {
        self.kick_sequencer.stop();
    }

    #[wasm_bindgen]
    pub fn schedule(&mut self) -> Result<(), JsValue> {
        self.kick_sequencer.schedule_sounds(
            &self.ctx,
            &self.default_kick,
            self.schedule_interval,
        )?;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn get_steps(&mut self) -> i8 {
        let time = self.ctx.current_time();
        let seq = self.get_sequencer();
        get_sequencer_steps(seq, time)
    }

    #[wasm_bindgen]
    pub fn update_sequencer_length(&mut self, length: i8) {
        let tempo = self.tempo;
        let seq = self.get_sequencer();
        seq.steps = length;
        seq.step_delta = (60.0 / tempo as f64) * (4.0 / length as f64);
    }

    #[wasm_bindgen]
    pub fn update_offset(&mut self, offset: f64) {
        let seq = self.get_sequencer();
        seq.offset = offset;
    }

    #[wasm_bindgen]
    pub fn update_tempo(&mut self, tempo: f32) {
        self.tempo = tempo;
        let seq = self.get_sequencer();
        seq.step_delta =
            (60.0 / tempo as f64) * (4.0 / seq.steps as f64);
    }

    #[wasm_bindgen]
    pub fn update_steps(&mut self, steps: JsValue) {
        let elements: Vec<KickValues> = steps.into_serde().unwrap();
        let mut steps: [Option<Trigger<Box<dyn Sound>>>; 16] = Default::default();
        for i in 0..16 {
            steps[i] = match &elements[i] {
                KickValues {
                    freq,
                    pitch,
                    wave,
                    decay,
                    punch,
                    volume,
                    step_type,
                } => match step_type.as_str() {
                    "trigger" => {
                        self.default_kick = Kick {
                            freq: *freq,
                            pitch: *pitch,
                            wave: wave_string_to_osc(wave),
                            decay: *decay,
                            punch: *punch,
                            volume: *volume,
                        };
                        Some(Trigger::NormalTrigger)
                    }
                    "lock_trigger" => Some(Trigger::LockTrigger(Box::new(Kick {
                        freq: *freq,
                        pitch: *pitch,
                        wave: wave_string_to_osc(wave),
                        decay: *decay,
                        punch: *punch,
                        volume: *volume,
                    }))),
                    &_ => None,
                },
            }
        }
        self.kick_sequencer.sequence = steps;
    }
}
