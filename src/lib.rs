mod utils;

use wasm_bindgen::prelude::*;
use web_sys::console;
use web_sys::AudioContext;

mod sounds;
use sounds::kick::{Kick, KickValues};
use sounds::snare::{Snare, SnareValues};
use sounds::{wave_string_to_osc, Sound};

mod sequencer;
use sequencer::{get_sequencer_steps, Sequencer, Trigger};

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
    snare_sequencer: Sequencer,
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
        let mut snare_sequencer = Sequencer::new(90.0);
        snare_sequencer.sequence = Default::default();

        Ok(Audio {
            ctx,
            schedule_interval: 0.04,
            kick_sequencer: kick_sequencer,
            default_kick: Kick::default(),
            snare_sequencer: snare_sequencer,
            default_snare: Snare::default(),
            tempo: 90.0,
        })
    }

    fn get_sequencer(&mut self, seq: &str) -> &mut Sequencer {
        match seq {
            "snare" => &mut self.snare_sequencer,
            &_ => &mut self.kick_sequencer
        }
    }

    #[wasm_bindgen]
    pub fn update_kick(
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
    pub fn update_snare(
        &mut self,
        freq: f32,
        blend: f32,
        decay: f32,
        punch: f32,
        volume: f32,
    ) -> Result<(), JsValue> {
        let snare = Snare {
            freq,
            blend,
            decay,
            punch: punch,
            volume,
        };
        self.default_snare = snare;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn start(&mut self) -> Result<(), JsValue> {
        // let l = format!("{:?}", self.ctx.current_time());
        // console::log_1(&l.into());
        self.kick_sequencer.play(&self.ctx);
        self.snare_sequencer.play(&self.ctx);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn stop(&mut self) {
        self.kick_sequencer.stop();
        self.snare_sequencer.stop();
    }

    #[wasm_bindgen]
    pub fn schedule(&mut self) -> Result<(), JsValue> {
        self.kick_sequencer.schedule_sounds(
            &self.ctx,
            &self.default_kick,
            self.schedule_interval,
        )?;
        self.snare_sequencer.schedule_sounds(
            &self.ctx,
            &self.default_snare,
            self.schedule_interval,
        )?;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn get_steps(&mut self, seq_name: &str) -> i8 {
        let time = self.ctx.current_time();
        let seq = self.get_sequencer(seq_name);
        get_sequencer_steps(seq, time)
    }

    #[wasm_bindgen]
    pub fn update_sequencer_length(&mut self, seq_name: &str, length: i8) {
        let tempo = self.tempo;
        let seq = self.get_sequencer(seq_name);
        seq.steps = length;
        seq.step_delta = (60.0 / tempo as f64) * (4.0 / length as f64);
    }

    #[wasm_bindgen]
    pub fn update_offset(&mut self, seq_name: &str, offset: f64) {
        let seq = self.get_sequencer(seq_name);
        seq.offset = offset;
    }

    #[wasm_bindgen]
    pub fn update_tempo(&mut self, tempo: f32) {
        self.tempo = tempo;
        // let seq = self.get_sequencer();
        self.kick_sequencer.step_delta =
            (60.0 / tempo as f64) * (4.0 / self.kick_sequencer.steps as f64);
        self.snare_sequencer.step_delta =
            (60.0 / tempo as f64) * (4.0 / self.snare_sequencer.steps as f64);
    }

    #[wasm_bindgen]
    pub fn update_kick_steps(&mut self, steps: JsValue) {
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

    #[wasm_bindgen]
    pub fn update_snare_steps(&mut self, steps: JsValue) {
        let elements: Vec<SnareValues> = steps.into_serde().unwrap();
        let mut steps: [Option<Trigger<Box<dyn Sound>>>; 16] = Default::default();
        for i in 0..16 {
            steps[i] = match &elements[i] {
                SnareValues {
                    freq,
                    blend,
                    decay,
                    punch,
                    volume,
                    step_type,
                } => match step_type.as_str() {
                    "trigger" => {
                        self.default_snare = Snare {
                            freq: *freq,
                            blend: *blend,
                            decay: *decay,
                            punch: *punch,
                            volume: *volume,
                        };
                        Some(Trigger::NormalTrigger)
                    }
                    "lock_trigger" => Some(Trigger::LockTrigger(Box::new(Snare {
                        freq: *freq,
                        blend: *blend,
                        decay: *decay,
                        punch: *punch,
                        volume: *volume,
                    }))),
                    &_ => None,
                },
            }
        }
        self.snare_sequencer.sequence = steps;
    }
}
