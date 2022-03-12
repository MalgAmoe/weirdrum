mod utils;

use wasm_bindgen::prelude::*;
use web_sys::console;
use web_sys::AudioContext;

mod sounds;
use sounds::hat::{Hat, HatParams, HatValues};
use sounds::kick::{Kick, KickParams, KickValues};
use sounds::snare::{Snare, SnareParams, SnareValues};
use sounds::{wave_string_to_osc, SoundParams};

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
    snare_sequencer: Sequencer,
    hat_sequencer: Sequencer,
    tempo: f32,
}

#[wasm_bindgen]
impl Audio {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<Audio, JsValue> {
        let ctx = web_sys::AudioContext::new()?;
        let kick = Kick::new(&ctx)?;
        let mut kick_sequencer = Sequencer::new(90.0, Box::new(kick));
        kick_sequencer.sequence = Default::default();
        let snare = Snare::new(&ctx)?;
        let mut snare_sequencer = Sequencer::new(90.0, Box::new(snare));
        snare_sequencer.sequence = Default::default();
        let hat = Hat::new(&ctx)?;
        let mut hat_sequencer = Sequencer::new(90.0, Box::new(hat));
        hat_sequencer.sequence = Default::default();

        Ok(Audio {
            ctx,
            schedule_interval: 0.04,
            kick_sequencer,
            snare_sequencer,
            hat_sequencer,
            tempo: 90.0,
        })
    }

    fn get_sequencer(&mut self, seq: &str) -> &mut Sequencer {
        match seq {
            "snare" => &mut self.snare_sequencer,
            "hat" => &mut self.hat_sequencer,
            &_ => &mut self.kick_sequencer,
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
        let kick = KickParams {
            freq,
            pitch,
            wave,
            decay,
            punch: punch,
            volume,
        };
        self.kick_sequencer.sound.update(SoundParams::Kick(kick));
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
        let snare = SnareParams {
            freq,
            blend,
            decay,
            punch: punch,
            volume,
        };
        self.snare_sequencer.sound.update(SoundParams::Snare(snare));
        Ok(())
    }

    #[wasm_bindgen]
    pub fn update_hat(
        &mut self,
        freq: f32,
        decay: f32,
        punch: f32,
        volume: f32,
    ) -> Result<(), JsValue> {
        let hat = HatParams {
            freq,
            decay,
            punch: punch,
            volume,
        };
        self.hat_sequencer.sound.update(SoundParams::Hat(hat));
        Ok(())
    }

    #[wasm_bindgen]
    pub fn start(&mut self) -> Result<(), JsValue> {
        self.kick_sequencer.play(&self.ctx);
        self.snare_sequencer.play(&self.ctx);
        self.hat_sequencer.play(&self.ctx);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn stop(&mut self) {
        self.kick_sequencer.stop();
        self.snare_sequencer.stop();
        self.hat_sequencer.stop();
    }

    #[wasm_bindgen]
    pub fn schedule(&mut self) -> Result<(), JsValue> {
        self.kick_sequencer
            .schedule_sounds(&self.ctx, self.schedule_interval)?;
        self.snare_sequencer
            .schedule_sounds(&self.ctx, self.schedule_interval)?;
        self.hat_sequencer
            .schedule_sounds(&self.ctx, self.schedule_interval)?;
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
        self.kick_sequencer.step_delta =
            (60.0 / tempo as f64) * (4.0 / self.kick_sequencer.steps as f64);
        self.snare_sequencer.step_delta =
            (60.0 / tempo as f64) * (4.0 / self.snare_sequencer.steps as f64);
        self.hat_sequencer.step_delta =
            (60.0 / tempo as f64) * (4.0 / self.hat_sequencer.steps as f64);
    }

    #[wasm_bindgen]
    pub fn update_kick_steps(&mut self, steps: JsValue) {
        let elements: Vec<KickValues> = steps.into_serde().unwrap();
        let mut steps: [Option<Trigger<SoundParams>>; 16] = Default::default();
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
                        self.kick_sequencer
                            .sound
                            .update(SoundParams::Kick(KickParams {
                                freq: *freq,
                                pitch: *pitch,
                                wave: wave_string_to_osc(wave),
                                decay: *decay,
                                punch: *punch,
                                volume: *volume,
                            }));
                        Some(Trigger::NormalTrigger)
                    }
                    "lock_trigger" => Some(Trigger::LockTrigger(SoundParams::Kick(KickParams {
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
        let mut steps: [Option<Trigger<SoundParams>>; 16] = Default::default();
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
                        self.snare_sequencer
                            .sound
                            .update(SoundParams::Snare(SnareParams {
                                freq: *freq,
                                blend: *blend,
                                decay: *decay,
                                punch: *punch,
                                volume: *volume,
                            }));
                        Some(Trigger::NormalTrigger)
                    }
                    "lock_trigger" => Some(Trigger::LockTrigger(SoundParams::Snare(SnareParams {
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

    #[wasm_bindgen]
    pub fn update_hat_steps(&mut self, steps: JsValue) {
        let elements: Vec<HatValues> = steps.into_serde().unwrap();
        let mut steps: [Option<Trigger<SoundParams>>; 16] = Default::default();
        for i in 0..16 {
            steps[i] = match &elements[i] {
                HatValues {
                    freq,
                    decay,
                    punch,
                    volume,
                    step_type,
                } => match step_type.as_str() {
                    "trigger" => {
                        self.hat_sequencer
                            .sound
                            .update(SoundParams::Hat(HatParams {
                                freq: *freq,
                                decay: *decay,
                                punch: *punch,
                                volume: *volume,
                            }));
                        Some(Trigger::NormalTrigger)
                    }
                    "lock_trigger" => {
                        Some(Trigger::LockTrigger(SoundParams::Hat(HatParams {
                            freq: *freq,
                            decay: *decay,
                            punch: *punch,
                            volume: *volume,
                        })))
                    }
                    &_ => None,
                },
            }
        }
        self.hat_sequencer.sequence = steps;
    }
}
