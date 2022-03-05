mod utils;

use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use web_sys::console;
use web_sys::AudioContext;

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
    tempo: f32,
}

// #[derive(Debug, Clone, Copy)]
struct Sequencer {
    sequence: [Option<Trigger<Box<dyn Sound>>>; 16],
    trigger_times: [Option<f64>; 16],
    steps: i8,
    step_to_schedule: i8,
    step_playing: i8,
    next_step_time: f64,
    step_delta: f64,
    offset: f64,
}

impl Sequencer {
    fn new(tempo: f32) -> Self {
        Sequencer {
            sequence: Default::default(),
            trigger_times: Default::default(),
            steps: 16,
            step_to_schedule: 0,
            step_playing: 0,
            next_step_time: 0.0,
            step_delta: (60.0 / tempo as f64) * (4.0 / 16.0),
            offset: 0.0,
        }
    }

    fn schedule_sounds(
        &mut self,
        ctx: &AudioContext,
        sound: &dyn Sound,
        schedule_interval: f32,
    ) -> Result<(), JsValue> {
        while self.next_step_time < ctx.current_time() + schedule_interval as f64 {
            match &self.sequence[self.step_to_schedule as usize] {
                Some(trigger) => match trigger {
                    Trigger::LockTrigger(locked_sound) => {
                        locked_sound.play(ctx, self.next_step_time, self.offset)?;
                    }
                    Trigger::NormalTrigger => {
                        sound.play(ctx, self.next_step_time, self.offset)?;
                    }
                },
                None => {}
            }
            self.trigger_times[self.step_to_schedule as usize] = Some(self.next_step_time);
            self.step_to_schedule += 1;
            if self.step_to_schedule >= self.steps {
                self.step_to_schedule = 0;
            }
            self.next_step_time += self.step_delta;
        }
        Ok(())
    }

    fn play(&mut self, ctx: &AudioContext) {
        self.next_step_time = ctx.current_time();
    }

    fn stop(&mut self) {
        self.step_to_schedule = 0;
        self.step_playing = 0;
        self.trigger_times = Default::default();
    }
}

trait Sound {
    fn play(&self, ctx: &AudioContext, time_delta: f64, offset: f64) -> Result<(), JsValue>;
}

enum Trigger<Sound> {
    NormalTrigger,
    LockTrigger(Sound),
}

// #[derive(Debug, Clone, Copy)]
struct Kick {
    freq: f32,
    pitch: f32,
    wave: web_sys::OscillatorType,
    decay: f32,
    punch: f32,
    volume: f32,
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

impl Sound for Kick {
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
    freq: f32,
    pitch: f32,
    wave: String,
    decay: f32,
    punch: f32,
    volume: f32,
    step_type: String,
}

fn wave_string_to_osc(wave: &str) -> web_sys::OscillatorType {
    match wave {
        "triangle" => web_sys::OscillatorType::Triangle,
        _ => web_sys::OscillatorType::Sine,
    }
}

fn get_sequencer_steps(sequencer: &mut Sequencer, time: f64) -> i8 {
    let mut step = get_step(
        sequencer.step_to_schedule,
        sequencer.steps,
    );
    for _ in 0..16 {
        match sequencer.trigger_times[step as usize] {
            Some(trigger_time) => {
                if trigger_time < time {
                    sequencer.step_playing = step;
                    return step;
                }
            }
            _ => {}
        }
        step = get_step(
            sequencer.step_to_schedule,
            sequencer.steps,
        );
    }
    sequencer.step_playing
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

fn get_step(i: i8, steps: i8) -> i8 {
    let mut step = i - 1;
    if step < 0 {
        step = steps - 1;
    }
    step
}
