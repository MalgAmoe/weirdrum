mod utils;

use wasm_bindgen::prelude::*;
use web_sys::console;
use web_sys::AudioContext;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub struct Audio {
    ctx: AudioContext,
    schedule_interval: f32,
    sequencer: Sequencer,
}

struct GlobalTransport {
    tempo: f32,
    playing: bool,
}

#[derive(Debug, Clone, Copy)]
struct Sequencer {
    sequence: [Option<KickTrigger>; 16],
    trigger_times: [Option<f64>; 16],
    steps: i8,
    step_to_schedule: i8,
    step_playing: i8,
    next_step_time: f64,
    step_delta: f64,
    default_trigger: Kick,
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
            default_trigger: Kick::default(),
        }
    }
    fn schedule_sounds(
        &mut self,
        ctx: &AudioContext,
        schedule_interval: f32,
    ) -> Result<(), JsValue> {
        while self.next_step_time < ctx.current_time() + schedule_interval as f64 {
            // let l = format!("{:?}", ctx.current_time());
            // console::log_1(&l.into());
            match self.sequence[self.step_to_schedule as usize] {
                Some(kick_trigger) => match kick_trigger {
                    KickTrigger::LockTrigger(kick) => {
                        play_kick(ctx, kick, self.next_step_time)?;
                    }
                    KickTrigger::Trigger => {
                        play_kick(ctx, self.default_trigger, self.next_step_time)?;
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

#[derive(Debug, Clone, Copy)]
enum Trigger {
    KickTrigger,
}

#[derive(Debug, Clone, Copy)]
enum KickTrigger {
    Trigger,
    LockTrigger(Kick),
}

#[derive(Debug, Clone, Copy)]
struct Kick {
    freq: f32,
    pitch: f32,
    wave: web_sys::OscillatorType,
    decay: f32,
    attack: f32,
    volume: f32,
}

impl Default for Kick {
    fn default() -> Kick {
        Kick {
            freq: 40.0,
            pitch: 1.0,
            wave: web_sys::OscillatorType::Sine,
            decay: 0.1,
            attack: -30.0 * 0.0,
            volume: 0.7,
        }
    }
}

fn wave_string_to_osc(wave: &str) -> web_sys::OscillatorType {
    match wave {
        "triangle" => web_sys::OscillatorType::Triangle,
        _ => web_sys::OscillatorType::Sine,
    }
}

fn play_kick(ctx: &AudioContext, values: Kick, time_delta: f64) -> Result<(), JsValue> {
    let time = time_delta /* + 0.05 */;
    let osc = ctx.create_oscillator()?;
    osc.set_type(values.wave);
    let gain = ctx.create_gain()?;

    let compressor = ctx.create_dynamics_compressor()?;
    compressor.threshold().set_value(values.attack);
    compressor.knee().set_value(1.0);
    compressor.ratio().set_value(5.0);
    compressor.attack().set_value(0.1);
    compressor.release().set_value(0.1);

    osc.connect_with_audio_node(&gain)?;
    gain.connect_with_audio_node(&compressor)?;
    compressor.connect_with_audio_node(&ctx.destination())?;
    // out.connect_with_audio_node(&ctx.destination())?;

    osc.frequency()
        .set_value_at_time(values.freq + values.freq * values.pitch, time)?;
    osc.frequency()
        .exponential_ramp_to_value_at_time(values.freq, time + 0.02)?;
    gain.gain().set_value(0.0);
    gain.gain()
        .set_target_at_time(0.5 * values.volume, time, 0.0005)?;
    let decay = (values.decay * 0.5) as f64;
    gain.gain().set_target_at_time(0.0, time + decay, decay)?;
    osc.start()?;
    osc.stop_with_when(time + 4.0)?;
    Ok(())
}

#[wasm_bindgen]
impl Audio {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<Audio, JsValue> {
        let ctx = web_sys::AudioContext::new()?;
        let mut kick_sequencer = Sequencer::new(120.0);
        let kick = Kick {
            freq: 40.0,
            pitch: 10.0,
            wave: web_sys::OscillatorType::Sine,
            decay: 0.1,
            attack: -30.0 * 0.5,
            volume: 0.7,
        };
        let kick2 = Kick {
            freq: 50.0,
            pitch: 8.0,
            wave: web_sys::OscillatorType::Sine,
            decay: 0.3,
            attack: -30.0 * 1.0,
            volume: 0.7,
        };
        let kick3 = Kick {
            freq: 60.0,
            pitch: 8.0,
            wave: web_sys::OscillatorType::Sine,
            decay: 0.1,
            attack: -30.0 * 1.0,
            volume: 0.7,
        };
        kick_sequencer.sequence = [
            Some(KickTrigger::LockTrigger(kick)),
            None,
            None,
            None,
            Some(KickTrigger::Trigger),
            None,
            None,
            None,
            Some(KickTrigger::LockTrigger(kick)),
            None,
            None,
            None,
            Some(KickTrigger::Trigger),
            None,
            None,
            None,
        ];

        Ok(Audio {
            ctx,
            schedule_interval: 0.1,
            sequencer: kick_sequencer,
        })
    }

    #[wasm_bindgen]
    pub fn play(
        &mut self,
        freq: f32,
        pitch: f32,
        wave_str: &str,
        decay: f32,
        attack: f32,
        volume: f32,
    ) -> Result<(), JsValue> {
        let wave = wave_string_to_osc(wave_str);
        let kick = Kick {
            freq,
            pitch,
            wave,
            decay,
            attack: -30.0 * attack,
            volume,
        };
        play_kick(&self.ctx, kick, self.ctx.current_time())?;
        // let l = format!("{:?}", self.ctx.current_time());
        // console::log_1(&l.into());
        // self.sequencer.play(&self.ctx);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn update(
        &mut self,
        freq: f32,
        pitch: f32,
        wave_str: &str,
        decay: f32,
        attack: f32,
        volume: f32,
    ) -> Result<(), JsValue> {
        let wave = wave_string_to_osc(wave_str);
        let kick = Kick {
            freq,
            pitch,
            wave,
            decay,
            attack: -30.0 * attack,
            volume,
        };
        self.sequencer.default_trigger = kick;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn start(&mut self) -> Result<(), JsValue> {
        // let l = format!("{:?}", self.ctx.current_time());
        // console::log_1(&l.into());
        self.sequencer.play(&self.ctx);
        Ok(())
    }

    #[wasm_bindgen]
    pub fn stop(&mut self) {
        self.sequencer.stop();
    }

    #[wasm_bindgen]
    pub fn schedule(&mut self) -> Result<(), JsValue> {
        self.sequencer
            .schedule_sounds(&self.ctx, self.schedule_interval)?;
        Ok(())
    }

    #[wasm_bindgen]
    pub fn get_steps(&mut self) -> i8 {
        let time = self.ctx.current_time();
        let mut step = get_step(self.sequencer.step_to_schedule);
        for _ in 0..16 {
            match self.sequencer.trigger_times[step as usize] {
                Some(trigger_time) => {
                    if trigger_time < time {
                        self.sequencer.step_playing = step;
                        return step;
                    }
                }
                _ => {}
            }
            step = get_step(self.sequencer.step_to_schedule);
        }
        // let l = format!("yeyeyey{:?}", step);
        // console::log_1(&l.into());
        self.sequencer.step_playing
    }
}

fn get_step(i: i8) -> i8 {
    let mut step = i - 1;
    if step < 0 {
        step = 15;
    }
    step
}
