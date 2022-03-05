use wasm_bindgen::prelude::*;
use web_sys::AudioContext;
use crate::sounds::{Sound};

pub struct Sequencer {
  pub sequence: [Option<Trigger<Box<dyn Sound>>>; 16],
  trigger_times: [Option<f64>; 16],
  pub steps: i8,
  step_to_schedule: i8,
  step_playing: i8,
  next_step_time: f64,
  pub step_delta: f64,
  pub offset: f64,
}

pub enum Trigger<Sound> {
  NormalTrigger,
  LockTrigger(Sound),
}

pub fn get_sequencer_steps(sequencer: &mut Sequencer, time: f64) -> i8 {
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

fn get_step(i: i8, steps: i8) -> i8 {
  let mut step = i - 1;
  if step < 0 {
      step = steps - 1;
  }
  step
}

impl Sequencer {
  pub fn new(tempo: f32) -> Self {
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

  pub fn schedule_sounds(
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

  pub fn play(&mut self, ctx: &AudioContext) {
      self.next_step_time = ctx.current_time();
  }

  pub fn stop(&mut self) {
      self.step_to_schedule = 0;
      self.step_playing = 0;
      self.trigger_times = Default::default();
  }
}