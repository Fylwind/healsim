use std;

pub fn duration_to_f64(t: std::time::Duration) -> f64 {
    t.as_secs() as f64 + 1e-9 * t.subsec_nanos() as f64
}

#[derive(Clone, Debug)]
pub struct Clock {
    pub time: std::time::Instant,
    pub time_delta: f64,
}

impl Clock {
    pub fn new() -> Self {
        Self {
            time: std::time::Instant::now(),
            time_delta: Default::default(),
        }
    }

    pub fn update(&mut self) {
        let now = std::time::Instant::now();
        let prev_time = std::mem::replace(&mut self.time, now);
        self.time_delta = duration_to_f64(now - prev_time);
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Countdown {
    pub remaining: f64,
}

impl From<f64> for Countdown {
    fn from(remaining: f64) -> Self {
        Self { remaining }
    }
}

impl From<Countdown> for f64 {
    fn from(countdown: Countdown) -> Self {
        countdown.remaining
    }
}

impl Countdown {
    pub fn tick(&mut self, clock: &Clock) -> Result<(), ()> {
        if !self.is_expired() {
            self.remaining -= clock.time_delta;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn reset(&mut self, time: f64) {
        self.remaining = time;
    }

    pub fn is_expired(&self) -> bool {
        self.remaining <= 0.0
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Timer {
    pub duration: f64,
    pub countdown: Countdown,
}

impl From<f64> for Timer {
    fn from(duration: f64) -> Self {
        Self {
            duration: duration,
            countdown: From::from(duration),
        }
    }
}

impl Timer {
    pub fn tick(&mut self, clock: &Clock) -> Result<(), ()> {
        self.countdown.tick(clock)
    }

    pub fn reset(&mut self, time: f64) {
        *self = From::from(time);
    }

    pub fn remaining(&self) -> f64 {
        self.countdown.remaining
    }

    pub fn is_expired(&self) -> bool {
        self.countdown.is_expired()
    }
}
