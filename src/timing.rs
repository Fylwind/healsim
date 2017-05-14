use std::{cmp, mem};
use std::ops::{Add, Index};
use std::time::{Duration, Instant};
use std::collections::{binary_heap, BinaryHeap};
use vec_arena::Arena;

pub fn duration_to_f64(t: Duration) -> f64 {
    t.as_secs() as f64 + 1e-9 * t.subsec_nanos() as f64
}

pub fn duration_from_f64(t: f64) -> Duration {
    debug_assert!(t >= 0.0);
    Duration::new(t.floor() as _, (t % 1.0 * 1e9) as _)
}

#[derive(Clone, Debug)]
pub struct Clock {
    pub time: Instant,
    pub time_delta: f64,
}

impl Clock {
    pub fn new() -> Self {
        Self {
            time: Instant::now(),
            time_delta: Default::default(),
        }
    }

    pub fn update(&mut self) {
        let now = Instant::now();
        let prev_time = mem::replace(&mut self.time, now);
        self.time_delta = duration_to_f64(now - prev_time);
    }
}

/// Convenience overload for getting a future time.
impl<'a> Add<f64> for &'a Clock {
    type Output = Instant;
    fn add(self, rhs: f64) -> Self::Output {
        self.time + duration_from_f64(rhs)
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

/// An ID associated with a watch timer.  Note: This is specific to a given
/// `Watch` object!
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WatchTimer(pub usize);

#[derive(Clone, Copy, Debug, Eq)]
struct WatchItem {
    when: Instant,
    timer: WatchTimer,
}

impl PartialEq for WatchItem {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == cmp::Ordering::Equal
    }
}

impl PartialOrd for WatchItem {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WatchItem {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        other.when.cmp(&self.when)
    }
}

/// Timer multiplexer.
#[derive(Clone, Debug)]
pub struct Watch<T> {
    queue: BinaryHeap<WatchItem>,
    // the arena uses None as tombstones for canceled timers
    arena: Arena<Option<(Instant, T)>>,
}

impl<T> Default for Watch<T> {
    fn default() -> Self {
        Self {
            queue: Default::default(),
            arena: Default::default(),
        }
    }
}

impl<T> Watch<T> {
    pub fn query(&self, timer: WatchTimer) -> Option<&(Instant, T)> {
        self.arena.get(timer.0).and_then(|x| x.as_ref())
    }

    pub fn schedule(&mut self, when: Instant, data: T) -> WatchTimer {
        let timer = WatchTimer(self.arena.insert(Some((when, data))));
        self.queue.push(WatchItem { when, timer });
        timer
    }

    pub fn cancel(&mut self, timer: WatchTimer) -> Option<(Instant, T)> {
        self.arena.get_mut(timer.0).and_then(|item| item.take())
    }

    pub fn poll(&mut self, now: Instant) -> WatchPoll<T> {
        WatchPoll { now, watch: self }
    }
}

impl<T> Index<WatchTimer> for Watch<T> {
    type Output = (Instant, T);
    fn index(&self, timer: WatchTimer) -> &Self::Output {
        self.arena[timer.0].as_ref().unwrap()
    }
}

#[derive(Debug)]
pub struct WatchPoll<'a, T: 'a> {
    now: Instant,
    watch: &'a mut Watch<T>,
}

impl<'a, T> Iterator for WatchPoll<'a, T> {
    type Item = (Instant, T);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.watch.queue.peek_mut() {
            if self.now < top.when {
                break;
            }
            let timer = binary_heap::PeekMut::pop(top).timer.0;
            let data = self.watch.arena.remove(timer)
                .expect("can't find queued item in arena!?");
            if data.is_some() {
                return data;
            }
        }
        None
    }
}
