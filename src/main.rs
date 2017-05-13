extern crate piston_window;
extern crate rand;

use std::f64::consts::PI;
use piston_window::*;
use piston_window::types::Color;
use piston_window::color::WHITE;

fn duration_to_f64(t: std::time::Duration) -> f64 {
    t.as_secs() as f64 + 1e-9 * t.subsec_nanos() as f64
}

#[derive(Clone, Copy, Debug, Default)]
struct Countdown {
    remaining: f64,
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
    fn tick(&mut self, env: &Env) -> Result<(), ()> {
        if !self.is_expired() {
            self.remaining -= env.time_delta;
            Ok(())
        } else {
            Err(())
        }
    }

    fn reset(&mut self, time: f64) {
        self.remaining = time;
    }

    fn is_expired(&self) -> bool {
        self.remaining <= 0.0
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Timer {
    duration: f64,
    countdown: Countdown,
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
    fn tick(&mut self, env: &Env) -> Result<(), ()> {
        self.countdown.tick(env)
    }

    fn reset(&mut self, time: f64) {
        *self = From::from(time);
    }

    fn remaining(&self) -> f64 {
        self.countdown.remaining
    }

    fn is_expired(&self) -> bool {
        self.countdown.is_expired()
    }
}

#[derive(Clone, Debug)]
struct Env {
    time_delta: f64,
    mouse_pos: [f64; 2],
    hitbox_id: Option<HitboxId>,
}

impl Env {
    fn selected(&self) -> Option<UnitId> {
        match self.hitbox_id {
            Some(HitboxId::UnitBar(unit_id)) => Some(unit_id),
            _ => None,
        }
    }
}

type UnitId = usize;

#[derive(Clone, Debug)]
struct Unit {
    health: f64,
    max_health: f64,
    armor: f64,
    buffs: Vec<Buff>,
}

impl Default for Unit {
    fn default() -> Self {
        Self {
            health: 100.0,
            max_health: 100.0,
            armor: Default::default(),
            buffs: Default::default(),
        }
    }
}

impl Unit {
    fn tick(&mut self, env: &Env, unit_id: UnitId, queue: &mut Vec<Action>) {
        let mut i = 0;
        while i < self.buffs.len() {
            if self.buffs[i].tick(env, unit_id, queue).is_err() {
                self.buffs.remove(i);
            } else {
                i += 1;
            }
        }
    }

    fn is_alive(&self) -> bool {
        self.max_health != 0.0
    }

    fn add_health(&mut self, amount: f64) {
        let max = self.max_health;
        clamped_add_assign(&mut self.health, amount, 0.0, max);
        if self.health == 0.0 {
            self.max_health = 0.0;
        }
    }

    fn add_buff(&mut self, buff: Buff) {
        if let (Some(_), Some(ref mut old_buff)) =
            (buff.class,
             self.buffs.iter_mut().filter(|b| b.class == buff.class).next())
        {
            match (buff.timer, &mut old_buff.timer) {
                (Some(timer), &mut Some(ref mut old_timer))
                    if timer.remaining() > old_timer.remaining() => {
                        old_timer.reset(timer.remaining());
                    }
                _ => {}
            }
            return;
        }
        self.buffs.push(buff);
    }

    fn damage(&mut self, amount: f64) {
        let damage = amount - self.armor;
        if damage > 0.0 {
            self.add_health(-damage);
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Target {
    Unit(UnitId),
    LowestHealth(UnitId, usize),
}

impl Target {
    fn apply<F: FnMut(&mut Unit)>(&self, units: &mut [Unit], mut callback: F) {
        match *self {
            Target::Unit(unit_id) => {
                callback(&mut units[unit_id]);
            }
            Target::LowestHealth(unit_id, n) => {
                callback(&mut units[unit_id]);
                let mut finder: Vec<_> = units.iter().enumerate().map(
                    |(unit_id, unit)| {
                        (unit.health / unit.max_health, unit_id)
                    }).collect();
                finder.sort_by(|&(x, _), &(y, _)| x.partial_cmp(&y).unwrap());
                for &mut (_, unit_id) in finder.iter_mut().take(n) {
                    callback(&mut units[unit_id]);
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Action {
    AddHealth { target: Target, amount: f64 },
    AddMana { amount: f64 },
    AddBuff { target: Target, buff: Buff },
}

fn clamped_add_assign(lhs: &mut f64, rhs: f64, min: f64, max: f64) {
    let sum = *lhs + rhs;
    if sum > max {
        *lhs = max;
    } else if sum < min {
        *lhs = min;
    } else {
        *lhs = sum;
    }
}

impl Action {
    fn act(self, _: &Env, state: &mut State) {
        match self {
            Action::AddHealth { target, amount } => {
                target.apply(&mut state.units, |unit| unit.add_health(amount));
            }
            Action::AddMana { amount } => {
                state.player.add_mana(amount);
            }
            Action::AddBuff { target, buff } => {
                target.apply(&mut state.units, |unit| {
                    unit.add_buff(buff.clone())
                });
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Spell {
    mana_cost: f64,
    cast_time: f64,
    action: Action,
}

impl Spell {
    fn queue_action(self, queue: &mut Vec<Action>) {
        queue.push(Action::AddMana {
            amount: -self.mana_cost,
        });
        queue.push(self.action);
    }
}

#[derive(Clone, Debug)]
enum Effect {
    HealthRegen { rate: f64 },
}

impl Effect {
    fn tick(&mut self, env: &Env, unit_id: UnitId, queue: &mut Vec<Action>) {
        match *self {
            Effect::HealthRegen { rate } => {
                queue.push(Action::AddHealth {
                    target: Target::Unit(unit_id),
                    amount: rate * env.time_delta,
                });
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Class {
    Revitalize,
}

#[derive(Clone, Debug)]
struct Buff {
    timer: Option<Timer>,
    class: Option<Class>,
    effect: Effect,
}

impl Buff {
    fn tick(&mut self, env: &Env, unit_id: UnitId, queue: &mut Vec<Action>)
            -> Result<(), ()> {
        self.effect.tick(env, unit_id, queue);
        match self.timer {
            Some(ref mut timer) => timer.tick(env),
            None => Ok(()),
        }
    }
}

#[derive(Clone, Debug)]
struct Player {
    mana: f64,
    max_mana: f64,
    mana_regen: f64,
    gcd: Timer,
    cast_spell: Option<(Spell, f64)>,
}

impl Player {
    fn tick(&mut self, env: &Env, queue: &mut Vec<Action>) {
        if let Some((spell, mut progress)) = self.cast_spell.take() {
            progress += env.time_delta;
            if progress < spell.cast_time {
                self.cast_spell = Some((spell, progress));
            } else {
                if spell.mana_cost <= self.mana {
                    spell.queue_action(queue);
                }
            }
        }

        let mana_regen = self.mana_regen;
        self.add_mana(mana_regen * env.time_delta);

        let _ = self.gcd.tick(env);
    }

    fn draw(&self, c: Context, g: &mut G2d, _: &Env) {
        let draw_state = DrawState::default();
        let x = BAR_PADDING;
        let y = BAR_PADDING;

        if !self.gcd.is_expired() {
            rectangle([1.0, 1.0, 1.0, 0.1], [
                x,
                y,
                CAST_BAR_WIDTH * (1.0
                                  - self.gcd.remaining() / self.gcd.duration),
                CAST_BAR_HEIGHT * 0.1,
            ], c.transform, g);
        }
        if let Some((ref spell, progress)) = self.cast_spell {
            rectangle(YELLOW, [
                x,
                y,
                CAST_BAR_WIDTH * (progress / spell.cast_time),
                CAST_BAR_HEIGHT,
            ], c.transform, g);
        }

        let y = y + CAST_BAR_HEIGHT + BAR_PADDING;
        rectangle(BLUE, [
            x,
            y,
            MANA_BAR_WIDTH * self.mana / self.max_mana,
            MANA_BAR_HEIGHT,
        ], c.transform, g);
        Rectangle::new_border(WHITE, 1.0)
            .draw([x, y, MANA_BAR_WIDTH, MANA_BAR_HEIGHT],
                  &draw_state, c.transform, g);
    }

    fn is_casting(&self) -> bool {
        self.cast_spell.is_some()
    }

    fn cast(&mut self, queue: &mut Vec<Action>, spell: Spell) {
        if self.is_casting() || !self.gcd.is_expired() {
            return;
        }
        if spell.mana_cost <= self.mana {
            if spell.cast_time == 0.0 {
                spell.queue_action(queue);
            } else {
                self.cast_spell = Some((spell, 0.0));
            }
            self.gcd.reset(1.5);
        }
    }

    fn stop_casting(&mut self) {
        self.cast_spell = None;
        self.gcd.reset(0.0);
    }

    fn add_mana(&mut self, amount: f64) {
        clamped_add_assign(&mut self.mana, amount, 0.0, self.max_mana);
    }
}

#[derive(Clone, Debug)]
struct State {
    player: Player,
    units: Vec<Unit>,
    boss_swing: Timer,
    game_over: bool,
}

fn test_units() -> Vec<Unit> {
    let mut units = vec![
        Unit {
            health: 150.0,
            max_health: 150.0,
            armor: 50.0,
            .. Default::default()
        },
    ];
    units.extend(vec![Default::default(); 9]);
    units
}
impl Default for State {
    fn default() -> Self {
        Self {
            player: Player {
                mana: 100.0,
                max_mana: 100.0,
                mana_regen: 0.2,
                gcd: Timer::from(1.5),
                cast_spell: Default::default(),
            },
            units: test_units(),
            boss_swing: Timer::from(1.5),
            game_over: false,
        }
    }
}

impl State {
    fn game_over(&mut self) {
        self.game_over = true;
        self.player.mana = 0.0;
        self.player.mana_regen = 0.0;
        self.player.cast_spell = None;
    }
}

fn draw(c: Context, g: &mut G2d, env: &Env,
        state: &State, unit_bars: &[UnitBar]) {
    clear([0.0, 0.0, 0.0, 1.0], g);
    state.player.draw(c, g, env);
    for unit_bar in unit_bars {
        unit_bar.draw(c, g, env, &state.units);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Layer {
    UnitBar,
}

#[derive(Clone, Copy, Debug)]
enum HitboxId {
    UnitBar(UnitId),
}

#[derive(Clone, Debug)]
struct Hitbox {
    id: HitboxId,
    rect: [f64; 4],
    layer: Layer,
}

const YELLOW: Color = [0.7, 0.6, 0.0, 1.0];
const BLUE: Color = [0.0, 0.2, 0.9, 1.0];

const BAR_PADDING: f64 = 20.0;
const MANA_BAR_WIDTH: f64 = 400.0;
const MANA_BAR_HEIGHT: f64 = 40.0;
const CAST_BAR_WIDTH: f64 = 400.0;
const CAST_BAR_HEIGHT: f64 = 40.0;
const UNIT_BAR_WIDTH: f64 = 400.0;
const UNIT_BAR_HEIGHT: f64 = 100.0;

#[derive(Clone, Debug)]
struct UnitBar {
    unit_id: UnitId,
    rect: [f64; 4],
}

impl UnitBar {
    fn draw(&self, c: Context, g: &mut G2d, env: &Env, units: &[Unit]) {
        const GREEN: Color = [0.0, 0.8, 0.0, 1.0];
        const LIGHT_GREEN: Color = [0.3, 1.0, 0.4, 1.0];
        const GREY: Color = [0.2, 0.2, 0.2, 1.0];
        let draw_state = DrawState::default();

        let unit = &units[self.unit_id];

        let (color, ratio) = if unit.is_alive() {
            let color = match env.hitbox_id {
                Some(HitboxId::UnitBar(id)) if id == self.unit_id => {
                    LIGHT_GREEN
                }
                _ => { GREEN }
            };
            (color, unit.health / unit.max_health)
        } else {
            (GREY, 1.0)
        };
        rectangle(color, [
            self.rect[0],
            self.rect[1],
            self.rect[2] * ratio,
            self.rect[3],
        ], c.transform, g);

        Rectangle::new_border(WHITE, 1.0)
            .draw(self.rect, &draw_state, c.transform, g);

        for (i, buff) in unit.buffs.iter().enumerate() {
            let ratio = if let Some(timer) = buff.timer {
                timer.remaining() / timer.duration
            } else {
                1.0
            };
            CircleArc::new([0.9, 0.0, 0.8, 1.0],
                           5.0,
                           0.0,
                           2.0 * PI * ratio)
                .draw([
                    self.rect[0] + self.rect[2] - 10.0 - (i + 1) as f64 * 20.0,
                    self.rect[1] + 10.0,
                    20.0,
                    20.0,
                ], &draw_state, c.transform, g);
        }
    }

    fn hitbox(&self) -> Hitbox {
        Hitbox {
            id: HitboxId::UnitBar(self.unit_id),
            rect: self.rect,
            layer: Layer::UnitBar,
        }
    }
}

fn get_current_hitbox(hitboxes: &[Hitbox], mouse_pos: [f64; 2])
                      -> Option<HitboxId> {
    let mut found = None;
    for hitbox in hitboxes {
        if
            mouse_pos[0] >= hitbox.rect[0] &&
            mouse_pos[1] >= hitbox.rect[1] &&
            mouse_pos[0] < hitbox.rect[0] + hitbox.rect[2] &&
            mouse_pos[1] < hitbox.rect[1] + hitbox.rect[3]
        {
            match found {
                Some((_, layer)) if layer > hitbox.layer => {
                    break;
                }
                _ => {}
            }
            found = Some((hitbox.id, hitbox.layer));
        }
    }
    found.map(|(id, _)| id)
}

fn handle_input<E>(env: &Env,
                   queue: &mut Vec<Action>,
                   state: &mut State,
                   e: &E)
    where E: PressEvent + ReleaseEvent
{
    if let Some(button) = e.press_args() {
        match button {
            Button::Keyboard(Key::Escape) => {
                state.player.stop_casting();
            }
            Button::Mouse(MouseButton::Left) => {
                if let Some(unit_id) = env.selected() {
                    // Fast Heal
                    state.player.cast(queue, Spell {
                        cast_time: 1.5,
                        mana_cost: 3.0,
                        action: Action::AddHealth {
                            target: Target::Unit(unit_id),
                            amount: 30.0,
                        },
                    });
                }
            }
            Button::Mouse(MouseButton::Right) => {
                if let Some(unit_id) = env.selected() {
                    // Revitalize
                    state.player.cast(queue, Spell {
                        cast_time: 0.0,
                        mana_cost: 0.8,
                        action: Action::AddBuff {
                            target: Target::Unit(unit_id),
                            buff: Buff {
                                timer: Some(Timer::from(12.0)),
                                class: Some(Class::Revitalize),
                                effect: Effect::HealthRegen {
                                    rate: 2.0,
                                },
                            },
                        },
                    });
                }
            }
            Button::Mouse(MouseButton::Middle) => {
                if let Some(unit_id) = env.selected() {
                    // Healing Prayer
                    state.player.cast(queue, Spell {
                        cast_time: 2.5,
                        mana_cost: 4.0,
                        action: Action::AddHealth {
                            target: Target::LowestHealth(unit_id, 4),
                            amount: 10.0,
                        },
                    });
                }
            }
            _ => {}
        }
    }
    // if let Some(button) = e.release_args() {
    // }
}

fn encounter<R: rand::Rng>(env: &Env, state: &mut State, rng: &mut R) {
    for unit in &mut state.units {
        unit.add_health(-1.0 * env.time_delta);
    }
    let swing_now = state.boss_swing.tick(&env).is_err();
    if swing_now {
        state.boss_swing.reset(rng.gen_range(1.5, 2.0));
        for unit in &mut state.units {
            if unit.is_alive() {
                unit.damage(rng.gen_range(40.0, 70.0));
                break;
            }
        }
    }
}

fn main() {
    let num_units_per_col = 5;

    let mut window: PistonWindow =
        WindowSettings::new("healsim", [640, 480])
        .build().unwrap();
    let mut rng = rand::thread_rng();

    // game state
    let mut prev_time = std::time::Instant::now();
    let mut env = Env {
        time_delta: 0.0,
        mouse_pos: [0.0, 0.0],
        hitbox_id: None,
    };
    let mut queue = Vec::new();
    let mut hitboxes = Vec::new();
    let mut state = State::default();

    let unit_bars: Vec<_> = (0 .. state.units.len()).map(
        |unit_id| {
            let x = BAR_PADDING;
            let y = CAST_BAR_HEIGHT + MANA_BAR_HEIGHT + BAR_PADDING * 3.0;
            UnitBar {
                unit_id,
                rect: [
                    x + ((unit_id / num_units_per_col) as f64
                         * (UNIT_BAR_WIDTH + BAR_PADDING)),
                    y + ((unit_id % num_units_per_col) as f64
                         * (UNIT_BAR_HEIGHT + BAR_PADDING)),
                    UNIT_BAR_WIDTH,
                    UNIT_BAR_HEIGHT,
                ]
            }
        }).collect();

    for unit_bar in &unit_bars {
        hitboxes.push(unit_bar.hitbox())
    }

    while let Some(e) = window.next() {
        let now = std::time::Instant::now();
        env.time_delta = duration_to_f64(now - prev_time);
        prev_time = now;
        if let Some(pos) = e.mouse_cursor_args() {
            env.mouse_pos = pos;
        }
        env.hitbox_id = get_current_hitbox(&hitboxes, env.mouse_pos);

        if !state.game_over {
            state.player.tick(&env, &mut queue);
            for (unit_id, unit) in state.units.iter_mut().enumerate() {
                unit.tick(&env, unit_id, &mut queue);
            }
            handle_input(&env, &mut queue, &mut state, &e);
            for action in queue.drain(..) {
                action.act(&env, &mut state);
            }
            encounter(&env, &mut state, &mut rng);
            if !state.units.iter().any(|unit| unit.is_alive()) {
                state.game_over();
            }
        }

        window.draw_2d(&e, |c, g| draw(c, g, &env, &state, &unit_bars));
    }
}
