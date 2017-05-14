#![feature(binary_heap_peek_mut_pop)]

extern crate float_ord;
extern crate piston_window;
extern crate rand;
extern crate vec_arena;

pub mod timing;

use std::collections::{btree_map, BTreeMap};
use std::f64::consts::PI;
use std::f64::{EPSILON, INFINITY};
use std::ops::{Add, Index};
use std::sync::Arc;
use float_ord::FloatOrd;
use piston_window::*;
use piston_window::types::Color;
use piston_window::color::WHITE;
use timing::*;
use vec_arena::Arena;

fn clamped_add_assign<T>(lhs: &mut T, rhs: T, min: T, max: T)
    where T: PartialOrd,
          for<'a> &'a T: Add<Output=T>
{
    let sum = &*lhs + &rhs;
    if sum > max {
        *lhs = max;
    } else if sum < min {
        *lhs = min;
    } else {
        *lhs = sum;
    }
}

fn vec_remove_item<T: Eq>(vec: &mut Vec<T>, item: &T) {
    let i = vec.iter().position(|x| x == item).unwrap();
    vec.remove(i);
}

#[derive(Clone, Debug)]
struct Env {
    clock: Clock,
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

#[derive(Clone, Copy, Debug)]
enum Target {
    Single(UnitId),
    LeastHealth(UnitId, usize),
}

impl Target {
    fn for_each<P, G, F>(&self, state: &mut State, priority: G, mut callback: F)
                         -> Result<(), ()>
        where G: Fn(&Unit) -> P,
              F: FnMut(UnitId, &mut State),
              P: Ord,
    {
        match *self {
            Target::Single(unit_id) => {
                if !state.units[unit_id].is_alive() {
                    return Err(());
                }
                callback(unit_id, state);
                Ok(())
            }
            Target::LeastHealth(unit_id, n) => {
                if !state.units[unit_id].is_alive() {
                    return Err(());
                }
                callback(unit_id, state);
                let mut finder: Vec<_> = state.units.iter().enumerate()
                    .filter(|&(id, unit)| unit.is_alive() && id != unit_id)
                    .map(|(id, unit)| {
                        let health_percent = FloatOrd(
                            unit.health / (unit.max_health + EPSILON));
                        ((priority(unit), health_percent), id)
                    }).collect();
                finder.sort_by(|&(ref x, _), &(ref y, _)| {
                    x.partial_cmp(&y).unwrap()
                });
                for &(_, id) in finder.iter().take(n) {
                    callback(id, state)
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Action {
    Boxed(NoDebug<Arc<Fn(&Clock, &mut State) -> Result<(), ()>>>),
    Effect { target: Target, effect: Effect },
}

impl Action {
    fn apply(self, clock: &Clock, state: &mut State) -> Result<(), ()> {
        match self {
            Action::Boxed(NoDebug(action)) => action(clock, state),
            Action::Effect { target, effect } => match effect {
                Effect::UnitEffect(UnitEffect::Buff { ref buff }) => target.for_each(
                    state,
                    |unit| {
                        // prefer refreshing buffs with least time remaining
                        buff.class.group().and_then(|group| {
                            unit.buffs.group(&group).map(|id| {
                                FloatOrd(-unit.buffs.buff_progress(id, clock))
                            })
                        })
                    },
                    |unit_id, state| {
                        effect.clone().apply(unit_id, clock, state);
                    },
                ),
                _ => target.for_each(state, |_| (), |unit_id, state| {
                    effect.clone().apply(unit_id, clock, state);
                }),
            },
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoDebug<T>(T);

impl<T> std::fmt::Debug for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("NoDebug")
    }
}

#[derive(Clone, Debug)]
enum Effect {
    Boxed(NoDebug<Arc<Fn(UnitId, &Clock, &mut State)>>),
    UnitEffect(UnitEffect),
}

impl Effect {
    fn apply(self, unit_id: UnitId, clock: &Clock, state: &mut State) {
        match self {
            Effect::Boxed(NoDebug(effect)) => effect(unit_id, clock, state),
            Effect::UnitEffect(effect) => {
                effect.apply(clock, &mut state.units[unit_id]);
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Spell {
    mana_cost: f64,
    cast_time: f64,
    cooldown: Option<(Class, f64)>,
    action: Action,
}

impl Spell {
    fn cast_action(self, unit_id: UnitId) -> Action {
        Action::Boxed(NoDebug(Arc::new(move |clock, state| {
            self.action.clone().apply(clock, state)?;
            let unit = &mut state.units[unit_id];
            if let Some((class, duration)) = self.cooldown {
                unit.buffs.insert(Buff {
                    class: class,
                    num_ticks: 1,
                    interval: duration,
                    effect: None,
                }, clock);
            }
            unit.add_mana(-self.mana_cost);
            Ok(())
        })))
    }
}

// multiple classes can share the same group
//
// buffs within the same group are mutually exclusive
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Group {
    Of(Class),                         // class is representative of the group
}

// buffs within the same class are considered the "same"
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Class {
    Special,
    SerenityCooldown,
    Revitalize,
}

impl Class {
    fn group(self) -> Option<Group> {
        match self {
            Class::Special => None,
            _ => Some(Group::Of(self)),
        }
    }

    fn visible(self) -> bool {
        match self {
            Class::Revitalize => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
struct Buff {
    class: Class,
    num_ticks: u64,
    interval: f64,
    effect: Option<Effect>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ActiveBuffId(usize);

#[derive(Clone, Debug)]
struct ActiveBuff {
    timer: WatchTimer,
    remaining_ticks: u64,
    buff: Buff,
}

#[derive(Clone, Debug, Default)]
struct Buffs {
    watch: Watch<ActiveBuffId>,
    buffs: Arena<ActiveBuff>,
    buff_groups: BTreeMap<Group, ActiveBuffId>,
    visible_buffs: Vec<ActiveBuffId>,
}

impl Buffs {
    fn update(&mut self, clock: &Clock, unit_id: UnitId,
              queue: &mut Vec<Action>) {
        while let Some((_, id)) = self.watch.poll(clock.time).next() {
            {
                let active_buff = &mut self.buffs[id.0];
                if let Some(ref effect) = active_buff.buff.effect {
                    queue.push(Action::Effect {
                        target: Target::Single(unit_id),
                        effect: effect.clone(),
                    });
                }
                active_buff.remaining_ticks -= 1;
                if active_buff.remaining_ticks > 0 {
                    let when = clock + active_buff.buff.interval;
                    let timer = self.watch.schedule(when, id);
                    active_buff.timer = timer;
                    continue;
                }
            }
            let active_buff = self.buffs.remove(id.0).unwrap();
            if let Some(group) = active_buff.buff.class.group() {
                self.buff_groups.remove(&group);
            }
            if active_buff.buff.class.visible() {
                vec_remove_item(&mut self.visible_buffs, &id);
            }
        }
    }

    fn insert(&mut self, mut buff: Buff, clock: &Clock) -> ActiveBuffId {
        fn raw_insert(buffs: &mut Arena<ActiveBuff>, buff: Buff) -> ActiveBuffId {
            ActiveBuffId(buffs.insert(ActiveBuff {
                timer: WatchTimer(!0),       // dummy
                remaining_ticks: buff.num_ticks,
                buff: buff,
            }))
        }
        let id = match buff.class.group() {
            Some(group) => match self.buff_groups.entry(group) {
                btree_map::Entry::Occupied(entry) => {
                    // refresh buff, allowing up to 30% of ticks to be kept
                    let id = *entry.get();
                    let active_buff = &mut self.buffs[id.0];
                    let visible = active_buff.buff.class.visible();
                    if visible {
                        vec_remove_item(&mut self.visible_buffs, &id);
                    }
                    if active_buff.buff.class == buff.class {
                        if visible {
                            self.visible_buffs.push(id);
                        }
                        clamped_add_assign(&mut active_buff.remaining_ticks,
                                           buff.num_ticks,
                                           0, buff.num_ticks * 13 / 10);
                        buff.num_ticks = active_buff.remaining_ticks;
                        active_buff.buff = buff;
                        return id;
                    } else {
                        self.watch.cancel(active_buff.timer);
                        active_buff.remaining_ticks = buff.num_ticks;
                        active_buff.buff = buff;
                        id
                    }
                },
                btree_map::Entry::Vacant(entry) => {
                    let id = raw_insert(&mut self.buffs, buff);
                    entry.insert(id);
                    id
                }
            },
            None => raw_insert(&mut self.buffs, buff),
        };
        let active_buff = &mut self.buffs[id.0];
        if active_buff.buff.class.visible() {
            self.visible_buffs.push(id);
        }
        let timer = self.watch.schedule(clock + active_buff.buff.interval, id);
        active_buff.timer = timer;
        id
    }

    fn remove(&mut self, id: ActiveBuffId) {
        let active_buff = match self.buffs.get(id.0) {
            None => return,
            Some(active_buff) => active_buff,
        };
        self.watch.cancel(active_buff.timer);
        if active_buff.buff.class.visible() {
            vec_remove_item(&mut self.visible_buffs, &id);
        }
        if let Some(group) = active_buff.buff.class.group() {
            self.buff_groups.remove(&group);
        }
    }

    fn group(&self, group: &Group) -> Option<ActiveBuffId> {
        self.buff_groups.get(group).cloned()
    }

    fn buff_progress(&self, id: ActiveBuffId, clock: &Clock) -> f64 {
        let active_buff = &self[id];
        let buff = &active_buff.buff;
        let duration = buff.interval * buff.num_ticks as f64;
        if duration == INFINITY {
            return 0.0;
        }
        let when = self.watch[active_buff.timer].0;
        let remaining =
            buff.interval * (active_buff.remaining_ticks - 1) as f64
            + duration_to_f64(when - clock.time);
        1.0 - remaining / duration
    }

    fn start_unit_timer<P, F>(unit: &mut Unit, property: P,
                              duration: f64, clock: &Clock, callback: F)
        where P: Fn(&mut Unit) -> (&mut Buffs, &mut Option<ActiveBuffId>)
                 + 'static,
              F: Fn(UnitId, &Clock, &mut State) + 'static,
    {
        let (buffs, timer) = property(unit);
        *timer = Some(buffs.insert(Buff {
            class: Class::Special,
            num_ticks: 1,
            interval: duration,
            effect: Some(Effect::Boxed(NoDebug(Arc::new(move |unit_id, clock, state| {
                *property(&mut state.units[unit_id]).1 = None;
                callback(unit_id, clock, state);
            })))),
        }, clock))
    }

    fn reset_unit_timer(&mut self, timer: &mut Option<ActiveBuffId>) {
        timer.map(|timer| self.remove(timer));
    }
}

impl Index<ActiveBuffId> for Buffs {
    type Output = ActiveBuff;
    fn index(&self, id: ActiveBuffId) -> &Self::Output {
        &self.buffs[id.0]
    }
}

type UnitId = usize;

#[derive(Clone, Debug)]
struct Unit {
    health: f64,
    max_health: f64,
    armor: f64,
    mana: f64,
    max_mana: f64,
    mana_regen: f64,
    gcd: f64,
    buffs: Buffs,
    global_cooldown: Option<ActiveBuffId>,
    casting: Option<ActiveBuffId>,
}

impl Default for Unit {
    fn default() -> Self {
        Self {
            health: 100.0,
            max_health: 100.0,
            armor: Default::default(),
            mana: Default::default(),
            max_mana: Default::default(),
            mana_regen: Default::default(),
            gcd: 1.5,
            buffs: Default::default(),
            global_cooldown: Default::default(),
            casting: Default::default(),
        }
    }
}

impl Unit {
    fn update(&mut self, env: &Env, unit_id: UnitId, queue: &mut Vec<Action>) {
        let mana_regen = self.mana_regen;
        self.add_mana(mana_regen * env.clock.time_delta);
        self.buffs.update(&env.clock, unit_id, queue);
    }

    fn is_alive(&self) -> bool {
        self.max_health != 0.0
    }

    fn kill(&mut self) {
        self.health = 0.0;
        self.max_health = 0.0;
        self.mana = 0.0;
        self.max_mana = 0.0;
        self.buffs = Default::default();
        self.global_cooldown = None;
        self.casting = None;
    }

    fn add_health(&mut self, amount: f64) {
        let max = self.max_health;
        clamped_add_assign(&mut self.health, amount, 0.0, max);
        if self.health == 0.0 {
            self.kill();
        }
    }

    fn add_mana(&mut self, amount: f64) {
        clamped_add_assign(&mut self.mana, amount, 0.0, self.max_mana);
    }

    fn is_casting(&self) -> bool {
        self.casting.is_some()
    }

    fn cast(&mut self, spell: Spell, clock: &Clock) {
        if self.is_casting()
            || self.global_cooldown.is_some()
            || !self.can_cast(&spell)
        {
            return;
        }
        Buffs::start_unit_timer(
            self,
            |unit| (&mut unit.buffs, &mut unit.casting),
            spell.cast_time,
            clock,
            move |unit_id, clock, state| {
                if state.units[unit_id].can_cast(&spell) {
                    // FIXME redundant?
                    let _ = spell.clone().cast_action(unit_id).apply(clock, state);
                }
            });
        let gcd = self.gcd;
        Buffs::start_unit_timer(
            self,
            |unit| (&mut unit.buffs, &mut unit.global_cooldown),
            gcd,
            clock,
            |_, _, _| ());
    }

    fn can_cast(&self, spell: &Spell) -> bool {
        if spell.mana_cost > self.mana {
            return false;
        }
        if let Some((cooldown, _)) = spell.cooldown {
            if let Some(group) = cooldown.group() {
                if self.buffs.group(&group).is_some() {
                    return false;
                }
            }
        }
        true
    }

    fn stop_casting(&mut self) {
        if !self.is_casting() {
            return;
        }
        self.buffs.reset_unit_timer(&mut self.casting);
        self.buffs.reset_unit_timer(&mut self.global_cooldown);
    }
}

#[derive(Clone, Debug)]
enum UnitEffect {
    Heal { amount: f64 },
    Damage { amount: f64 },
//    AddMana { amount: f64 },
    Buff { buff: Box<Buff> },
}

impl UnitEffect {
    fn apply(self, clock: &Clock, unit: &mut Unit) {
        match self {
            UnitEffect::Heal { amount } => {
                if amount > 0.0 {
                    unit.add_health(amount);
                }
            }
            UnitEffect::Damage { amount } => {
                let damage = amount * (-unit.armor).exp();
                if damage > 0.0 {
                    unit.add_health(-damage);
                }
            }
            // UnitEffect::AddMana { amount } => {
            //     unit.add_mana(amount);
            // }
            UnitEffect::Buff { buff } => {
                let buff = *buff;
                unit.buffs.insert(buff, clock);
            }
        }
    }
}

fn test_player() -> Unit {
    Unit {
        health: 100.0,
        max_health: 100.0,
        armor: 0.0,
        mana: 100.0,
        max_mana: 100.0,
        mana_regen: 0.5,
        gcd: 1.0,
        .. Default::default()
    }
}

fn draw_player(player: &Unit, env: &Env, c: Context, g: &mut G2d) {
    let draw_state = DrawState::default();
    let x = BAR_PADDING;
    let y = BAR_PADDING;

    if let Some(id) = player.global_cooldown {
        let progress = player.buffs.buff_progress(id, &env.clock);
        rectangle([1.0, 1.0, 1.0, 0.1], [
            x,
            y,
            CAST_BAR_WIDTH * progress,
            CAST_BAR_HEIGHT * 0.1,
        ], c.transform, g);
    }
    if let Some(id) = player.casting {
        let progress = player.buffs.buff_progress(id, &env.clock);
        rectangle(YELLOW, [
            x,
            y,
            CAST_BAR_WIDTH * progress,
            CAST_BAR_HEIGHT,
        ], c.transform, g);
    }

    let y = y + CAST_BAR_HEIGHT + BAR_PADDING;
    rectangle(BLUE, [
        x,
        y,
        MANA_BAR_WIDTH * player.mana / player.max_mana,
        MANA_BAR_HEIGHT,
    ], c.transform, g);
    Rectangle::new_border(WHITE, 1.0)
        .draw([x, y, MANA_BAR_WIDTH, MANA_BAR_HEIGHT],
              &draw_state, c.transform, g);
}

#[derive(Clone, Debug)]
struct State {
    player_unit_id: UnitId,
    units: Vec<Unit>,
    boss_multiplier: f64,
    boss_aura: Timer,
    boss_swing: Timer,
    boss_eruption: Timer,
}

fn test_units() -> (UnitId, Vec<Unit>) {
    let mut units = vec![
        Unit {
            health: 150.0,
            max_health: 150.0,
            armor: 1.0,
            .. Default::default()
        },
    ];
    units.extend(vec![Default::default(); 8]);
    let player_unit_id = units.len();
    units.push(test_player());
    (player_unit_id, units)
}

fn test_state() -> State {
    let (player_unit_id, units) = test_units();
    State {
        player_unit_id,
        units,
        boss_multiplier: 1.0,
        boss_aura: Timer::from(2.0),
        boss_swing: Timer::from(2.0),
        boss_eruption: Timer::from(5.0),
    }
}

impl State {
    fn player(&self) -> &Unit {
        &self.units[self.player_unit_id]
    }

    fn player_mut(&mut self) -> &mut Unit {
        &mut self.units[self.player_unit_id]
    }
}

fn draw(c: Context, g: &mut G2d, env: &Env,
        state: &State, unit_bars: &[UnitBar]) {
    clear([0.0, 0.0, 0.0, 1.0], g);
    draw_player(state.player(), env, c, g);
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

const BAR_PADDING: f64 = 10.0;
const MANA_BAR_WIDTH: f64 = 300.0;
const MANA_BAR_HEIGHT: f64 = 20.0;
const CAST_BAR_WIDTH: f64 = 300.0;
const CAST_BAR_HEIGHT: f64 = 20.0;
const UNIT_BAR_WIDTH: f64 = 300.0;
const UNIT_BAR_HEIGHT: f64 = 40.0;

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

        // draw buffs
        for (i, &id) in unit.buffs.visible_buffs.iter().enumerate() {
            let progress = unit.buffs.buff_progress(id, &env.clock);
            CircleArc::new([0.9, 0.0, 0.8, 1.0],
                           5.0,
                           2.0 * PI * (progress - 0.25),
                           2.0 * PI * -0.25)
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
                   state: &mut State,
                   e: &E)
    where E: PressEvent + ReleaseEvent
{
    let clock = &env.clock;
    if let Some(button) = e.press_args() {
        match button {
            Button::Keyboard(Key::Escape) => {
                state.player_mut().stop_casting();
            }
            Button::Mouse(MouseButton::Left) => {
                if let Some(unit_id) = env.selected() {
                    // Serenity
                    state.player_mut().cast(Spell {
                        cast_time: 0.0,
                        mana_cost: 0.8,
                        cooldown: Some((Class::SerenityCooldown, 6.0)),
                        action: Action::Effect {
                            target: Target::LeastHealth(unit_id, 3),
                            effect: Effect::UnitEffect(UnitEffect::Heal { amount: 75.0 }),
                        },
                    }, clock);
                }
            }
            Button::Mouse(MouseButton::Right) => {
                if let Some(unit_id) = env.selected() {
                    // Revitalize
                    state.player_mut().cast(Spell {
                        cast_time: 0.0,
                        mana_cost: 0.3,
                        cooldown: None,
                        action: Action::Effect {
                            target: Target::LeastHealth(unit_id, 1),
                            effect: Effect::UnitEffect(UnitEffect::Buff {
                                buff: Box::new(Buff {
                                    class: Class::Revitalize,
                                    num_ticks: 4,
                                    interval: 3.0,
                                    effect: Some(Effect::UnitEffect(
                                        UnitEffect::Heal {
                                            amount: 8.0,
                                        }
                                    )),
                                }),
                            }),
                        },
                    }, clock);
                }
            }
            Button::Mouse(MouseButton::Middle) => {
                if let Some(unit_id) = env.selected() {
                    // Healing Prayer
                    state.player_mut().cast(Spell {
                        cast_time: 1.0,
                        mana_cost: 3.0,
                        cooldown: None,
                        action: Action::Effect {
                            target: Target::LeastHealth(unit_id, 5),
                            effect: Effect::UnitEffect(UnitEffect::Heal { amount: 30.0 }),
                        },
                    }, clock);
                }
            }
            _ => {}
        }
    }
    // if let Some(button) = e.release_args() {
    // }
}

fn encounter<R: rand::Rng>(env: &Env, state: &mut State, rng: &mut R) {
    // damaging aura
    if state.boss_aura.tick(&env.clock).is_err() {
        state.boss_aura.reset(2.0);
        for unit in &mut state.units {
            if unit.is_alive() {
                UnitEffect::Damage {
                    amount: rng.gen_range(4.0, 6.5) * state.boss_multiplier,
                }.apply(&env.clock, unit);
            }
        }
    }

    // boss attacks
    if state.boss_swing.tick(&env.clock).is_err(){
        state.boss_swing.reset(rng.gen_range(1.4, 1.6) / state.boss_multiplier);
        for unit in &mut state.units {
            if unit.is_alive() {
                UnitEffect::Damage {
                    amount: rng.gen_range(60.0, 65.0) * state.boss_multiplier,
                }.apply(&env.clock, unit);
                break;
            }
        }
    }

    if state.boss_eruption.tick(&env.clock).is_err() {
        state.boss_eruption.reset(rng.gen_range(5.5, 6.5) / state.boss_multiplier);
        state.boss_multiplier += 0.0;
        for unit in &mut state.units {
            if unit.is_alive() {
                UnitEffect::Damage {
                    amount: rng.gen_range(24.0, 29.0) * state.boss_multiplier,
                }.apply(&env.clock, unit);
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
    let mut env = Env {
        clock: Clock::new(),
        mouse_pos: [0.0, 0.0],
        hitbox_id: None,
    };
    let mut queue = Vec::new();
    let mut hitboxes = Vec::new();
    let mut state = test_state();

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

        window.draw_2d(&e, |c, g| draw(c, g, &env, &state, &unit_bars));

        env.clock.update();
        if let Some(pos) = e.mouse_cursor_args() {
            env.mouse_pos = pos;
        }
        env.hitbox_id = get_current_hitbox(&hitboxes, env.mouse_pos);

        for (unit_id, unit) in state.units.iter_mut().enumerate() {
            unit.update(&env, unit_id, &mut queue);
        }
        handle_input(&env, &mut state, &e);
        for action in queue.drain(..) {
            let _ = action.apply(&env.clock, &mut state);
        }
        encounter(&env, &mut state, &mut rng);
    }
}
