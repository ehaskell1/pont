use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;

use std::collections::HashMap;
use std::sync::Mutex;
use web_sys::{
    AddEventListenerOptions,
    Blob,
    Element,
    Event,
    EventTarget,
    FileReader,
    Document,
    KeyboardEvent,
    HtmlButtonElement,
    HtmlElement,
    HtmlInputElement,
    MessageEvent,
    MouseEvent,
    PointerEvent,
    ProgressEvent,
    SvgGraphicsElement,
    WebSocket,
};

use pont_common::{ClientMessage, ServerMessage, Side, Game, Position, Move, PositionState,
man_in_bounds, WIDTH, HEIGHT};

// Minimal logging macro
macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format!($($t)*).into()))
}

type JsResult<T> = Result<T, JsValue>;
type JsError = Result<(), JsValue>;
type JsClosure<T> = Closure<dyn FnMut(T) -> JsError>;

trait DocExt {
    fn create_svg_element(&self, t: &str) -> JsResult<Element>;
}

impl DocExt for Document {
    fn create_svg_element(&self, t: &str) -> JsResult<Element> {
        self.create_element_ns(Some("http://www.w3.org/2000/svg"), t)
    }
}

fn get_time_ms() -> f64 {
    web_sys::window()
        .expect("No global window found")
        .performance()
        .expect("No performance object found")
        .now()
}

////////////////////////////////////////////////////////////////////////////////

macro_rules! methods {
    ($($sub:ident => [$($name:ident($($var:ident: $type:ty),*)),+ $(,)?]),+
       $(,)?) =>
    {
        $($(
        fn $name(&mut self, $($var: $type),* ) -> JsError {
            match self {
                State::$sub(s) => s.$name($($var),*),
                _ => panic!("Invalid state transition"),
            }
        }
        )+)+
    }
}

macro_rules! transitions {
    ($($sub:ident => [$($name:ident($($var:ident: $type:ty),*)
                        -> $into:ident),+ $(,)?]),+$(,)?) =>
    {
        $($(
        fn $name(&mut self, $($var: $type),* ) -> JsError {
            let s = std::mem::replace(self, State::Empty);
            match s {
                State::$sub(s) => *self = State::$into(s.$name($($var),*)?),
                _ => panic!("Invalid state"),
            }
            Ok(())
        }
        )+)+
    }
}

////////////////////////////////////////////////////////////////////////////////

type Pos = (f32, f32);
#[derive(PartialEq)]
struct Dragging {
    target: Element,
    shadow: Element,
    offset: Pos,
}

#[derive(PartialEq)]
struct Panning {
    target: Element,
    pos: Pos,
    pointer_id: i32,
}

#[derive(PartialEq)]
struct TileAnimation {
    target: Element,
    start: Pos,
    end: Pos,
    t0: f64,
}

impl TileAnimation {
    // Returns true if the animation should keep running
    fn run(&self, t: f64) -> JsResult<bool> {
        let anim_length = 100.0;
        let mut frac = ((t - self.t0) / anim_length) as f32;
        if frac > 1.0 {
            frac = 1.0;
        }
        let x = self.start.0 * (1.0 - frac) + self.end.0 * frac;
        let y = self.start.1 * (1.0 - frac) + self.end.1 * frac;
        self.target.set_attribute("transform", &format!("translate({} {})",
                                                        x, y))?;
        Ok(frac < 1.0)
    }
}

impl JumpBall {
    // Returns true if the animation should keep running
    fn run(&self, t: f64) -> JsResult<bool> {
        let anim_length = 300.0;
        let frac = ((t - self.t0) / anim_length) as f32;
        let q = frac as usize;
        if q >= self.points.len() - 1 {
            let last = *self.points.last().unwrap();
            self.target.set_attribute("transform", &format!("translate({} {})", last.0, last.1))?;
            Ok(false)
        } else {
            let start = self.points[q];
            let end = self.points[q + 1];
            let rem = frac - q as f32;
            let x = start.0 * (1.0 - rem) + end.0 * rem;
            let y = start.1 * (1.0 - rem) + end.1 * rem;
            self.target.set_attribute("transform", &format!("translate({} {})",
                                                            x, y))?;
            for i in 0..q {
                for man in &self.men[i] {
                    man.first_child()
                        .unwrap()
                        .dyn_into::<Element>()?
                        .set_attribute("r", &format!("{}", 0.0))?;
                }
            }
            for man in &self.men[q] {
                man.first_child()
                    .unwrap()
                    .dyn_into::<Element>()?
                    .set_attribute("r", &format!("{}", 4.0 * (1.0 - rem)))?;
            }
            Ok(true)
        }
    }
}

impl PlaceMan {
    // Returns true if the animation should keep running
    fn run(&self, t: f64) -> JsResult<bool> {
        let anim_length = 300.0;
        let mut frac = ((t - self.t0) / anim_length) as f32;
        if frac > 1.0 {
            frac = 1.0;
        }
        self.target
            .first_child()
            .unwrap()
            .dyn_into::<Element>()?
            .set_attribute("r", &format!("{}", frac * 4.0))?;
        Ok(frac < 1.0)
    }
}

#[derive(PartialEq)]
struct DropBall {
    anim: TileAnimation,
    shadow: Element,
    remove_men: Vec<Position>,
}

#[derive(PartialEq)]
struct ReturnBall(TileAnimation);

#[derive(PartialEq)]
struct JumpBall {
    target: Element,
    points: Vec<Pos>,
    men: Vec<Vec<Element>>,
    t0: f64,
}

#[derive(PartialEq)]
struct UndoBall {
    anim: TileAnimation,
    add_men: Vec<(Position, Pos)>,
}

#[derive(PartialEq)]
struct PlaceMan {
    target: Element,
    t0: f64,
}

#[derive(PartialEq)]
enum DragAnim {
    DropBall(DropBall),
    ReturnBall(ReturnBall),
    JumpBall(JumpBall),
    UndoBall(UndoBall),
    PlaceMan(PlaceMan),
}

#[derive(PartialEq)]
enum BoardState {
    Idle,
    Dragging(Dragging),
    Animation(DragAnim),
}

enum DropTarget {
    DropBall(Position),
    ReturnBall,
}

pub struct Board {
    doc: Document,
    svg: SvgGraphicsElement,
    svg_div: Element,
    man_shadow: Element,

    state: BoardState,

    pieces_group: Element,

    mov: Option<Move>,
    grid: HashMap<Position, Element>,
    game_states: Vec<Game>,
    side: Side,

    accept_button: HtmlButtonElement,
    undo_button: HtmlButtonElement,

    pointer_down_cb: JsClosure<PointerEvent>,
    pointer_move_cb: JsClosure<PointerEvent>,
    pointer_up_cb: JsClosure<PointerEvent>,
    touch_start_cb: JsClosure<Event>,

    anim_cb: JsClosure<f64>,
}

impl Board {
    fn new(doc: &Document, game: Game, side: Side)
        -> JsResult<Board>
    {
        let board_rect = doc.get_element_by_id("board_rect")
            .expect("Could not find board_rect");
        set_event_cb(&board_rect, "pointerup", move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_board_click(evt)
        }).forget();
        set_event_cb(&board_rect, "pointermove", move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_board_hover(evt)
        }).forget();

        let accept_button = doc.get_element_by_id("accept_button")
            .expect("Could not find accept_button")
            .dyn_into()?;
        set_event_cb(&accept_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_accept_button(evt)
        }).forget();

        let undo_button = doc.get_element_by_id("undo_button")
            .expect("Could not find undo_button")
            .dyn_into()?;
        set_event_cb(&undo_button, "click", move |evt: Event| {
            HANDLE.lock().unwrap()
                .on_undo_button(evt)
        }).forget();

        let pointer_down_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_down(evt)
        });
        let pointer_move_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_move(evt)
        });
        let pointer_up_cb = build_cb(move |evt: PointerEvent| {
            HANDLE.lock().unwrap()
                .on_pointer_up(evt)
        });
        let anim_cb = build_cb(move |evt: f64| {
            HANDLE.lock().unwrap()
                .on_anim(evt)
        });
        let touch_start_cb = build_cb(move |evt: Event| {
            evt.prevent_default();
            Ok(())
        });

        let svg = doc.get_element_by_id("game_svg")
            .expect("Could not find game svg")
            .dyn_into()?;
        let svg_div = doc.get_element_by_id("svg_div")
            .expect("Could not find svg div");
        let pieces_group = doc.get_element_by_id("pieces_group")
            .expect("Could not find pieces_group");
        let grid_lines = doc.get_element_by_id("grid_lines")
            .expect("Could nto find grid_lines");

        let man_shadow = doc.create_svg_element("circle")?;
        man_shadow.class_list().add_1("shadow")?;
        man_shadow.class_list().add_1("man")?;
        man_shadow.set_attribute("r", "4.0")?;
        man_shadow.set_attribute("cx", "5.0")?;
        man_shadow.set_attribute("cy", "5.0")?;
        man_shadow.set_attribute("visibility", "hidden")?;
        pieces_group.append_child(&man_shadow)?;

        let g = doc.create_svg_element("rect")?;
        g.set_attribute("width", "150")?;
        g.set_attribute("height", "210")?;
        g.set_attribute("x", "0")?;
        g.set_attribute("y", "0")?;
        g.class_list().add_1("boundary")?;
        grid_lines.append_child(&g)?;

        let g = doc.create_svg_element("rect")?;
        g.set_attribute("width", "140")?;
        g.set_attribute("height", "200")?;
        g.set_attribute("x", "5")?;
        g.set_attribute("y", "5")?;
        g.class_list().add_1("field")?;
        grid_lines.append_child(&g)?;

        for y in &["5", "195"] {
            let g = doc.create_svg_element("rect")?;
            g.set_attribute("width", "140")?;
            g.set_attribute("height", "10")?;
            g.set_attribute("x", "5")?;
            g.set_attribute("y", y)?;
            g.class_list().add_1("goal")?;
            grid_lines.append_child(&g)?;
        }


        for y in 0..HEIGHT {
            let g = doc.create_svg_element("line")?;
            g.set_attribute("x1", "5")?;
            g.set_attribute("x2", "145")?;
            let y = (5 + y * 10).to_string();
            g.set_attribute("y1", &y)?;
            g.set_attribute("y2", &y)?;
            g.set_attribute("stroke", "black")?;
            g.set_attribute("stroke-width", "0.25px")?;
            grid_lines.append_child(&g)?;
        }
        for x in 0..WIDTH {
            let g = doc.create_svg_element("line")?;
            g.set_attribute("y1", "15")?;
            g.set_attribute("y2", "195")?;
            let x = (5 + x * 10).to_string();
            g.set_attribute("x1", &x)?;
            g.set_attribute("x2", &x)?;
            g.set_attribute("stroke", "black")?;
            g.set_attribute("stroke-width", "0.25px")?;
            grid_lines.append_child(&g)?;
        }

        let mut out = Board {
            doc: doc.clone(),
            state: BoardState::Idle,
            mov: None,
            svg, svg_div,
            pieces_group,
            pointer_down_cb,
            pointer_up_cb,
            pointer_move_cb,
            touch_start_cb,
            anim_cb,
            accept_button,
            undo_button,
            game_states: vec![game],
            grid: HashMap::new(),
            man_shadow,
            side,
        };

        let ball = out.new_ball()?;
        out.pieces_group.append_child(&ball)?;
        let (x, y) = out.grid_position(out.game_states[0].ball);
        ball.set_attribute("transform",
                          &format!("translate({} {})", x, y))?;

        out.grid.insert(out.game_states[0].ball, ball);

        for x in 0..WIDTH {
            for y in 0..HEIGHT {
                if let PositionState::Man = out.game_states[0].board[x as usize][y as usize] {
                    let man = out.new_man(1.0)?;
                    out.pieces_group.append_child(&man)?;
                    man.class_list().add_1("placed")?;
                    let grid_position = out.grid_position((x, y));
                    man.set_attribute("transform",
                                      &format!("translate({} {})", grid_position.0, grid_position.1))?;
                    out.grid.insert((x, y), man);
                }
            }
        }

        Ok(out)
    }

    fn new_game(&mut self, game: Game, side: Side) -> JsError {
        for piece in self.grid.values() {
            self.pieces_group.remove_child(&piece)?;
        }
        self.grid = HashMap::new();
        self.game_states = vec![game];
        self.side = side;

        let ball = self.new_ball()?;
        self.pieces_group.append_child(&ball)?;
        let (x, y) = self.grid_position(self.game_states[0].ball);
        ball.set_attribute("transform",
                           &format!("translate({} {})", x, y))?;

        self.grid.insert(self.game_states[0].ball, ball);

        for x in 0..WIDTH {
            for y in 0..HEIGHT {
                if let PositionState::Man = self.game_states[0].board[x as usize][y as usize] {
                    let man = self.new_man(1.0)?;
                    self.pieces_group.append_child(&man)?;
                    man.class_list().add_1("placed")?;
                    let grid_position = self.grid_position((x, y));
                    man.set_attribute("transform",
                                      &format!("translate({} {})", grid_position.0, grid_position.1))?;
                    self.grid.insert((x, y), man);
                }
            }
        }

        Ok(())
    }

    fn grid_position(&self, (x, y): (u8, u8)) -> Pos {
        if self.side == Side::Bottom {
            (x as f32 * 10.0, y as f32 * 10.0)
        } else {
            ((WIDTH - x - 1) as f32 * 10.0, (HEIGHT - y - 1) as f32 * 10.0)
        }
    }

    fn game_position(&self, (x, y): (f32, f32)) -> Position {
        if self.side == Side::Bottom {
            ((x / 10.0).floor() as u8, (y / 10.0).floor() as u8)
        } else {
            (WIDTH - 1 - (x / 10.0).floor() as u8, HEIGHT - 1 - (y / 10.0).floor() as u8)
        }
    }

    fn set_my_turn(&mut self, is_my_turn: bool) -> JsError {
        if is_my_turn {
            self.svg_div.class_list().remove_1("nyt")?;
        } else {
            self.svg_div.class_list().add_1("nyt")?;
        }
        Ok(())
    }

    fn get_transform(e: &Element) -> Pos {
        let t = e.get_attribute("transform").unwrap();
        let s = t.chars()
            .filter(|&c| c.is_digit(10) || c == ' ' || c == '.' || c == '-')
            .collect::<String>();
        let mut itr = s.split(' ')
            .map(|s| s.parse().unwrap());

        let dx = itr.next().unwrap();
        let dy = itr.next().unwrap();

        (dx, dy)
    }

    fn mouse_pos(&self, evt: &MouseEvent) -> Pos {
        let mat = self.svg.get_screen_ctm().unwrap();
        let x = (evt.client_x() as f32 - mat.e()) / mat.a();
        let y = (evt.client_y() as f32 - mat.f()) / mat.d();
        (x, y)
    }

    fn on_board_click(&mut self, evt: PointerEvent) -> JsError {
        if self.state != BoardState::Idle || self.mov.is_some() {
            return Ok(());
        }
        evt.prevent_default();

        let (mx, my) = self.mouse_pos(evt.as_ref());
        if mx < 0.0 || my < 0.0 {
            return Ok(());
        }
        let game_position = self.game_position((mx, my));
        let (mx, my) = self.grid_position(game_position);
        if !man_in_bounds(game_position) || self.grid.contains_key(&game_position) {
            return Ok(());
        }
        self.hide_man_shadow()?;
        let man = self.new_man(1.0)?;
        self.pieces_group.append_child(&man)?;
        man.set_attribute("transform",
                          &format!("translate({} {})", mx, my))?;
        self.grid.insert(game_position, man);

        self.mov = Some(Move::Man(game_position));
        let mut game = self.game_states.last().unwrap().clone();
        game.place_man(game_position);
        self.game_states.push(game);

        self.accept_button.set_disabled(false);
        self.undo_button.set_disabled(false);


        Ok(())
    }

    fn hide_man_shadow(&self) -> JsError {
        self.man_shadow.set_attribute("visibility", "hidden")
    }

    fn on_board_hover(&mut self, evt: PointerEvent) -> JsError {
        if self.state != BoardState::Idle || self.mov.is_some() {
            self.man_shadow.set_attribute("vsibility", "hidden")?;
            return Ok(());
        }
        evt.prevent_default();

        let (mx, my) = self.mouse_pos(evt.as_ref());
        let game_position = self.game_position((mx, my));
        let (mx, my) = self.grid_position(game_position);

        if !self.grid.contains_key(&game_position) && man_in_bounds(game_position) {
            self.man_shadow.set_attribute(
                "transform", &format!("translate({} {})",
                                      mx, my))?;
            self.man_shadow.set_attribute("visibility", "visible")?;
        } else {
            self.man_shadow.set_attribute("visibility", "hidden")?;
        }
        Ok(())
    }

    fn on_pointer_down(&mut self, evt: PointerEvent) -> JsError {
        // We only drag if nothing else is dragging;
        // no fancy multi-touch dragging here.
        if self.state != BoardState::Idle {
            return Ok(());
        }
        if let Some(Move::Man(_)) = self.mov {
            return Ok(());
        }
        evt.prevent_default();

        self.hide_man_shadow()?;

        let mut target = evt.target()
            .unwrap()
            .dyn_into::<Element>()?;

        // Shadow goes underneath the dragged piece
        let shadow = self.doc.create_svg_element("circle")?;
        shadow.class_list().add_1("shadow")?;
        shadow.class_list().add_1("ball")?;
        shadow.set_attribute("r", "4.0")?;
        shadow.set_attribute("cx", "5.0")?;
        shadow.set_attribute("cy", "5.0")?;
        shadow.set_attribute("visibility", "hidden")?;
        self.pieces_group.append_child(&shadow)?;

        // Walk up the tree to find the piece's <g> group,
        // which sets its position with a translation
        while !target.has_attribute("transform") {
            target = target.parent_node().unwrap().dyn_into::<Element>()?;
        }
        let (mx, my) = self.mouse_pos(evt.as_ref());
        let (tx, ty) = Self::get_transform(&target);
        let game_position = self.game_position((tx, ty));

        self.pieces_group.remove_child(&target)?;
        self.grid.remove(&game_position);

        // Move to the back of the SVG object, so it's on top
        self.svg.append_child(&target)?;

        let mut options = AddEventListenerOptions::new();
        options.passive(false);
        target.set_pointer_capture(evt.pointer_id())?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointermove",
            self.pointer_move_cb.as_ref().unchecked_ref(), &options)?;
        target.add_event_listener_with_callback_and_add_event_listener_options(
            "pointerup",
            self.pointer_up_cb.as_ref().unchecked_ref(), &options)?;
        self.doc.body()
            .expect("Could not get boby")
            .add_event_listener_with_callback_and_add_event_listener_options(
                "pointermove",
                self.pointer_move_cb.as_ref().unchecked_ref(), &options)?;

        self.state = BoardState::Dragging(Dragging {
            target,
            shadow,
            offset: (mx - tx, my - ty),
        });
        Ok(())
    }

    fn drop_target(&self, evt: &PointerEvent) -> JsResult<(Pos, DropTarget)> {
        if let BoardState::Dragging(d) = &self.state {
            // Get the position of the tile being dragged
            // in SVG frame coordinates (0-200)
            let (mut x, mut y) = self.mouse_pos(evt.as_ref());
            x -= d.offset.0;
            y -= d.offset.1;

            // Clamp to grid
            if x < 0.0 {
                x = 0.0;
            }
            if y < 0.0 {
                y = 0.0;
            }
            if x > 140.0 {
                x = 140.0;
            }
            if y > 200.0 {
                y = 200.0;
            }

            let pos = (x, y);
            let (tx, ty) = self.game_position((x + 4.0, y + 4.0)); // get center of ball

            if self.game_states.last().unwrap().clone().move_ball(vec![(tx, ty)]).is_some() {
                return Ok((pos, DropTarget::DropBall((tx, ty))));
            }

            // Otherwise, return to the grid
            Ok((pos, DropTarget::ReturnBall))
        } else {
            Err(JsValue::from_str("Invalid state (drop target)"))
        }
    }

    fn on_pointer_move(&self, evt: PointerEvent) -> JsError {
        if let BoardState::Dragging(d) = &self.state {
            evt.prevent_default();

            let (pos, drop_target) = self.drop_target(&evt)?;
            d.target.set_attribute("transform",
                                   &format!("translate({} {})", pos.0, pos.1))?;
            if let DropTarget::DropBall((gx, gy)) = drop_target {
                let (gx, gy) = self.grid_position((gx, gy));
                d.shadow.set_attribute(
                    "transform", &format!("translate({} {})",
                         gx, gy))?;
                d.shadow.set_attribute("visibility", "visible")
            } else {
                d.shadow.set_attribute("visibility", "hidden")
            }
        } else {
            Err(JsValue::from_str("Invalid state (pointer move)"))
        }
    }

    fn on_pointer_up(&mut self, evt: PointerEvent) -> JsError {
        if let BoardState::Dragging(d) = &self.state {
            evt.prevent_default();

            d.target.release_pointer_capture(evt.pointer_id())?;
            d.target.remove_event_listener_with_callback("pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())?;
            d.target.remove_event_listener_with_callback("pointerup",
                    self.pointer_up_cb.as_ref().unchecked_ref())?;
            self.doc.body()
                .expect("Could not get boby")
                .remove_event_listener_with_callback(
                    "pointermove",
                    self.pointer_move_cb.as_ref().unchecked_ref())?;

            let (pos, drop_target) = self.drop_target(&evt)?;
            let drag_anim = match drop_target {
                DropTarget::ReturnBall => {
                    self.pieces_group.remove_child(&d.shadow)?;
                    let game = self.game_states.last().unwrap();
                    self.grid.insert(game.ball, d.target.clone());
                    let (x, y) = self.grid_position(game.ball);
                    DragAnim::ReturnBall(ReturnBall(
                        TileAnimation {
                            target: d.target.clone(),
                            start: pos,
                            end: (x, y),
                            t0: evt.time_stamp()
                        }))
                },
                DropTarget::DropBall((gx, gy)) => {
                    if self.mov.is_none() {
                        self.mov = Some(Move::Ball(vec![]))
                    }
                    if let Some(Move::Ball(jumps)) = &mut self.mov {
                        jumps.push((gx, gy));
                    } else {
                        unreachable!();
                    }
                    self.grid.insert((gx, gy), d.target.clone());
                    let mut game = self.game_states.last().unwrap().clone();
                    let remove_men = game.move_ball(vec![(gx, gy)]).unwrap();
                    self.game_states.push(game);
                    let target = d.target.clone();
                    let (gx, gy) = self.grid_position((gx, gy));
                    DragAnim::DropBall(DropBall {
                        anim: TileAnimation {
                            target,
                            start: (pos.0, pos.1),
                            end: (gx, gy),
                            t0: evt.time_stamp(),
                        },
                        shadow: d.shadow.clone(),
                        remove_men,
                    })
                },
            };

            self.state = BoardState::Animation(drag_anim);
            self.request_animation_frame()?;

            if self.mov.is_some() {
                self.accept_button.set_disabled(false);
                self.undo_button.set_disabled(false);
            }
        }
        Ok(())
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        if let BoardState::Animation(drag) = &mut self.state {
            match drag {
                DragAnim::UndoBall(d) => {
                    let target = d.anim.target.clone();
                    let add_men = d.add_men.clone();
                    if d.anim.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        for &(game_pos, grid_pos) in &add_men {
                            let man = self.new_man(1.0)?;
                            self.pieces_group.append_child(&man)?;
                            man.set_attribute("transform",
                                              &format!("translate({} {})", grid_pos.0, grid_pos.1))?;
                            self.grid.insert(game_pos, man);
                        }
                        self.svg.remove_child(&target)?;
                        self.pieces_group.append_child(&target)?;
                        self.state = BoardState::Idle;
                    }
                }
                DragAnim::DropBall(d) => {
                    if d.anim.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        for pos in &d.remove_men {
                            self.pieces_group.remove_child(&self.grid.remove(pos).unwrap())?;
                        }
                        self.pieces_group.remove_child(&d.shadow)?;
                        self.svg.remove_child(&d.anim.target)?;
                        self.pieces_group.append_child(&d.anim.target)?;
                        self.state = BoardState::Idle;
                    }
                },
                DragAnim::ReturnBall(d) => {
                    if d.0.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        self.svg.remove_child(&d.0.target)?;
                        self.pieces_group.append_child(&d.0.target)?;
                        self.state = BoardState::Idle;
                    }
                },
                DragAnim::JumpBall(j) => {
                    if j.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        self.svg.remove_child(&j.target)?;
                        self.pieces_group.append_child(&j.target)?;
                        for men in &j.men {
                            for man in men {
                                self.pieces_group.remove_child(man)?;
                            }
                        }
                        self.state = BoardState::Idle;
                    }
                },
                DragAnim::PlaceMan(p) => {
                    if p.run(t)? {
                        self.request_animation_frame()?;
                    } else {
                        self.state = BoardState::Idle;
                    }
                }
            }
        }
        Ok(())
    }

    fn request_animation_frame(&self) -> JsResult<i32> {
        web_sys::window()
            .expect("no global `window` exists")
            .request_animation_frame(self.anim_cb.as_ref()
                                     .unchecked_ref())
    }

    fn new_man(&self, size: f32) -> JsResult<Element> {
        let g = self.doc.create_svg_element("g")?;
        let s = self.doc.create_svg_element("circle")?;
        s.set_attribute("r", &format!("{}", 4.0 * size))?;
        s.set_attribute("cx", "5.0")?;
        s.set_attribute("cy", "5.0")?;

        g.append_child(&s)?;
        g.class_list().add_1("man")?;
        g.class_list().add_1("piece")?;

        Ok(g)
    }

    fn new_ball(&self) -> JsResult<Element> {
        let g = self.doc.create_svg_element("g")?;
        let s = self.doc.create_svg_element("circle")?;
        s.set_attribute("r", "4.0")?;
        s.set_attribute("cx", "5.0")?;
        s.set_attribute("cy", "5.0")?;

        g.append_child(&s)?;
        g.class_list().add_1("ball")?;

        let mut options = AddEventListenerOptions::new();
        options.passive(false);
        g.add_event_listener_with_callback_and_add_event_listener_options(
            "pointerdown",
            self.pointer_down_cb.as_ref().unchecked_ref(),
            &options)?;
        g.add_event_listener_with_callback_and_add_event_listener_options(
            "touchstart",
            self.touch_start_cb.as_ref().unchecked_ref(),
            &options)?;

        g.class_list().add_1("piece")?;

        Ok(g)
    }

    fn on_undo_button(&mut self, evt: Event) -> JsError {
        // Don't allow for any tricky business here
        if self.state != BoardState::Idle {
            return Ok(());
        }

        self.hide_man_shadow()?;

        let mut mov = None;
        std::mem::swap(&mut mov, &mut self.mov);
        let game = self.game_states.pop().unwrap();

        match mov {
            Some(Move::Ball(mut jumps)) => {
                jumps.pop();
                if !jumps.is_empty() {
                    self.mov = Some(Move::Ball(jumps));
                }
                let mut prev_game = self.game_states.last().unwrap().clone();
                let t = self.grid.remove(&game.ball).unwrap();
                self.grid.insert(prev_game.ball, t.clone());
                self.pieces_group.remove_child(&t)?;
                self.svg.append_child(&t)?;
                let start = self.grid_position(game.ball);
                let end = self.grid_position(prev_game.ball);
                let drag = DragAnim::UndoBall(UndoBall {
                    anim: TileAnimation {
                        target: t,
                        start,
                        end,
                        t0: evt.time_stamp()
                    },
                    add_men: prev_game.move_ball(vec![game.ball]).unwrap()
                        .iter()
                        .map(|&pos| (pos, self.grid_position(pos)))
                        .collect(),
                });
                self.state = BoardState::Animation(drag);
                self.request_animation_frame()?;
            },
            Some(Move::Man(pos)) => {
                self.pieces_group.remove_child(&self.grid.remove(&pos).unwrap())?;
            },
            _ => unreachable!(),
        }

        self.accept_button.set_disabled(self.mov.is_none());
        self.undo_button.set_disabled(self.mov.is_none());

        Ok(())
    }

    /*  Attempts to make the given move. */
    fn make_move(&mut self, _evt: Event) -> JsResult<Move> {
        self.hide_man_shadow()?;

        self.accept_button.set_disabled(true);
        self.undo_button.set_disabled(true);

        Ok(self.mov.as_ref().unwrap().clone())
    }

    fn on_move_accepted(&mut self) {
        self.mov = None;
        self.game_states = vec![self.game_states.pop().unwrap()];
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct Base {
    doc: Document,
    ws: WebSocket,
}

impl Base {
    fn send(&self, msg: ClientMessage) -> JsError {
        let encoded = bincode::serialize(&msg)
            .map_err(|e| JsValue::from_str(
                    &format!("Could not encode: {}", e)))?;
        self.ws.send_with_u8_array(&encoded[..])
    }
}

////////////////////////////////////////////////////////////////////////////////

// These are the states in the system
struct Connecting {
    base: Base
}

struct CreateOrJoin {
    base: Base,

    name_input: HtmlInputElement,
    room_input: HtmlInputElement,
    play_button: HtmlButtonElement,
    colorblind_checkbox: HtmlInputElement,
    err_div: HtmlElement,
    err_span: HtmlElement,

    // Callbacks are owned so that it lives as long as the state
    _room_invalid_cb: JsClosure<Event>,
    _input_cb: JsClosure<Event>,
    _submit_cb: JsClosure<Event>,
}

struct Playing {
    base: Base,

    chat_div: HtmlElement,
    chat_input: HtmlInputElement,
    score_table: HtmlElement,
    your_side: Side,
    active_side: Side,
    opponent: Option<String>,
    player_name: String,

    board: Board,

    // Callback is owned so that it lives as long as the state
    _keyup_cb: JsClosure<KeyboardEvent>,
}

////////////////////////////////////////////////////////////////////////////////

enum State {
    Connecting(Connecting),
    CreateOrJoin(CreateOrJoin),
    Playing(Playing),
    Empty,
}

impl State {
    transitions!(
        Connecting => [
            on_connected() -> CreateOrJoin,
        ],
        CreateOrJoin => [
            on_joined_room(room_name: &str, opponent: &Option<String>,
                           active_side: Side,
                           your_side: Side,
                           game: Game) -> Playing,
        ],
    );

    methods!(
        Playing => [
            on_board_click(evt: PointerEvent),
            on_board_hover(evt: PointerEvent),
            on_pointer_down(evt: PointerEvent),
            on_pointer_up(evt: PointerEvent),
            on_pointer_move(evt: PointerEvent),
            on_accept_button(evt: Event),
            on_undo_button(evt: Event),
            on_anim(t: f64),
            on_send_chat(),
            on_chat(from: &str, msg: &str),
            on_information(msg: &str),
            on_opponent_joined(name: &str),
            on_opponent_disconnected(),
            on_opponent_moved(mov: Move),
            on_move_accepted(),
            on_move_rejected(),
            on_finished(winner: Side),
            on_new_game(active_side: Side, your_side: Side, game: Game),
        ],
        CreateOrJoin => [
            on_room_name_invalid(),
            on_join_inputs_changed(),
            on_join_button(),
            on_join_failed(room: &str),
        ],
    );
}

unsafe impl Send for State { /* YOLO */}

lazy_static::lazy_static! {
    static ref HANDLE: Mutex<State> = Mutex::new(State::Empty);
}
////////////////////////////////////////////////////////////////////////////////

// Boilerplate to wrap and bind a callback.
// The resulting callback must be stored for as long as it may be used.
#[must_use]
fn build_cb<F, T>(f: F) -> JsClosure<T>
    where F: FnMut(T) -> JsError + 'static,
          T: FromWasmAbi + 'static
{
    Closure::wrap(Box::new(f) as Box<dyn FnMut(T) -> JsError>)
}

#[must_use]
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F) -> JsClosure<T>
    where E: JsCast + Clone + std::fmt::Debug,
          F: FnMut(T) -> JsError + 'static,
          T: FromWasmAbi + 'static
{
    let cb = build_cb(f);
    let target = obj.dyn_ref::<EventTarget>()
        .expect("Could not convert into `EventTarget`");
    target.add_event_listener_with_callback(name, cb.as_ref().unchecked_ref())
        .expect("Could not add event listener");
    cb
}

////////////////////////////////////////////////////////////////////////////////

impl Connecting {
    fn on_connected(self) -> JsResult<CreateOrJoin> {
        self.base.doc.get_element_by_id("disconnected_msg")
            .expect("Could not get disconnected_msg div")
            .dyn_into::<HtmlElement>()?
            .set_text_content(Some("Lost connection to game server"));
        CreateOrJoin::new(self.base)
    }
}

impl CreateOrJoin {
    fn new(base: Base) -> JsResult<CreateOrJoin> {
        let name_input = base.doc.get_element_by_id("name_input")
            .expect("Could not find name_input")
            .dyn_into::<HtmlInputElement>()?;
        let room_input = base.doc.get_element_by_id("room_input")
            .expect("Could not find room_input")
            .dyn_into::<HtmlInputElement>()?;
        let room_invalid_cb = set_event_cb(&room_input, "invalid",
            move |_: Event| {
                HANDLE.lock().unwrap().on_room_name_invalid()
            });
        let input_cb = set_event_cb(&room_input, "input", move |_: Event| {
            HANDLE.lock().unwrap().on_join_inputs_changed()
        });

        let form = base.doc.get_element_by_id("join_form")
            .expect("Could not find join_form");
        let submit_cb = set_event_cb(&form, "submit", move |e: Event| {
            e.prevent_default();
            HANDLE.lock().unwrap().on_join_button()
        });

        let err_div = base.doc.get_element_by_id("err_div")
            .expect("Could not find err_div")
            .dyn_into()?;
        let err_span = base.doc.get_element_by_id("err_span")
            .expect("Could not find err_span")
            .dyn_into()?;

        let play_button = base.doc.get_element_by_id("play_button")
            .expect("Could not find play_button")
            .dyn_into::<HtmlButtonElement>()?;

        play_button.set_text_content(Some(
            if room_input.value().is_empty() {
                "Create new room"
            } else {
                "Join existing room"
            }));
        play_button.class_list().remove_1("disabled")?;

        let colorblind_checkbox = base.doc.get_element_by_id("colorblind")
            .expect("Could not find colorblind checkbox")
            .dyn_into()?;

        Ok(CreateOrJoin {
            base,
            name_input,
            room_input,
            play_button,
            colorblind_checkbox,
            err_div,
            err_span,

            _input_cb: input_cb,
            _submit_cb: submit_cb,
            _room_invalid_cb: room_invalid_cb,
        })
    }

    fn on_join_failed(&self, err: &str) -> JsError {
        self.err_span.set_text_content(Some(err));
        self.err_div.set_hidden(false);
        self.play_button.set_disabled(false);
        Ok(())
    }

    fn on_joined_room(self, room_name: &str, opponent: &Option<String>,
                      active_side: Side, your_side: Side,
                      game: Game) -> JsResult<Playing>
    {
        self.base.doc.get_element_by_id("join")
            .expect("Could not get join div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(true);
        self.base.doc.get_element_by_id("playing")
            .expect("Could not get playing div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(false);

        let player_name = self.name_input.value();
        let mut p = Playing::new(self.base, room_name, player_name.clone(), opponent,
                                 active_side, your_side,
                                 game)?;
        p.set_my_turn(active_side == your_side)?;
        p.on_information(&format!("Welcome, {}!", player_name))?;
        Ok(p)
    }

    fn on_join_button(&self) -> JsError {
        self.play_button.set_disabled(true);
        let name = self.name_input.value();
        let room = self.room_input.value();
        if self.colorblind_checkbox.checked() {
            self.base.doc.get_element_by_id("playing")
                .ok_or_else(|| JsValue::from_str("No playing box"))?
                .class_list()
                .add_1("colorblind")?;
        }
        let msg = if room.is_empty() {
            ClientMessage::CreateRoom(name)
        } else {
            ClientMessage::JoinRoom(name, room)
        };
        self.base.send(msg)
    }

    fn on_join_inputs_changed(&self) -> JsError {
        self.play_button.set_text_content(Some(
            if self.room_input.value().is_empty() {
                "Create new room"
            } else {
                "Join existing room"
            }));
        self.room_input.set_custom_validity("");
        Ok(())
    }

    fn on_room_name_invalid(&self) -> JsError {
        self.room_input.set_custom_validity("three lowercase words");
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

impl Playing {
    fn new(base: Base, room_name: &str, player_name: String, opponent: &Option<String>,
           active_side: Side, your_side: Side,
           game: Game) -> JsResult<Playing>
    {
        // The title lists the room name
        let s: HtmlElement = base.doc.get_element_by_id("room_name")
            .expect("Could not get room_name")
            .dyn_into()?;
        s.set_text_content(Some(&room_name));

        let board = Board::new(&base.doc, game, your_side)?;

        let b = base.doc.get_element_by_id("chat_name")
            .expect("Could not get chat_name");
        b.set_text_content(Some(&format!("{}:", player_name)));

        // If Enter is pressed while focus is in the chat box,
        // send a chat message to the server.
        let chat_input = base.doc.get_element_by_id("chat_input")
            .expect("Could not get chat_input")
            .dyn_into()?;
        let keyup_cb = set_event_cb(&chat_input, "keyup",
            move |e: KeyboardEvent| {
                if e.key_code() == 13 { // Enter key
                    e.prevent_default();
                    HANDLE.lock().unwrap().on_send_chat()
                } else {
                    Ok(())
                }
            });

        let chat_div = base.doc.get_element_by_id("chat_msgs")
            .expect("Could not get chat_div")
            .dyn_into()?;
        let score_table = base.doc.get_element_by_id("score_rows")
            .expect("Could not get score_rows")
            .dyn_into()?;

        let mut out = Playing {
            base,
            board,

            chat_input,
            chat_div,
            score_table,
            your_side,
            active_side,
            opponent: opponent.clone(),
            player_name: player_name.clone(),

            _keyup_cb: keyup_cb,
        };

        out.add_player_row("".to_string())?;
        out.add_player_row("".to_string())?;
        out.change_player_name(your_side, &format!("{} (you)", player_name));
        out.change_player_name(your_side.opposite(), opponent.as_deref().unwrap_or(""));

        out.set_my_turn(active_side == your_side)?;

        Ok(out)
    }

    fn on_new_game(&mut self, active_side: Side, your_side: Side, game: Game) -> JsError {
        self.on_information("New game started!")?;
        self.active_side = active_side;
        self.your_side = your_side;
        self.change_player_name(your_side, &format!("{} (you)", self.player_name));
        self.change_player_name(your_side.opposite(), self.opponent.clone().as_deref().unwrap_or(""));
        self.board.new_game(game, your_side)?;
        self.set_my_turn(active_side == your_side)
    }

    fn set_my_turn(&mut self, my_turn: bool) -> JsError {
        self.active_side =
            if my_turn {
                self.your_side
            } else {
                self.your_side.opposite()
            };
        self.score_table.child_nodes()
            .item(Self::row(self.active_side.opposite()) + 3)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .remove_1("active")?;
        self.score_table.child_nodes()
            .item(Self::row(self.active_side) + 3)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .add_1("active")?;

        if self.active_side == self.your_side {
            self.on_information("It's your turn!")
        } else {
            if let Some(opponent) = &self.opponent {
                self.on_information(&format!("It's {}'s turn!", opponent))
            } else {
                self.on_information("It's your opponent's turn!")
            }
        }?;

        self.board.set_my_turn(self.active_side == self.your_side)
    }

    fn on_chat(&self, from: &str, msg: &str) -> JsError {
        let p = self.base.doc.create_element("p")?;
        p.set_class_name("msg");

        let b = self.base.doc.create_element("b")?;
        b.set_text_content(Some(from));
        p.append_child(&b)?;

        let s =  self.base.doc.create_element("b")?;
        s.set_text_content(Some(":"));
        p.append_child(&s)?;

        let s =  self.base.doc.create_element("span")?;
        s.set_text_content(Some(msg));
        p.append_child(&s)?;

        self.chat_div.append_child(&p)?;
        self.chat_div.set_scroll_top(self.chat_div.scroll_height());
        Ok(())
    }

    fn on_information(&self, msg: &str) -> JsError {
        let p = self.base.doc.create_element("p")?;
        p.set_class_name("msg");

        let i = self.base.doc.create_element("i")?;
        i.set_text_content(Some(msg));
        p.append_child(&i)?;
        self.chat_div.append_child(&p)?;
        self.chat_div.set_scroll_top(self.chat_div.scroll_height());
        Ok(())
    }

    fn row(side: Side) -> u32 {
        match side {
            Side::Top => 0,
            Side::Bottom => 1,
        }
    }

    fn change_player_name(&mut self, side: Side, name: &str) {
        self.score_table.child_nodes()
            .item(Self::row(side) + 3)
            .expect("Could not get table row")
            .child_nodes()
            .item(1)
            .expect("Could not get score value")
            .set_text_content(Some(name));
    }

    fn add_player_row(&mut self, name: String) -> JsError {
        let tr = self.base.doc.create_element("tr")?;
        tr.set_class_name("player-row");

        let td = self.base.doc.create_element("td")?;
        let i = self.base.doc.create_element("i")?;
        i.set_class_name("fas fa-caret-right");
        td.append_child(&i)?;
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(&name));
        tr.append_child(&td)?;

        let td = self.base.doc.create_element("td")?;
        td.set_text_content(Some(""));
        tr.append_child(&td)?;

        self.score_table.append_child(&tr)?;

        Ok(())
    }

    fn on_send_chat(&self) -> JsError {
        let i = self.chat_input.value();
        if !i.is_empty() {
            self.chat_input.set_value("");
            self.base.send(ClientMessage::Chat(i))
        } else {
            Ok(())
        }
    }

    fn on_opponent_joined(&mut self, name: &str) -> JsError {
        self.change_player_name(self.your_side.opposite(), name);
        self.on_information(&format!("{} joined the room", name))?;
        self.opponent = Some(name.to_string());
        Ok(())
    }

    fn on_opponent_disconnected(&mut self) -> JsError {
        self.change_player_name(self.your_side.opposite(), "");
        self.on_information(&format!("{} disconnected",
                                     self.opponent.as_ref().unwrap()))?;
        self.opponent = None;
        Ok(())
    }

    fn on_anim(&mut self, t: f64) -> JsError {
        self.board.on_anim(t)
    }

    fn on_board_click(&mut self, evt: PointerEvent) -> JsError {
        console_log!("Board was clicked");
        self.board.on_board_click(evt)
    }

    fn on_board_hover(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_board_hover(evt)
    }

    fn on_pointer_down(&mut self, evt: PointerEvent) -> JsError {
        console_log!("Pointer down");
        self.board.on_pointer_down(evt)
    }

    fn on_pointer_move(&mut self, evt: PointerEvent) -> JsError {
        self.board.on_pointer_move(evt)
    }

    fn on_pointer_up(&mut self, evt: PointerEvent) -> JsError {
        console_log!("Pointer up");
        self.board.on_pointer_up(evt)
    }

    fn on_undo_button(&mut self, evt: Event) -> JsError {
        console_log!("Undo button pressed");
        self.board.on_undo_button(evt)
    }

    fn on_accept_button(&mut self, evt: Event) -> JsError {
        if self.board.state != BoardState::Idle {
            return Ok(());
        }
        console_log!("Accept button pressed");
        self.set_my_turn(false)?;
        self.base.send(ClientMessage::Move(self.board.make_move(evt)?))
    }

    fn on_opponent_moved(&mut self, mov: Move) -> JsError {
        match mov {
            Move::Ball(jumps) => {
                let start_pos = self.board.game_states.last().unwrap().ball;
                let ball = self.board.grid.remove(&start_pos).unwrap();
                let t0 = get_time_ms();
                self.board.pieces_group.remove_child(&ball)?;
                self.board.svg.append_child(&ball)?;
                self.board.state = BoardState::Animation(DragAnim::JumpBall(JumpBall {
                    target: ball.clone(),
                    points: std::iter::once(self.board.grid_position(start_pos))
                        .chain(jumps.iter().map(|&pos| self.board.grid_position(pos)))
                        .collect(),
                    t0,
                    men: jumps.iter()
                        .fold((self.board.game_states.last().unwrap().clone(), Vec::new()), |(mut game, mut v), &pos| {
                            v.push(game.move_ball(vec![pos]).unwrap().iter()
                                .map(|pos| self.board.grid.get(pos).unwrap().clone())
                                .collect());
                            (game, v)
                        }).1,
                }));
                self.board.request_animation_frame()?;
                for remove_man in self.board.game_states.last_mut().unwrap().move_ball(jumps.clone()).unwrap() {
                    self.board.grid.remove(&remove_man);
                }
                self.board.grid.insert(*jumps.last().unwrap(), ball);
            }
            Move::Man(pos) => {
                let man = self.board.new_man(0.0)?;
                self.board.pieces_group.append_child(&man)?;
                let (x, y) = self.board.grid_position(pos);
                man.set_attribute("transform",
                                &format!("translate({} {})", x, y))?;
                self.board.grid.insert(pos, man.clone());
                self.board.game_states.last_mut().unwrap().place_man(pos);
                self.board.state = BoardState::Animation(DragAnim::PlaceMan(PlaceMan {
                    target: man,
                    t0: get_time_ms(),
                }));
                self.board.request_animation_frame()?;
            }
        }
        self.set_my_turn(true)
    }

    fn on_move_accepted(&mut self) -> JsError {
        self.board.on_move_accepted();
        Ok(())
    }

    fn on_move_rejected(&mut self) -> JsError {
        Ok(())
    }

    fn on_finished(&mut self, winner: Side) -> JsError {
        self.board.set_my_turn(false)?;

        let children = self.score_table.child_nodes();
        children
            .item(Self::row(self.active_side) + 3)
            .unwrap()
            .dyn_into::<HtmlElement>()?
            .class_list()
            .remove_1("active")?;

        if winner == self.your_side {
            self.on_information("You win!")
        } else {
            self.on_information("You lost!")
        }
    }
}

////////////////////////////////////////////////////////////////////////////////


fn on_message(msg: ServerMessage) -> JsError {
    use ServerMessage::*;
    console_log!("Got message {:?}", msg);

    let mut state = HANDLE.lock().unwrap();

    match msg {
        JoinFailed(name) => state.on_join_failed(&name),
        JoinedRoom{room_name, opponent, active_side, your_side, game} =>
            state.on_joined_room(&room_name, &opponent,
                                 active_side, your_side,
                                 game),
        Chat{from, message} => state.on_chat(&from, &message),
        Information(message) => state.on_information(&message),
        OpponentJoined(name) => state.on_opponent_joined(&name),
        OpponentDisconnected => state.on_opponent_disconnected(),
        OpponentMoved(mov) => state.on_opponent_moved(mov),
        MoveAccepted => state.on_move_accepted(),
        MoveRejected => state.on_move_rejected(),
        ItsOver(winner) => state.on_finished(winner),
        NewGame{active_side, your_side, game} => state.on_new_game(active_side, your_side, game),
    }
}

////////////////////////////////////////////////////////////////////////////////

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> JsError {
    console_error_panic_hook::set_once();

    let window = web_sys::window()
        .expect("no global `window` exists");
    let doc = window.document()
        .expect("should have a document on window");

    let location = doc.location()
        .expect("Could not get doc location");
    let hostname = location.hostname()?;

    // Pick the port based on the connection type
    let (ws_protocol, ws_port) = if location.protocol()? == "https:" {
        ("wss", 8081)
    } else {
        ("ws",  8080)
    };
    let hostname = format!("{}://{}:{}", ws_protocol, hostname, ws_port);

    let doc = web_sys::window()
        .expect("no global `window` exists")
        .document()
        .expect("should have a document on window");
    console_log!("Connecting to websocket at {}", hostname);
    let ws = WebSocket::new(&hostname)?;

    // The websocket callbacks are long-lived, so we forget them here
    set_event_cb(&ws, "open", move |_: JsValue| {
        HANDLE.lock().unwrap()
            .on_connected()
    }).forget();
    let on_decoded_cb = Closure::wrap(Box::new(move |e: ProgressEvent| {
        let target = e.target().expect("Could not get target");
        let reader: FileReader = target.dyn_into().expect("Could not cast");
        let result = reader.result().expect("Could not get result");
        let buf = js_sys::Uint8Array::new(&result);
        let mut data = vec![0; buf.length() as usize];
        buf.copy_to(&mut data[..]);
        let msg = bincode::deserialize(&data[..])
            .map_err(|e| JsValue::from_str(
                    &format!("Failed to deserialize: {}", e)))
            .expect("Could not decode message");
        on_message(msg)
            .expect("Message decoding failed")
    }) as Box<dyn FnMut(ProgressEvent)>);
    set_event_cb(&ws, "message", move |e: MessageEvent| {
        let blob = e.data().dyn_into::<Blob>()?;
        let fr = FileReader::new()?;
        fr.add_event_listener_with_callback("load",
                &on_decoded_cb.as_ref().unchecked_ref())?;
        fr.read_as_array_buffer(&blob)?;
        Ok(())
    }).forget();
    set_event_cb(&ws, "close", move |_: Event| -> JsError {
        let doc = web_sys::window()
            .expect("no global `window` exists")
            .document()
            .expect("should have a document on window");
        for d in ["join", "playing"].iter() {
            doc.get_element_by_id(d)
                .expect("Could not get major div")
                .dyn_into::<HtmlElement>()?
                .set_hidden(true);
        }
        doc.get_element_by_id("disconnected")
            .expect("Could not get disconnected div")
            .dyn_into::<HtmlElement>()?
            .set_hidden(false);
        Ok(())
    }).forget();

    let rev = doc.get_element_by_id("revhash")
        .expect("Could not find rev");
    rev.set_text_content(Some(env!("VERGEN_SHA_SHORT")));

    let base = Base { doc, ws };
    base.doc.get_element_by_id("play_button")
        .expect("Could not get loading div")
        .dyn_into::<HtmlElement>()?
        .set_text_content(Some("Connecting..."));

    *HANDLE.lock().unwrap() = State::Connecting(Connecting { base });

    Ok(())
}
