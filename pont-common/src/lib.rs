use serde::{Serialize, Deserialize};

#[derive(Debug, Deserialize, Serialize)]
pub enum ClientMessage {
    CreateRoom(String),
    JoinRoom(String, String),
    Chat(String),
    Move(Move),

    Disconnected,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ServerMessage {
    JoinedRoom {
        room_name: String,
        opponent: Option<String>,
        active_side: Side,
        your_side: Side,
        game: Game,
    },
    JoinFailed(String),
    Chat {
        from: String,
        message: String,
    },
    Information(String),
    OpponentJoined(String),
    OpponentDisconnected,
    OpponentMoved(Move),
    MoveAccepted,
    MoveRejected,
    ItsOver(Side),
    NewGame {
        active_side: Side,
        your_side: Side,
        game: Game,
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub enum Side {
    Top, Bottom
}

impl Default for Side {
    fn default() -> Self {
        Self::Bottom
    }
}

impl Side {
    pub fn opposite(self) -> Self {
        use Side::*;
        match self {
            Top => Bottom,
            Bottom => Top,
        }
    }
}

pub type Position = (u8, u8);

pub const WIDTH: u8 = 15;
pub const HEIGHT: u8 = 21;
pub const CENTER: Position = (WIDTH / 2, HEIGHT / 2);

#[derive(Deserialize, Serialize, Debug, Clone)]
pub enum Move {
    Man(Position),
    Ball(Vec<Position>),
}

#[derive(Clone, Copy, Eq, PartialEq, Deserialize, Serialize, Debug)]
pub enum PositionState {
    Empty, Man, Ball,
}

pub fn ball_in_bounds(pos: Position) -> bool {
    let (x, y) = pos;
    x < WIDTH && y < HEIGHT
}

pub fn man_in_bounds(pos: Position) -> bool {
    let (x, y) = pos;
    x < WIDTH && y >= 1 && y < (HEIGHT - 1)
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Game {
    pub board: [[PositionState; HEIGHT as usize]; WIDTH as usize],
    pub ball: Position,
}

impl Game {
    pub fn winner(&self) -> Option<Side> {
        let (_, y) = self.ball;
        if y <= 1 {
            Some(Side::Bottom)
        } else if y >= HEIGHT - 2 {
            Some(Side::Top)
        } else {
            None
        }
    }
    pub fn place_man(&mut self, pos: Position) -> bool {
        if man_in_bounds(pos) && self.state_at(pos) == PositionState::Empty {
            self.set_state(pos, PositionState::Man);
            true
        } else {
            false
        }
    }

    pub fn move_ball(&mut self, jumps: Vec<Position>) -> Option<Vec<Position>> {
        let mut scratch = self.clone();
        let mut remove = Vec::new();
        for pos in jumps {
            if ball_in_bounds(pos) {
                if let Some(mut r) = scratch.ball_jump(pos) {
                    remove.append(&mut r);
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        std::mem::swap(self, &mut scratch);
        Some(remove)
    }

    fn ball_jump(&mut self, to: Position) -> Option<Vec<Position>> {
        let dx = to.0 as i8 - self.ball.0 as i8;
        let dy = to.1 as i8 - self.ball.1 as i8;
        if dx != 0 && dy != 0 && dx.abs() != dy.abs() {
            return None;
        }
        if std::cmp::max(dx.abs(), dy.abs()) < 2 {
            return None;
        }
        self.set_state(self.ball, PositionState::Empty);
        self.ball = ((self.ball.0 as i8 + dx.signum()) as u8,
                     (self.ball.1 as i8 + dy.signum()) as u8);
        let mut remove = Vec::new();
        while self.ball != to {
            if self.state_at(self.ball) != PositionState::Man {
                return None;
            }
            self.set_state(self.ball, PositionState::Empty);
            remove.push(self.ball);
            self.ball = ((self.ball.0 as i8 + dx.signum()) as u8,
                         (self.ball.1 as i8 + dy.signum()) as u8);
        }
        if self.state_at(self.ball) != PositionState::Empty {
            return None;
        }
        self.set_state(self.ball, PositionState::Ball);
        Some(remove)
    }

    fn set_state(&mut self, pos: Position, state: PositionState) {
        let (x, y) = pos;
        self.board[x as usize][y as usize] = state;
    }

    fn state_at(&mut self, pos: Position) -> PositionState {
        let (x, y) = pos;
        self.board[x as usize][y as usize]
    }
}

impl Default for Game {
    fn default() -> Game {
        let mut game = Self {
            board: [[PositionState::Empty; HEIGHT as usize]; WIDTH as usize],
            ball: CENTER,
        };
        game.set_state(game.ball, PositionState::Ball);
        game
    }
}
