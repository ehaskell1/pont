use std::{
    collections::HashMap,
    env,
    io::Error as IoError,
    net::{TcpStream, TcpListener, SocketAddr},
    sync::{Arc, Mutex},
    time::Duration,
};
use rand::Rng;
use log::{error, info, trace, warn};
use env_logger::Env;

use futures::{future, join};
use futures::stream::StreamExt;
use futures::sink::SinkExt;
use futures::channel::mpsc::{unbounded, UnboundedSender, UnboundedReceiver};

use anyhow::Result;
use tungstenite::Message as WebsocketMessage;
use async_tungstenite::WebSocketStream;
use smol::{Async, Task, Timer};

use pont_common::{ClientMessage, ServerMessage, Game, Position, Move, Side};

////////////////////////////////////////////////////////////////////////////////

lazy_static::lazy_static! {
    // words.txt is the EFF's random word list for passphrases
    static ref WORD_LIST: Vec<&'static str> = include_str!("words.txt")
        .split('\n')
        .filter(|w| !w.is_empty())
        .collect();
}

// Normally, a room exists as a relatively standalone task:
// Client websockets send their messages to `write`, and `run_room` reads
// them from `read` and applies them to the `room` object.
//
// It's made more complicated by the fact that adding players needs to
// access the room object *before* clients are plugged into the `read`/`write`
// infrastructure, so it must be shared and accessible from `handle_connection`
type TaggedClientMessage = (SocketAddr, ClientMessage);
#[derive(Clone)]
struct RoomHandle {
    write: UnboundedSender<TaggedClientMessage>,
    room: Arc<Mutex<Room>>,
}

impl RoomHandle {
    async fn run_room(&mut self,
                      mut read: UnboundedReceiver<TaggedClientMessage>)
    {
        while let Some((addr, msg)) = read.next().await {
            if !self.room.lock().unwrap().on_message(addr, msg) {
                break;
            }
        }
    }
}

async fn add_player(player_name: String, addr: SocketAddr,
                    handle: RoomHandle,
                    ws_stream: &mut WebSocketStream<Async<TcpStream>>,
                    ws_tx: UnboundedSender<ServerMessage>)
                    -> Result<()> {
    let res = {
        let mut room = handle.room.lock().unwrap();
        room.add_player(addr, player_name, ws_tx)
    };
    match res {
        Ok(false) => {
            let msg = ServerMessage::JoinFailed("Room is full".to_string());
            let encoded = bincode::serialize(&msg)?;
            ws_stream.send(WebsocketMessage::Binary(encoded)).await?;
        }
        Err(e) => {
            error!("[{}] Failed to add player: {:?}", handle.room.lock().unwrap().name, e);
        }
        _ => {}
    }
    Ok(())
}

async fn run_player(player_name: String, addr: SocketAddr,
                    handle: RoomHandle,
                    mut ws_stream: WebSocketStream<Async<TcpStream>>)
{
    // Messages to the player's websocket are mediated by a queue,
    // with a separate async task reading messages from the queue
    // and pushing them down the websocket.  This lets us send messages to
    // a player without blocking or needing an extra await in the
    // main game loop, which would get awkward.
    let (ws_tx, ws_rx) = unbounded();

    if let Err(e) = add_player(player_name.clone(), addr,
                               handle.clone(),
                               &mut ws_stream,
                               ws_tx).await {
        warn!("Failed to handle connection from {}: {}", addr, e);
    }

    let (incoming, outgoing) = ws_stream.split();

    let write = handle.write.clone();
    let ra = ws_rx
        .map(|c| bincode::serialize(&c)
            .unwrap_or_else(|_| panic!("Could not encode {:?}", c)))
        .map(WebsocketMessage::Binary)
        .map(Ok)
        .forward(incoming);
    let rb = outgoing.map(|m|
        match m {
            Ok(WebsocketMessage::Binary(t)) =>
                bincode::deserialize::<ClientMessage>(&t).ok(),
            _ => None,
        })
        .take_while(|m| future::ready(m.is_some()))
        .map(|m| m.unwrap())
        .chain(futures::stream::once(async {
            ClientMessage::Disconnected }))
        .map(move |m| Ok((addr, m)))
        .forward(write);
    let (ra, rb) = join!(ra, rb);

    if let Err(e) = ra {
        error!("[{}] Got error {} from player {}'s rx queue",
               addr, e, player_name);
    }
    if let Err(e) = rb {
        error!("[{}] Got error {} from player {}'s tx queue",
               addr, e, player_name);
    }
    info!("[{}] Finished session with {}", addr, player_name);
}

type RoomList = Arc<Mutex<HashMap<String, RoomHandle>>>;

#[derive(Default)]
struct Room {
    name: String,
    started: bool,
    connections: HashMap<SocketAddr, Side>,
    players: HashMap<Side, Player>,
    active_side: Side,
    game: Game,
}

struct Player {
    name: String,
    ws: Option<UnboundedSender<ServerMessage>>
}

impl Room {
    fn running(&self) -> bool {
        !self.connections.is_empty()
    }

    fn broadcast(&self, msg: ServerMessage) {
        for s in self.connections.values() {
            if let Some(ws) = &self.players.get(s).unwrap().ws {
                if let Err(e) = ws.unbounded_send(msg.clone()) {
                    error!("[{}] Failed to send broadcast to {}: {}",
                           self.name, self.players.get(s).unwrap().name, e);
                }
            }
        }
    }

    fn broadcast_except(&self, s: Side, msg: ServerMessage) {
        for (t, p) in self.players.iter() {
            if s != *t {
                if let Some(ws) = p.ws.as_ref() {
                    if let Err(e) = ws.unbounded_send(msg.clone()) {
                        error!("[{}] Failed to send message to {}: {}",
                               self.name, self.players.get(t).unwrap().name, e);
                    }
                }
            }
        }
    }

    fn send(&self, s: Side, msg: ServerMessage) {
        if let Some(p) = self.players.get(&s).unwrap().ws.as_ref() {
            if let Err(e) = p.unbounded_send(msg) {
                error!("[{}] Failed to send message to {}: {}",
                       self.name, self.players.get(&s).unwrap().name, e);
            }
        } else {
            error!("[{}] Tried sending message to inactive player", self.name);
        }
    }

    fn add_player(&mut self, addr: SocketAddr, player_name: String,
                  ws_tx: UnboundedSender<ServerMessage>) -> Result<bool>
    {
        if self.players.len() == 2 {
            return Ok(false);
        }

        let opponent_side = self.players.iter()
            .next()
            .map(|(&s, _)| s);

        let your_side = opponent_side
            .map(|s| s.opposite())
            .unwrap_or(Side::Bottom);

        self.broadcast(ServerMessage::OpponentJoined(player_name.clone()));

        self.players.insert(your_side, Player {
            name: player_name,
            ws: Some(ws_tx.clone())
        });

        // Add the new player to the active list of connections and players
        self.connections.insert(addr, your_side);

        // The game counts as started once the first player joins
        self.started = true;

        // Tell the player that they have joined the room
        ws_tx.unbounded_send(ServerMessage::JoinedRoom {
                room_name: self.name.clone(),
                opponent: opponent_side.map(|s| self.players.get(&s).unwrap().name.clone()),
                active_side: self.active_side,
                your_side,
                game: self.game.clone(),
            })?;

        Ok(true)
    }

    fn next_player(&mut self) {
        self.active_side = self.active_side.opposite();
    }

    fn on_client_disconnected(&mut self, addr: SocketAddr) {
        if let Some(s) = self.connections.remove(&addr) {
            let player_name = self.players.get(&s).unwrap().name.clone();
            info!("[{}] Removed disconnected player '{}'",
                  self.name, player_name);
            self.players.remove(&s);
            self.broadcast(ServerMessage::OpponentDisconnected);
        } else {
            error!("[{}] Tried to remove non-existent player at {}",
                     self.name, addr);
        }
    }

    fn on_man(&mut self, pos: Position) {
        let player = self.players.get(&self.active_side).unwrap();

        if !self.game.place_man(pos) {
            warn!("[{}] Player {} tried to make an illegal move",
                  self.name, player.name);
            self.send(self.active_side, ServerMessage::MoveRejected);
            return;
        }

        let active_side = self.active_side;
        self.next_player();
        self.send(active_side, ServerMessage::MoveAccepted);
        self.broadcast_except(active_side, ServerMessage::OpponentMoved(Move::Man(pos)));
    }

    fn on_ball(&mut self, jumps: Vec<Position>) {
        let player = self.players.get(&self.active_side).unwrap();

        if self.game.move_ball(jumps.clone()).is_none() {
            warn!("[{}] Player {} tried to make an illegal move",
                  self.name, player.name);
            self.send(self.active_side, ServerMessage::MoveRejected);
            return;
        }

        let active_side = self.active_side;
        self.next_player();
        self.send(active_side, ServerMessage::MoveAccepted);
        self.broadcast_except(active_side, ServerMessage::OpponentMoved(Move::Ball(jumps)));

        if let Some(winner) = self.game.winner() {
            self.broadcast(ServerMessage::ItsOver(winner));
            self.game = Game::default();
            self.active_side = Side::default();
            // switch sides
            self.connections.values_mut().for_each(|side| *side = side.opposite());
            self.players = self.players.drain().map(|(side, player)| (side.opposite(), player)).collect();
            // broadcast new game
            for &side in &[Side::Bottom, Side::Top] {
                self.send(side, ServerMessage::NewGame {
                    active_side: self.active_side,
                    your_side: side,
                    game: self.game.clone(),
                });
            }
        }
    }

    fn on_message(&mut self, addr: SocketAddr, msg: ClientMessage) -> bool {
        trace!("[{}] Got message {:?} from {}", self.name, msg,
                self.connections.get(&addr)
                    .map(|s| self.players.get(s).unwrap().name.clone())
                    .unwrap_or_else(|| format!("unknown player at {}", addr)));
        match msg {
            ClientMessage::Disconnected => self.on_client_disconnected(addr),
            ClientMessage::Chat(c) => {
                let name = self.connections.get(&addr)
                    .map_or("unknown", |s| &self.players.get(s).unwrap().name);
                self.broadcast(ServerMessage::Chat {
                    from: name.to_string(),
                    message: c,
                });
            },
            ClientMessage::CreateRoom(_) | ClientMessage::JoinRoom(_, _) => {
                warn!("[{}] Invalid client message {:?}", self.name, msg);
            },
            ClientMessage::Move(mov) => {
                if let Some(s) = self.connections.get(&addr).copied() {
                    if s == self.active_side {
                        match mov {
                            Move::Man(pos) => self.on_man(pos),
                            Move::Ball(hops) => self.on_ball(hops),
                        }
                    } else {
                        warn!("[{}] Player {} out of turn", self.name, addr);
                    }
                } else {
                    warn!("[{}] Invalid player {}", self.name, addr);
                }
            },
        }
        self.running()
    }
}

fn next_room_name(rooms: &mut HashMap<String, RoomHandle>,
                  handle: RoomHandle) -> String
{
    // This loop should only run once, unless we're starting to saturate the
    // space of possible room names (which is quite large)
    let mut rng = rand::thread_rng();
    loop {
        let room_name = format!("{} {} {}",
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())],
            WORD_LIST[rng.gen_range(0, WORD_LIST.len())]);
        use std::collections::hash_map::Entry;
        if let Entry::Vacant(v) = rooms.entry(room_name.clone()) {
            v.insert(handle);
            return room_name;
        }
    }
}

async fn handle_connection(rooms: RoomList,
                           raw_stream: Async<TcpStream>,
                           addr: SocketAddr,
                           mut close_room: UnboundedSender<String>)
    -> Result<()>
{
    info!("[{}] Incoming TCP connection", addr);

    let mut ws_stream = async_tungstenite::accept_async(raw_stream)
        .await?;
    info!("[{}] WebSocket connection established", addr);

    // Clients are only allowed to send text messages at this stage.
    // If they do anything else, then just disconnect.
    while let Some(Ok(WebsocketMessage::Binary(t))) = ws_stream.next().await {
        let msg = bincode::deserialize::<ClientMessage>(&t)?;

        // Try to interpret their message as joining a room
        match msg {
            ClientMessage::CreateRoom(player_name) => {
                // Log to link address and player name
                info!("[{}] Player {} sent CreateRoom", addr, player_name);

                // We'll funnel all Websocket communication through one
                // MPSC queue per room, with websockets running in their
                // own little tasks writing to the queue.
                let (write, read) = unbounded();

                let room = Arc::new(Mutex::new(Room::default()));
                let handle = RoomHandle { write, room };
                // Lock the global room list for a short time
                let room_name = {
                    let map = &mut rooms.lock().unwrap();
                    next_room_name(map, handle.clone())
                };
                handle.room.lock().unwrap().name = room_name.clone();

                // To avoid spawning a new task, we'll use this task to run
                // both the player's tx/rx queues *and* the room itself.
                let mut h = handle.clone();
                join!(h.run_room(read),
                      run_player(player_name, addr, handle, ws_stream));

                info!("[{}] All players left, closing room.", room_name);
                if let Err(e) = close_room.send(room_name.clone()).await {
                    error!("[{}] Failed to close room: {}", room_name, e);
                }

                return Ok(());
            },
            ClientMessage::JoinRoom(name, room_name) => {
                // Log to link address and player name
                info!("[{}] Player {} sent JoinRoom({})",
                      addr, name, room_name);
                let handle = rooms.lock().unwrap().get_mut(&room_name).cloned();

                if let Some(h) = handle {
                    run_player(name, addr, h, ws_stream).await;
                    return Ok(());
                } else {
                    let msg = ServerMessage::JoinFailed(
                        format!("Could not find room '{}'", room_name));
                    let encoded = bincode::serialize(&msg)?;
                    ws_stream.send(WebsocketMessage::Binary(encoded)).await?;
                }
            }
            // If they send an illegal message, then they obviously have ill
            // intentions and we should disconnect them right now.
            msg => {
                warn!("[{}] Got unexpected message {:?}", addr, msg);
                break;
            }
        }
    }
    info!("[{}] Dropping connection", addr);
    Ok(())
}

fn main() -> Result<(), IoError> {
    env_logger::from_env(Env::default().default_filter_or("pont_server=TRACE"))
        .init();

    // Create an executor thread pool.
    for _ in 0..num_cpus::get().max(1) {
        std::thread::spawn(|| smol::run(future::pending::<()>()));
    }

    let rooms = RoomList::new(Mutex::new(HashMap::new()));

    // Run a small task whose job is to close rooms when the last player leaves.
    // This task accepts room names through a MPSC queue, which all of the
    // room tasks push their names into.
    let close_room = {
        let (tx, mut rx) = unbounded();
        let rooms = rooms.clone();
        Task::spawn(async move {
            while let Some(r) = rx.next().await {
                info!("Closing room [{}]", r);
                rooms.lock().unwrap().remove(&r);
            }
        }).detach();
        tx
    };

    {   // Periodically print the number of open rooms to the logs
        let rooms = rooms.clone();
        Task::spawn(async move {
            let mut prev_count = 0;
            loop {
                Timer::after(Duration::from_secs(60)).await;
                let count = rooms.lock().unwrap().len();
                if count != prev_count {
                    info!("{} rooms open", count);
                    prev_count = count;
                }
            }
        }).detach()
    }

    // The target address + port is optionally specified on the command line
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "0.0.0.0:8080".to_string());

    smol::block_on(async {
        // Create the event loop and TCP listener we'll accept connections on.
        info!("Listening on: {}", addr);
        let listener = Async::<TcpListener>::bind(addr)
            .expect("Could not create listener");

        // The main loop accepts incoming connections asynchronously
        while let Ok((stream, addr)) = listener.accept().await {
            let close_room = close_room.clone();
            let rooms = rooms.clone();
            Task::spawn(async move {
                if let Err(e) = handle_connection(rooms, stream,
                                                  addr, close_room).await
                {
                    error!("Failed to handle connection from {}: {}", addr, e);
                }
            }).detach();
        }
    });

    Ok(())
}
