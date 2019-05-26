#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
    FrameComplete,
}

#[derive(Clone, Debug)]
pub enum FrameState {
    Init,
    Open(u16),
    Count(u16),
    Spare,
    Strike,
}

impl FrameState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn points(&self) -> u16 {
        use FrameState::*;
        match *self {
            Init => 0,
            Open(c) | Count(c) => 10 - c,
            Spare | Strike => 10,
        }
    }
}

impl Default for FrameState {
    fn default() -> Self {
        FrameState::Init
    }
}

#[derive(Debug, Default)]
pub struct Frame {
    state: FrameState,
}

impl Frame {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn is_complete(&self) -> bool {
        use FrameState::*;
        match self.state {
            Spare | Strike | Count(_) => true,
            _ => false,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        use FrameState::*;
        match self.state {
            Init if pins < 10 => self.state = Open(10 - pins),
            Init if pins == 10 => self.state = Strike,
            Init => return Err(Error::NotEnoughPinsLeft),
            Open(pins_left) if pins < pins_left => self.state = Count(pins_left - pins),
            Open(pins_left) if pins == pins_left => self.state = Spare,
            Open(_) => return Err(Error::NotEnoughPinsLeft),
            Spare | Strike | Count(_) => return Err(Error::FrameComplete),
        };
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct FillFrame {
    state: FrameState,
}

impl FillFrame {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn is_complete(&self) -> bool {
        use FrameState::*;
        match self.state {
            Spare | Strike | Count(_) => true,
            _ => false,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        use FrameState::*;
        match self.state {
            Init if pins < 10 => self.state = Count(10 - pins),
            Init if pins == 10 => self.state = Strike,
            Init => return Err(Error::NotEnoughPinsLeft),
            Spare | Strike | Count(_) => return Err(Error::FrameComplete),
            Open(_) => unreachable!(),
        };
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct BowlingGame {
    frames: [Frame; 10],
    fill_frame: FillFrame,
}

impl BowlingGame {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn frame_states(&self) -> Vec<FrameState> {
        let mut vec: Vec<FrameState> = self.frames.iter().map(|frame| frame.state.clone()).collect();
        if self.has_fill_frame() {
            vec.push(self.fill_frame.state.clone());
        }
        vec
    }

    fn has_fill_frame(&self) -> bool {
        match self.frames[9].state {
            FrameState::Strike | FrameState::Spare => true,
            _ => false,
        }
    }

    fn is_complete(&self) -> bool {
        self.frames.iter().all(|frame| frame.is_complete()) && (!self.has_fill_frame() || self.fill_frame.is_complete())
    }

    fn next_frame_mut(&mut self) -> Option<&mut Frame> {
        for frame in self.frames.iter_mut() {
            if !frame.is_complete() {
                return Some(frame);
            }
        }
        None
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if let Some(frame) = self.next_frame_mut() {
            frame.roll(pins)
        } else if self.has_fill_frame() && !self.fill_frame.is_complete() {
            self.fill_frame.roll(pins)
        } else {
            Err(Error::GameComplete)
        }
    }

    pub fn score(&self) -> Option<u16> {
        if self.is_complete() {
            let fs = self.frame_states();
            for i in 0..fs.len() {
                if fs[i] == FrameState::Strike {
                    
                }
                fs[i].score()
            }
            let i = 0_usize;
            while i < fs.len() {

                i += 1;
            }
            Some(10)
        } else {
            None
        }
    }
}
