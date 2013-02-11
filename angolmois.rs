/*
 * Angolmois -- the simple BMS player
 * Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
 * Project Angolmois is copyright (c) 2003-2007, Choi Kaya (CHKY).
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

// This is a direct one-to-one translation of Angolmois which README is
// available in <http://github.com/lifthrasiir/angolmois/>. Angolmois is
// distributed under GNU GPL version 2+, so is this translation. The portions
// of it is intended to be sent as a patch to Rust, so those portions are
// licensed under Apache License 2.0 and MIT license.
//
// Angolmois is a combination of string parsing, path manipulation, two-
// dimensional graphics and complex game play carefully packed into some
// thousand lines of code. This translation is intended to provide an example
// of translating a moderately-sized C code to Rust, and also to investigate
// additional library supports required for such moderately-sized programs.

#[link(name = "angolmois",
       vers = "2.0-alpha2",
       uuid = "0E85EA95-BE62-4E0F-B811-8C1EC46C46EC",
       url = "https://github.com/lifthrasiir/angolmois/")];

#[comment = "Angolmois"];
#[license = "GPLv2+"];

extern mod std;

pub pure fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

//==============================================================================
// utility declarations

pub fn exename() -> ~str {
    let args = os::args();
    if args.is_empty() { ~"angolmois" } else { copy args[0] }
}

pub fn die(s: ~str) -> ! {
    io::stderr().write_line(fmt!("%s: %s", exename(), s));
    unsafe { libc::exit(1); }
}

macro_rules! die(
    ($($e:expr),+) => (::die(fmt!($($e),+)))
)

pub mod util {

    pub mod str {
        // NOTE: these additions will be eventually sent to libcore/str.rs
        // and are not subject to the above copyright notice.

        const tag_cont_u8: u8 = 128u8; // copied from libcore/str.rs

        pub pure fn each_chari_byte(s: &str, it: fn(uint, char) -> bool) {
            let mut pos = 0u;
            let len = s.len();
            while pos < len {
                let str::CharRange {ch, next} = ::str::char_range_at(s, pos);
                if !it(pos, ch) { break; }
                pos = next;
            }
        }

        pub pure fn fix_utf8(v: &[const u8], handler: pure fn(&[const u8])
                                        -> ~[u8]) -> ~[u8] {
            let mut i = 0u;
            let total = vec::len::<u8>(v);
            let mut result = ~[];
            while i < total {
                let chend = i + str::utf8_char_width(v[i]);
                let mut j = i + 1u;
                while j < total && j < chend && v[j] & 192u8 == tag_cont_u8 {
                    j += 1u;
                }
                if j == chend {
                    assert i != chend;
                    result = vec::append(result, v.view(i, j));
                } else {
                    result = vec::append(result, handler(v.view(i, j)));
                }
                i = j;
            }
            result
        }

        pub pure fn fix_utf8_str(s: &str, handler: pure fn(&[const u8]) -> ~str) -> ~str {
            from_fixed_utf8_bytes(str::to_bytes(s), handler)
        }

        pub pure fn from_fixed_utf8_bytes(v: &[const u8],
                                          handler: pure fn(&[const u8]) -> ~str) -> ~str {
            let newhandler: pure fn(&[const u8]) -> ~[u8] =
                |v: &[const u8]| -> ~[u8] { str::to_bytes(handler(v)) };
            let bytes = fix_utf8(v, newhandler);
            unsafe { str::raw::from_bytes(bytes) }
        }

        pub trait StrUtil {
            pure fn slice_to_end(&self, begin: uint) -> ~str;
            pure fn each_chari_byte(self, it: fn(uint, char) -> bool);
            pure fn fix_utf8(self, handler: pure fn(&[const u8]) -> ~str) -> ~str;
        }

        pub impl &str: StrUtil {
            pure fn slice_to_end(&self, begin: uint) -> ~str {
                self.slice(begin, self.len())
            }
            pure fn each_chari_byte(self, it: fn(uint, char) -> bool) {
                each_chari_byte(self, it)
            }
            pure fn fix_utf8(self, handler: pure fn(&[const u8]) -> ~str) -> ~str {
                fix_utf8_str(self, handler)
            }
        }
    }

    pub mod io {
        // NOTE: these additions will be eventually sent to libcore/io.rs
        // and are not subject to the above copyright notice.

        pub trait ReaderUtilEx {
            fn read_and_fix_utf8_line(&self, handler: pure fn(&[const u8]) -> ~str) -> ~str;
            fn each_fixed_utf8_line(&self, handler: pure fn(&[const u8]) -> ~str,
                                    it: fn(&str) -> bool);
        }

        pub impl<T: io::Reader> T: ReaderUtilEx {
            fn read_and_fix_utf8_line(&self, handler: pure fn(&[const u8]) -> ~str) -> ~str {
                let mut bytes = ~[];
                loop {
                    let ch = self.read_byte();
                    if ch == -1 || ch == 10 { break; }
                    bytes.push(ch as u8);
                }
                ::util::str::from_fixed_utf8_bytes(bytes, handler)
            }

            fn each_fixed_utf8_line(&self, handler: pure fn(&[const u8]) -> ~str,
                                    it: fn(&str) -> bool) {
                while !self.eof() {
                    if !it(self.read_and_fix_utf8_line(handler)) { break; }
                }
            }
        }
    }

}

use core::io::{ReaderUtil, WriterUtil};

use util::str::*;
use util::io::*;

//==============================================================================
// bms parser

pub mod parser {

    pub enum Key = int;
    pub const MAXKEY: int = 36*36;
    pub impl Key {
        pure fn is_valid(self) -> bool { let Key(key) = self; 0 <= key && key < MAXKEY }
        pure fn to_hex(self) -> Option<int> {
            let Key(key) = self, sixteens = key / 36, ones = key % 36;
            if sixteens < 16 && ones < 16 { Some(sixteens * 16 + ones) } else { None }
        }
    }

    pub struct BlitCmd {
        dst: Key, src: Key,
        x1: int, y1: int, x2: int, y2: int, dx: int, dy: int
    }

    pub struct Obj {
        time: float,
        chan: int, typ: int, index: int, value: int, nograding: bool
    }

    pub struct Bms {
        mut title: Option<~str>,
        mut genre: Option<~str>,
        mut artist: Option<~str>,
        mut stagefile: Option<~str>,
        mut basepath: Option<~str>,

        mut player: int,
        mut playlevel: Option<int>,
        mut rank: int,
        mut lntype: int,
        mut lnobj: Option<Key>,

        mut initbpm: float,
        sndpath: ~[mut ~str * 1296], // XXX 1296=MAXKEY
        imgpath: ~[mut ~str * 1296], // XXX 1296=MAXKEY
        blitcmd: ~[mut BlitCmd],
        bpmtab: ~[mut float * 1296], // XXX 1296=MAXKEY
        stoptab: ~[mut float * 1296], // XXX 1296=MAXKEY

        mut objs: ~[mut Obj],
        shorten: ~[mut float],
        mut originoffset: float,

        mut keysplit: int,
        keyorder: ~[mut int],
        keykind: ~[mut Option<int> * 72]
    }

    pub pure fn key2index(s: &[char]) -> Option<int> { // requires s[0] and s[1] allocated
        pure fn getdigit(n: char) -> Option<int> {
            match n {
                '0'..'9' => Some((n as int) - ('0' as int)),
                'a'..'z' => Some((n as int) - ('a' as int) + 10),
                'A'..'Z' => Some((n as int) - ('A' as int) + 10),
                _ => None
            }
        }

        match (getdigit(s[0]), getdigit(s[1])) {
            (Some(a), Some(b)) => Some(a * 36 + b),
            _ => None
        }
    }

    pub fn parse_bms(bmspath: &str, r: @rand::Rng) -> Bms {
        let bmsheader = [
            "TITLE", "GENRE", "ARTIST", "STAGEFILE", "PATH_WAV", "BPM", "PLAYER",
            "PLAYLEVEL", "RANK", "LNTYPE", "LNOBJ", "WAV", "BMP", "BGA", "STOP", "STP",
            "RANDOM", "SETRANDOM", "ENDRANDOM", "IF", "ELSEIF", "ELSE", "ENDSW", "END"];

        let f =
            match io::file_reader(&Path(bmspath)) {
                Ok(f) => f,
                Err(err) => die!("Couldn't load BMS file: %s", err)
            };

        enum RndState { Process = 0, Ignore = 1, NoFurther = -1 }
        struct Rnd { val: int, inside: bool, state: RndState, skip: bool }
        let rnd = ~[Rnd { val: 0, inside: false, state: Process, skip: false }];

        let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
        for lines.each |&line| {
            let mut line = ::util::str::from_fixed_utf8_bytes(line, |_| ~"\ufffd");
            line.trim_left();
            if !line.starts_with(~"#") { loop; }

            let mut prefix = "";
            for bmsheader.each |&header| {
                if line.len() > header.len() &&
                   line.substr(1, header.len()).to_upper() == header.to_owned() {
                    prefix = header;
                    break;
                }
            }

            match (prefix, if rnd.last().skip { Ignore } else { rnd.last().state }) {
                ("TITLE", Process) |
                ("GENRE", Process) |
                ("ARTIST", Process) |
                ("STAGEFILE", Process) |
                ("PATH_WAV", Process) => {
                    io::println("TITLE|GENRE|ARTIST|STAGEFILE|PATH_WAV");
                }
                ("BPM", Process) => {
                    io::println("BPM");
                }
                ("PLAYER", Process) |
                ("PLAYLEVEL", Process) |
                ("RANK", Process) |
                ("LNTYPE", Process) => {
                    io::println("PLAYER|PLAYLEVEL|RANK|LNTYPE");
                }
                ("LNOBJ", Process) => {
                    io::println("LNOBJ");
                }
                ("WAV", Process) |
                ("BMP", Process) => {
                    io::println("WAV|BMP");
                }
                ("BGA", Process) => {
                    io::println("BGA");
                }
                ("STOP", Process) => {
                    io::println("STOP");
                }
                ("STP", Process) => {
                    io::println("STP");
                }
                ("RANDOM", _) |
                ("SETRANDOM", _) => {
                    io::println("RANDOM|SETRANDOM");
                }
                ("ENDRANDOM", _) => {
                    io::println("ENDRANDOM");
                }
                ("IF", _) |
                ("ELSEIF", _) => {
                    io::println("IF|ELSEIF");
                }
                ("ELSE", _) => {
                    io::println("ELSE");
                }
                ("END", _) => {
                    io::println("END");
                }
                ("", Process) => {
                    io::println("##### (possibly)");
                }
                (_, _) => ()
            }
        }

        fail!(~"TODO");
    }

}

//==============================================================================
// game play

pub mod player {

    pub enum Mode { PlayMode, AutoPlayMode, ExclusiveMode }
    pub enum Modf { NoModf, MirrorModf, ShuffleModf, ShuffleExModf, RandomModf, RandomExModf }
    pub enum Bga { BgaAndMovie, BgaButNoMovie, NoBga }

    pub struct Options {
        mut bmspath: Option<~str>,
        mut mode: Mode,
        mut modf: Modf,
        mut bga: Bga,
        mut showinfo: bool,
        mut fullscreen: bool,
        mut joystick: Option<int>,
        mut preset: Option<~str>,
        mut leftkeys: Option<~str>,
        mut rightkeys: Option<~str>,
        mut playspeed: float,
    }

    pub impl Options {
        static pure fn new() -> ~Options {
            ~Options { bmspath: None, mode: PlayMode, modf: NoModf, bga: BgaAndMovie,
                       showinfo: true, fullscreen: true, joystick: None,
                       preset: None, leftkeys: None, rightkeys: None, playspeed: 1.0 }
        }
    }

    use core::repr;
    pub fn play(opts: &Options) {
        repr::write_repr(io::stdout(), opts);
        io::println("");

        let r = rand::task_rng();
        match opts.bmspath {
            Some(copy path) => ::parser::parse_bms(path, r),
            None => fail!(~"TODO")
        };
    }

}

//==============================================================================
// entry point

fn usage() {
    io::stderr().write_str(fmt!("\
%s -- the simple BMS player
http://mearie.org/projects/angolmois/

Usage: %s <options> <path>
  Accepts any BMS, BME, BML or PMS file.
  Resources should be in the same directory as the BMS file.

Options:
  -h, --help              This help
  -V, --version           Shows the version
  -a #.#, --speed #.#     Sets the initial play speed (default: 1.0x)
  -#                      Same as '-a #.0'
  -v, --autoplay          Enables AUTO PLAY (viewer) mode
  -x, --exclusive         Enables exclusive (BGA and sound only) mode
  -X, --sound-only        Enables sound only mode, equivalent to -xB
  --fullscreen            Enables the fullscreen mode (default)
  -w, --no-fullscreen     Disables the fullscreen mode
  --info                  Shows a brief information about the song (default)
  -q, --no-info           Do not show an information about the song
  -m, --mirror            Uses a mirror modifier
  -s, --shuffle           Uses a shuffle modifier
  -S, --shuffle-ex        Uses a shuffle modifier, even for scratches
  -r, --random            Uses a random modifier
  -R, --random-ex         Uses a random modifier, even for scratches
  -k NAME, --preset NAME  Forces a use of given key preset (default: bms)
  -K LEFT RIGHT, --key-spec LEFT RIGHT
                          Sets a custom key specification (see the manual)
  --bga                   Loads and shows the BGA (default)
  -B, --no-bga            Do not load and show the BGA
  -M, --no-movie          Do not load and show the BGA movie
  -j #, --joystick #      Enable the joystick with index # (normally 0)

Environment Variables:
  ANGOLMOIS_1P_KEYS=<scratch>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<pedal>
  ANGOLMOIS_2P_KEYS=<pedal>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<scratch>
  ANGOLMOIS_PMS_KEYS=<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<8>|<9>
  ANGOLMOIS_SPEED_KEYS=<speed down>|<speed up>
  ANGOLMOIS_XXy_KEY=<keys for channel XX and channel kind y>
    Sets keys used for game play. Use either SDL key names or joystick names
    like 'button #' or 'axis #' can be used. Separate multiple keys by '%%'.
    See the manual for more information.

", version(), exename()));
    unsafe { libc::exit(1); }
}

fn main() {
    use player::*;

    let longargs = std::oldmap::hash_from_vec::<~str,char>([
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'), (~"--fullscreen", ' '),
        (~"--info", ' '), (~"--no-info", 'q'), (~"--mirror", 'm'),
        (~"--shuffle", 's'), (~"--shuffle-ex", 'S'), (~"--random", 'r'),
        (~"--random-ex", 'R'), (~"--preset", 'k'), (~"--key-spec", 'K'),
        (~"--bga", ' '), (~"--no-bga", 'B'), (~"--movie", ' '),
        (~"--no-movie", 'M'), (~"--joystick", 'j'),
    ]);

    let args = os::args();
    let nargs = args.len();
    let opts = Options::new();

    let mut i = 1;
    while i < nargs {
        if !args[i].starts_with("-") {
            if opts.bmspath.is_none() { opts.bmspath = Some(copy args[i]); }
        } else if args[i] == ~"--" {
            i += 1;
            if opts.bmspath.is_none() && i < nargs { opts.bmspath = Some(copy args[i]); }
            break;
        } else {
            let shortargs =
                if args[i].starts_with("--") {
                    match longargs.find(&args[i]) {
                        Some(c) => str::from_char(c),
                        None => die!("Invalid option: %s", args[i])
                    }
                } else {
                    args[i].slice_to_end(1)
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for shortargs.each_chari_byte |j, c| {
                let fetch_arg = |opt| {
                    let off = if inside { j + 1 } else { j };
                    let nextarg =
                        if inside && off < nshortargs {
                            shortargs.slice_to_end(off)
                        } else {
                            i += 1;
                            if i < nargs {
                                copy args[i]
                            } else {
                                die!("No argument to the option -%c", opt);
                            }
                        };
                    inside = false;
                    nextarg
                };

                match c {
                    'h' => { usage(); },
                    'V' => { io::println(version()); return; },
                    'v' => { opts.mode = AutoPlayMode; },
                    'x' => { opts.mode = ExclusiveMode; },
                    'X' => { opts.mode = ExclusiveMode; opts.bga = NoBga; },
                    'w' => { opts.fullscreen = false; },
                    'q' => { opts.showinfo = false; },
                    'm' => { opts.modf = MirrorModf; },
                    's' => { opts.modf = ShuffleModf; },
                    'S' => { opts.modf = ShuffleExModf; },
                    'r' => { opts.modf = RandomModf; },
                    'R' => { opts.modf = RandomExModf; },
                    'k' => { opts.preset = Some(fetch_arg('k')); },
                    'K' => {
                        opts.leftkeys = Some(fetch_arg('K'));
                        opts.rightkeys = Some(fetch_arg('K'));
                    },
                    'a' => match float::from_str(fetch_arg('a')) {
                        Some(speed) if speed > 0.0 => {
                            opts.playspeed =
                                if speed < 0.1 { 0.1 }
                                else if speed > 99.0 { 99.0 }
                                else { speed };
                        },
                        _ => die!("Invalid argument to option -a")
                    },
                    'B' => { opts.bga = NoBga; },
                    'M' => { opts.bga = BgaButNoMovie; },
                    'j' => match int::from_str(fetch_arg('j')) {
                        Some(idx) if idx >= 0 => { opts.joystick = Some(idx); },
                        _ => die!("Invalid argument to option -j")
                    },
                    ' ' => {}, // for ignored long options
                    '1'..'9' => { opts.playspeed = char::to_digit(c, 10).get() as float; },
                    _ => die!("Invalid option: -%c", c),
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    //if opts.bmspath.is_none() { opts.bmspath = filedialog(); }
    if opts.bmspath.is_none() { usage(); } else { play(opts); }
}