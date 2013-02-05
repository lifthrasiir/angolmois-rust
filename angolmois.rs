extern mod std;

pub pure fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

mod util {

    trait StrUtil {
        pure fn slice_to_end(begin: uint) -> ~str;
        pure fn each_chari_byte(it: fn(uint, char) -> bool);
    }

    impl &str: StrUtil {
        pure fn slice_to_end(begin: uint) -> ~str { self.slice(begin, self.len()) }
        pure fn each_chari_byte(it: fn(uint, char) -> bool) {
            let mut pos = 0u;
            let len = self.len();
            while pos < len {
                let str::CharRange {ch, next} = str::char_range_at(self, pos);
                if !it(pos, ch) { break; }
                pos = next;
            }
        }
    }

}

use core::io::{ReaderUtil, WriterUtil};
use util::*;

////////////////////////////////////////////////////////////////////////////////

pub mod parser {

    pub enum Key = int;
    pub const MAXKEY: int = 36*36;
    pub impl Key {
        pure fn is_valid() -> bool { let Key(key) = self; 0 <= key && key < MAXKEY }
        pure fn to_hex() -> Option<int> {
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
        let f =
            match io::file_reader(&Path(bmspath)) {
                Ok(f) => f,
                Err(err) => fail fmt!("Couldn't load BMS file %s: %s", bmspath, err)
            };

        enum RndState { Process = 0, Ignore = 1, NoFurther = -1 }
        struct Rnd { val: int, inside: bool, state: RndState, skip: bool }
        let rnd = ~[Rnd { val: 0, inside: false, state: Process, skip: false }];

        while !f.eof() {
            let line = f.read_line();
            io::println(line);
        }

        fail ~"TODO";
    }

}

////////////////////////////////////////////////////////////////////////////////

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

    pub fn play(opts: &Options) {
        repr::write_repr(io::stdout(), opts);
        io::println("");

        let r = rand::task_rng();
        match opts.bmspath {
            Some(copy path) => parser::parse_bms(path, r),
            None => fail ~"TODO"
        };
    }

}

////////////////////////////////////////////////////////////////////////////////

fn usage() {
    let args = os::args();
    let exename = if args.is_empty() { ~"angolmois" } else { copy args[0] };
    io::stderr().write_str(
        version() + ~" -- the simple BMS player\n"
        + ~"http://mearie.org/projects/angolmois/\n\n"
        + ~"Usage: " + exename + ~" <options> <path>\n"
        + ~"  Accepts any BMS, BME, BML or PMS file.\n"
        + ~"  Resources should be in the same directory as the BMS file.\n\n"
        + ~"Options:\n"
        + ~"  -h, --help              This help\n"
        + ~"  -V, --version           Shows the version\n"
        + ~"  -a #.#, --speed #.#     Sets the initial play speed (default: 1.0x)\n"
        + ~"  -#                      Same as '-a #.0'\n"
        + ~"  -v, --autoplay          Enables AUTO PLAY (viewer) mode\n"
        + ~"  -x, --exclusive         Enables exclusive (BGA and sound only) mode\n"
        + ~"  -X, --sound-only        Enables sound only mode, equivalent to -xB\n"
        + ~"  --fullscreen            Enables the fullscreen mode (default)\n"
        + ~"  -w, --no-fullscreen     Disables the fullscreen mode\n"
        + ~"  --info                  Shows a brief information about the song (default)\n"
        + ~"  -q, --no-info           Do not show an information about the song\n"
        + ~"  -m, --mirror            Uses a mirror modifier\n"
        + ~"  -s, --shuffle           Uses a shuffle modifier\n"
        + ~"  -S, --shuffle-ex        Uses a shuffle modifier, even for scratches\n"
        + ~"  -r, --random            Uses a random modifier\n"
        + ~"  -R, --random-ex         Uses a random modifier, even for scratches\n"
        + ~"  -k NAME, --preset NAME  Forces a use of given key preset (default: bms)\n"
        + ~"  -K LEFT RIGHT, --key-spec LEFT RIGHT\n"
        + ~"                          Sets a custom key specification (see the manual)\n"
        + ~"  --bga                   Loads and shows the BGA (default)\n"
        + ~"  -B, --no-bga            Do not load and show the BGA\n"
        + ~"  -M, --no-movie          Do not load and show the BGA movie\n"
        + ~"  -j #, --joystick #      Enable the joystick with index # (normally 0)\n\n"
        + ~"Environment Variables:\n"
        + ~"  ANGOLMOIS_1P_KEYS=<scratch>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<pedal>\n"
        + ~"  ANGOLMOIS_2P_KEYS=<pedal>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<scratch>\n"
        + ~"  ANGOLMOIS_PMS_KEYS=<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<8>|<9>\n"
        + ~"  ANGOLMOIS_SPEED_KEYS=<speed down>|<speed up>\n"
        + ~"  ANGOLMOIS_XXy_KEY=<keys for channel XX and channel kind y>\n"
        + ~"    Sets keys used for game play. Use either SDL key names or joystick names\n"
        + ~"    like 'button #' or 'axis #' can be used. Separate multiple keys by '%%'.\n"
        + ~"    See the manual for more information.\n\n");
    os::set_exit_status(1);
}

fn main() {
    use player::*;

    let longargs = std::map::hash_from_vec::<&str,char>([
        ("--help", 'h'), ("--version", 'V'), ("--speed", 'a'),
        ("--autoplay", 'v'), ("--exclusive", 'x'), ("--sound-only", 'X'),
        ("--windowed", 'w'), ("--no-fullscreen", 'w'), ("--fullscreen", ' '),
        ("--info", ' '), ("--no-info", 'q'), ("--mirror", 'm'),
        ("--shuffle", 's'), ("--shuffle-ex", 'S'), ("--random", 'r'),
        ("--random-ex", 'R'), ("--preset", 'k'), ("--key-spec", 'K'),
        ("--bga", ' '), ("--no-bga", 'B'), ("--movie", ' '),
        ("--no-movie", 'M'), ("--joystick", 'j'),
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
                    match longargs.find(args[i]) {
                        Some(c) => str::from_char(c),
                        None => fail fmt!("Invalid option: %s", args[i])
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
                                fail fmt!("No argument to the option -%c", opt);
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
                        _ => fail fmt!("Invalid argument to option -a")
                    },
                    'B' => { opts.bga = NoBga; },
                    'M' => { opts.bga = BgaButNoMovie; },
                    'j' => match int::from_str(fetch_arg('j')) {
                        Some(idx) if idx >= 0 => { opts.joystick = Some(idx); },
                        _ => fail fmt!("Invalid argument to option -j")
                    },
                    ' ' => {}, // for ignored long options
                    '1'..'9' => { opts.playspeed = char::to_digit(c, 10).get() as float; },
                    _ => fail fmt!("Invalid option: -%c", c),
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    //if opts.bmspath.is_none() { opts.bmspath = filedialog(); }
    if opts.bmspath.is_none() { usage(); } else { play(opts); }
}