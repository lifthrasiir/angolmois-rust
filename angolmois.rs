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
//
// Unlike the original Angolmois code (which sacrifices most comments due to
// code size concerns), the Rust version has much more comments which can be
// beneficial for understanding Angolmois itself too.
//
// Key:
// * (C: ...) - variable/function corresponds to given name in the C code.
// * Rust: ... - suboptimal translation with a room for improvement in Rust.
//   often contains a Rust issue number like #1234.

#[link(name = "angolmois",
       vers = "2.0-alpha2",
       uuid = "0E85EA95-BE62-4E0F-B811-8C1EC46C46EC",
       url = "https://github.com/lifthrasiir/angolmois/")];

#[comment = "Angolmois"];
#[license = "GPLv2+"];

extern mod std;
extern mod sdl;

/// (C: `VERSION`)
pub pure fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

//============================================================================
// utility declarations

/// Returns an executable name used in the command line if any. (C: `argv0`)
pub fn exename() -> ~str {
    let args = os::args();
    if args.is_empty() { ~"angolmois" } else { copy args[0] }
}

/// Immediately terminates the program with given exit code.
pub fn exit(exitcode: int) -> ! {
    // Rust: `os::set_exit_status` doesn't immediately terminate the program.
    unsafe { libc::exit(exitcode as libc::c_int); }
}

/// Exits with an error message. Internally used in the `die` macro below.
pub fn die(s: ~str) -> ! {
    io::stderr().write_line(fmt!("%s: %s", exename(), s));
    exit(1)
}

// Exits with a formatted error message. (C: `die`)
//
// Rust: this comment cannot be a doc comment (yet).
macro_rules! die(
    ($($e:expr),+) => (::die(fmt!($($e),+)))
)

/// Utility functions.
pub mod util {

    /// String utilities for Rust. Parallels to `core::str`.
    ///
    /// NOTE: Some of these additions will be eventually sent to
    /// libcore/str.rs and are not subject to the above copyright notice.
    pub mod str {

        const tag_cont_u8: u8 = 128u8; // copied from libcore/str.rs

        /// Iterates over the chars in a string, with byte indices.
        pub pure fn each_chari_byte(s: &str, it: fn(uint, char) -> bool) {
            let mut pos = 0u;
            let len = s.len();
            while pos < len {
                let str::CharRange {ch, next} = ::str::char_range_at(s, pos);
                if !it(pos, ch) { break; }
                pos = next;
            }
        }

        /// Given a potentially invalid UTF-8 byte sequence, fixes an invalid
        /// UTF-8 sequence with given error handler.
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

        /// Given a potentially invalid UTF-8 string, fixes an invalid
        /// UTF-8 string with given error handler.
        pub pure fn fix_utf8_str(s: &str, handler: pure fn(&[const u8])
                                        -> ~str) -> ~str {
            from_fixed_utf8_bytes(str::to_bytes(s), handler)
        }

        /// Converts a vector of bytes to a UTF-8 string. Any invalid UTF-8
        /// sequences are fixed with given error handler.
        pub pure fn from_fixed_utf8_bytes(v: &[const u8],
                                          handler: pure fn(&[const u8])
                                                   -> ~str) -> ~str {
            let newhandler: pure fn(&[const u8]) -> ~[u8] =
                |v: &[const u8]| -> ~[u8] { str::to_bytes(handler(v)) };
            let bytes = fix_utf8(v, newhandler);
            unsafe { str::raw::from_bytes(bytes) }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `uint::from_str` accepts without a failure, if any.
        pub pure fn scan_uint(s: &str) -> Option<uint> {
            match str::find(s, |c| !('0' <= c && c <= '9')) {
                Some(first) if first > 0u => Some(first), _ => None
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `int::from_str` accepts without a failure, if any.
        pub pure fn scan_int(s: &str) -> Option<uint> {
            if s.starts_with(~"-") || s.starts_with(~"+") {
                scan_uint(s.slice_to_end(1u)).map(|pos| pos + 1u)
            } else {
                scan_uint(s)
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `float::from_str` accepts without a failure, if any.
        pub pure fn scan_float(s: &str) -> Option<uint> {
            do scan_int(s).chain_ref |&pos| {
                if s.len() >= pos && s.char_at(pos) == '.' {
                    let pos2 = scan_uint(s.slice_to_end(pos + 1u));
                    pos2.map(|&pos2| pos + pos2 + 1u)
                } else {
                    Some(pos)
                }
            }
        }

        /// Extensions to `str`.
        pub trait StrUtil {
            /// Returns a slice of the given string starting from `begin`.
            ///
            /// # Failure
            ///
            /// If `begin` does not point to valid characters or beyond
            /// the last character of the string
            pure fn slice_to_end(self, begin: uint) -> ~str;

            /// Iterates over the chars in a string, with byte indices.
            pure fn each_chari_byte(self, it: fn(uint, char) -> bool);

            /// Given a potentially invalid UTF-8 string, fixes an invalid
            /// UTF-8 string with given error handler.
            pure fn fix_utf8(self, handler: pure fn(&[const u8]) -> ~str)
                                        -> ~str;

            /// Returns a length of the longest prefix of given string, which
            /// `uint::from_str` accepts without a failure, if any.
            pure fn scan_uint(self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which
            /// `int::from_str` accepts without a failure, if any.
            pure fn scan_int(self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which
            /// `float::from_str` accepts without a failure, if any.
            pure fn scan_float(self) -> Option<uint>;
        }

        impl StrUtil for &str {
            pure fn slice_to_end(self, begin: uint) -> ~str {
                self.slice(begin, self.len())
            }
            pure fn each_chari_byte(self, it: fn(uint, char) -> bool) {
                each_chari_byte(self, it)
            }
            pure fn fix_utf8(self, handler: pure fn(&[const u8]) -> ~str)
                                        -> ~str {
                fix_utf8_str(self, handler)
            }
            pure fn scan_uint(self) -> Option<uint> { scan_uint(self) }
            pure fn scan_int(self) -> Option<uint> { scan_int(self) }
            pure fn scan_float(self) -> Option<uint> { scan_float(self) }
        }

        /// A trait which provides `prefix_shifted` method. Similar to
        /// `str::starts_with`, but with swapped `self` and argument.
        pub trait ShiftablePrefix {
            /// Returns a slice of given string with `self` at the start of
            /// the string stripped only once, if any.
            pure fn prefix_shifted(&self, s: &str) -> Option<~str>;
        }

        impl ShiftablePrefix for char {
            pure fn prefix_shifted(&self, s: &str) -> Option<~str> {
                if !s.is_empty() {
                     let str::CharRange {ch, next} = str::char_range_at(s, 0u);
                     if ch == *self { Some(s.slice_to_end(next)) } else { None }
                } else {
                    None
                }
            }
        }

        impl ShiftablePrefix for &str {
            pure fn prefix_shifted(&self, s: &str) -> Option<~str> {
                if s.starts_with(*self) {
                    Some(s.slice_to_end(self.len()))
                } else {
                    None
                }
            }
        }

    }

    /// I/O utilities for Rust. Parallels to `core::io`.
    ///
    /// NOTE: Some of these additions will be eventually sent to
    /// libcore/io.rs and are not subject to the above copyright notice.
    pub mod io {

        /// Extensions to `ReaderUtil`.
        pub trait ReaderUtilEx {
            /// Reads up until the first '\n' char (which is not returned),
            /// or EOF. Any invalid UTF-8 sequences are fixed with given
            /// error handler.
            fn read_and_fix_utf8_line(&self, handler: pure fn(&[const u8])
                                        -> ~str) -> ~str;

            /// Iterates over every line until the iterator breaks or EOF. Any
            /// invalid UTF-8 sequences are fixed with given error handler.
            fn each_fixed_utf8_line(&self, handler: pure fn(&[const u8])
                                        -> ~str, it: fn(&str) -> bool);
        }

        impl<T: io::Reader> ReaderUtilEx for T {
            fn read_and_fix_utf8_line(&self, handler: pure fn(&[const u8])
                                        -> ~str) -> ~str {
                let mut bytes = ~[];
                loop {
                    let ch = self.read_byte();
                    if ch == -1 || ch == 10 { break; }
                    bytes.push(ch as u8);
                }
                ::util::str::from_fixed_utf8_bytes(bytes, handler)
            }

            fn each_fixed_utf8_line(&self, handler: pure fn(&[const u8])
                                        -> ~str, it: fn(&str) -> bool) {
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

//============================================================================
// bms parser

/// BMS parser module.
pub mod parser {

    /// Two-letter alphanumeric identifier (henceforth "key") used for
    /// virtually everything, including resource management, variable BPM and
    /// chart specification.
    #[deriving_eq]
    pub enum Key = int;

    /// The number of all possible keys. (C: `MAXKEY`)
    pub const MAXKEY: int = 36*36;

    pub impl Key {
        /// Returns if the key is in the proper range. Angolmois supports
        /// the full range of 00-ZZ (0-1295) for every case.
        pure fn is_valid(self) -> bool {
            // Rust: `self as int` is handy.
            let Key(key) = self; 0 <= key && key < MAXKEY
        }

        /// Re-reads the key as a hexadecimal number if possible. This is
        /// required due to handling of channel #03 (BPM is expected to be
        /// in hexadecimal).
        pure fn to_hex(self) -> Option<int> {
            let Key(key) = self, sixteens = key / 36, ones = key % 36;
            if sixteens < 16 && ones < 16 { Some(sixteens * 16 + ones) }
            else { None }
        }
    }

    impl ToStr for Key {
        /// Returns a two-letter representation of key. (C: `TO_KEY`)
        pure fn to_str(&self) -> ~str {
            assert self.is_valid();
            let Key(key) = *self;
            let map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            fmt!("%c%c", map[key / 36] as char, map[key % 36] as char)
        }
    }

    #[deriving_eq]
    pub struct Lane {
        player: uint,
        channel: uint
    }

    /// Sound reference.
    #[deriving_eq]
    pub enum SoundRef = Key;

    /// Image reference.
    #[deriving_eq]
    pub enum ImageRef = Key;

    /// BGA layers. (C: `enum BGA_type`)
    ///
    /// Rust: why not #[deriving_cmp]?
    #[deriving_eq]
    pub enum BGALayer {
        /// The lowest layer. BMS channel #04. (C: `BGA_LAYER`)
        Layer1,
        /// The middle layer. BMS channel #07. (C: `BGA2_LAYER`)
        Layer2,
        /// The highest layer. BMS channel #0A. (C: `BGA3_LAYER`)
        Layer3,
        /// The layer only displayed shortly after the MISS grade. BMS channel
        /// #06. (C: `POORBGA_LAYER`)
        PoorBGA
    }

    pub enum BPM = float;
    pub enum Duration { Seconds(float), Measures(float) }
    pub enum Damage { GaugeDamage(float), InstantDeath }

    pub enum ObjData {
        /// Visible note. Sound is played when the key is input inside the
        /// associated grading area. (C: `NOTE`)
        Visible(Lane, Option<SoundRef>),
        /// Invisible note. Sound is played when the key is input inside the
        /// associated grading area. No render nor grading performed.
        /// (C: `INVNOTE`)
        Invisible(Lane, Option<SoundRef>),
        /// Start of long note. Sound is played when the key is down inside
        /// the associated grading area. (C: `LNSTART`)
        LNStart(Lane, Option<SoundRef>),
        /// End of long note. Sound is played when the start of long note is
        /// graded, the key was down and now up inside the associated grading
        /// area. (C: `LNDONE`)
        LNDone(Lane, Option<SoundRef>),
        /// Bomb. Pressing the key down at the moment that the object is
        /// on time causes the specified damage; sound is played in this case.
        /// No associated grading area. (C: `BOMB`)
        Bomb(Lane, Option<SoundRef>, Damage),
        /// Plays associated sound. (C: `BGM_CHANNEL`)
        BGM(SoundRef),
        /// Sets the virtual BGA layer to given image. The layer itself may
        /// not be displayed depending on the current game status.
        /// (C: `BGA_CHANNEL`)
        ///
        /// If the reference points to a movie, the movie starts playing; if
        /// the other layer had the same movie started, it rewinds to
        /// the beginning. The resulting image from the movie can be shared
        /// among multiple layers.
        SetBGA(BGALayer, Option<ImageRef>),
        /// Sets the BPM. Negative BPM causes the chart scrolls backwards
        /// (and implicitly signals the end of the chart). (C: `BPM_CHANNEL`)
        SetBPM(BPM),
        /// Stops the scroll of the chart for given duration.
        /// (C: `STOP_CHANNEL`)
        Stop(Duration)
    }

    impl ObjData {
        pub pure fn sounds(self) -> ~[SoundRef] {
            match self {
                Visible(_,Some(sref)) => ~[sref],
                Invisible(_,Some(sref)) => ~[sref],
                LNStart(_,Some(sref)) => ~[sref],
                LNDone(_,Some(sref)) => ~[sref],
                Bomb(_,Some(sref),_) => ~[sref],
                BGM(sref) => ~[sref],
                _ => ~[]
            }
        }

        pub pure fn keydown_sound(self) -> Option<SoundRef> {
            match self {
                Visible(_,sref) | Invisible(_,sref) | LNStart(_,sref) => sref,
                _ => None
            }
        }

        pub pure fn keyup_sound(self) -> Option<SoundRef> {
            match self {
                LNDone(_,sref) => sref,
                _ => None
            }
        }

        pub pure fn through_sound(self) -> Option<SoundRef> {
            match self {
                Bomb(_,sref,_) => sref,
                _ => None
            }
        }

        pub pure fn images(self) -> ~[ImageRef] {
            match self {
                SetBGA(_,Some(iref)) => ~[iref],
                _ => ~[]
            }
        }

        pub pure fn through_damage(self) -> Option<Damage> {
            match self {
                Bomb(_,_,damage) => Some(damage),
                _ => None
            }
        }
    }

    pub struct Obj {
        time: float, data: ObjData
    }

    pub const DefaultBPM: float = 130.0;

    /// Blit commands, which manipulate the image after the image had been
    /// loaded. This maps to BMS #BGA command. (C: `struct blitcmd`)
    pub struct BlitCmd {
        /// Destination surface
        dst: ImageRef,
        /// Source surface
        src: ImageRef,
        x1: int, y1: int,
        x2: int, y2: int, dx: int, dy: int,
    }

    pub const SinglePlay: int = 1;
    pub const CouplePlay: int = 2;
    pub const DoublePlay: int = 3;

    /// Loaded BMS data. It is not a global state unlike C.
    pub struct Bms {
        /// Title. Maps to BMS #TITLE command. (C: `string[S_TITLE]`)
        title: Option<~str>,
        /// Genre. Maps to BMS #GENRE command. (C: `string[S_GENRE]`)
        genre: Option<~str>,
        /// Artist. Maps to BMS #ARTIST command. (C: `string[S_ARTIST]`)
        artist: Option<~str>,
        /// Path to an image for loading screen. Maps to BMS #STAGEFILE
        /// command. (C: `string[S_STAGEFILE]`)
        stagefile: Option<~str>,
        /// A base path used for loading all other resources. Maps to BMS
        /// #PATH_WAV command. (C: `string[S_BASEPATH]`)
        basepath: Option<~str>,

        /// Game mode. One of `SinglePlay`(1), `CouplePlay`(2) or
        /// `DoublePlay`(3). Maps to BMS #PLAYER command.
        /// (C: `value[V_PLAYER]`)
        player: int,
        /// Game level. Does not affect the actual game play. Maps to BMS
        /// #PLAYLEVEL command. (C: `value[V_PLAYLEVEL]`)
        playlevel: int,
        /// Gauge difficulty. Higher is easier. Maps to BMS #RANK command.
        /// (C: `value[V_RANK]`)
        rank: int,
        /// Allows long notes to be specified as a consecutive row of same
        /// or non-zero keys (MGQ type, #LNTYPE 2). The default is to specify
        /// long notes as two endpoints (RDM type, #LNTYPE 1).
        /// (C: `value[V_LNTYPE]`)
        consecutiveln: bool,
        /// An end-of-long-note marker used in long-note specification for
        /// channels #1x/2x. Maps to BMS #LNOBJ command. (C: `value[V_LNOBJ]`)
        lnobj: Option<Key>,

        /// Initial BPM. (C: `initbpm`)
        initbpm: float,
        /// Paths to sound file relative to `basepath` or BMS file.
        /// (C: `sndpath`)
        ///
        /// Rust: constant expression in the array size is unsupported.
        sndpath: ~[Option<~str> * 1296], // 1296=MAXKEY
        /// Paths to image/movie file relative to `basepath` or BMS file.
        /// (C: `imgpath`)
        imgpath: ~[Option<~str> * 1296], // 1296=MAXKEY
        /// List of blit commands to be executed after `imgpath` is loaded.
        /// (C: `blitcmd`)
        blitcmd: ~[BlitCmd],
        /// (C: `bpmtab`)
        bpmtab: ~[float * 1296], // 1296=MAXKEY
        /// (C: `stoptab`)
        stoptab: ~[float * 1296], // 1296=MAXKEY

        objs: ~[Obj],
        shorten: ~[float],
        originoffset: float,

        keysplit: int,
        keyorder: ~[int],
        keykind: ~[Option<int> * 72]
    }

    /// Creates a default value of BMS data.
    pub pure fn Bms() -> Bms {
        Bms { title: None, genre: None, artist: None, stagefile: None,
              basepath: None, player: SinglePlay, playlevel: 0, rank: 2,
              consecutiveln: false, lnobj: None, initbpm: DefaultBPM,
              sndpath: ~[None, ..1296], imgpath: ~[None, ..1296],
              blitcmd: ~[], bpmtab: ~[DefaultBPM, ..1296],
              stoptab: ~[0.0, ..1296], objs: ~[], shorten: ~[],
              originoffset: 0.0, keysplit: 0, keyorder: ~[],
              keykind: ~[None, ..72] }
    }

    /// Converts a single alphanumeric (base-36) letter to an integer.
    /// (C: `getdigit`)
    pure fn getdigit(n: char) -> Option<int> {
        match n {
            '0'..'9' => Some((n as int) - ('0' as int)),
            'a'..'z' => Some((n as int) - ('a' as int) + 10),
            'A'..'Z' => Some((n as int) - ('A' as int) + 10),
            _ => None
        }
    }

    /// Converts the first two letters of `s` to a key. (C: `key2index`)
    pub pure fn key2index(s: &[char]) -> Option<int> {
        assert s.len() >= 2;
        do getdigit(s[0]).chain_ref |&a| {
            do getdigit(s[1]).map |&b| { a * 36 + b }
        }
    }

    /// Converts the first two letters of `s` to a key. (C: `key2index`)
    pub pure fn key2index_str(s: &str) -> Option<int> {
        assert s.len() >= 2;
        let str::CharRange {ch:c1, next:p1} = str::char_range_at(s, 0);
        do getdigit(c1).chain_ref |&a| {
            let str::CharRange {ch:c2, next:p2} = str::char_range_at(s, p1);
            do getdigit(c2).map |&b| {
                assert p2 == 2; // both characters should be in ASCII
                a * 36 + b
            }
        }
    }

    // A lexer barely powerful enough to parse BMS format. Comparable to
    // C's `sscanf`.
    //
    // `lex!(e; fmt1, fmt2, ..., fmtN)` returns an expression that evaluates
    // to true if and only if all format specification is consumed. The format
    // specification (analogous to `sscanf`'s `%`-string) is as follows:
    //
    // - `ws`: Consumes one or more whitespace. (C: `%*[ \t\r\n]` or similar)
    // - `ws*`: Consumes zero or more whitespace. (C: ` `)
    // - `int [-> e2]`: Consumes an integer and optionally saves it to `e2`.
    //   (C: `%d` and `%*d`, but does not consume preceding whitespace)
    //   The integer syntax is slightly limited compared to `sscanf`.
    // - `float [-> e2]`: Consumes a real number and optionally saves it to
    //   `e2`. (C: `%f` etc.) Again, the real number syntax is slightly
    //   limited; especially an exponent support is missing.
    // - `str [-> e2]`: Consumes a remaining input as a string and optionally
    //   saves it to `e2`. The string is at least one character long.
    //   (C: not really maps to `sscanf`, similar to `fgets`) Implies `!`.
    //   It can be followed by `ws*` which makes the string right-trimmed.
    // - `str* [-> e2]`: Same as above but the string can be empty.
    // - `Key [-> e2]`: Consumes a two-letter alphanumeric key and optionally
    //   saves it to `e2`. (C: `%2[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ]` etc.
    //   followed by a call to `key2index`)
    // - `!`: Ensures that the entire string has been consumed. Should be the
    //   last format specification.
    // - `"foo"` etc.: An ordinary expression is treated as a literal string
    //   or literal character.
    //
    // Rust: - there is no `libc::sscanf` due to the varargs. maybe regex
    //         support will make this useless in the future, but not now.
    //       - multiple statements do not expand correctly. (#4375)
    //       - it is desirable to have a matcher only accepts an integer
    //         literal or string literal, not a generic expression.
    //       - no hygienic macro yet. possibly observable names from `$e`
    //         should be escaped for now.
    //       - it would be more useful to generate bindings for parsed result.
    //         this is related to many issues in general.
    macro_rules! lex(
        ($e:expr; ) => (true);
        ($e:expr; !) => ($e.is_empty());

        ($e:expr; int -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            // Rust: num::from_str_bytes_common does not recognize a number
            // followed by garbage, so we need to parse it ourselves.
            do _line.scan_int().map_default(false) |&_endpos| {
                let _prefix = _line.slice(0, _endpos);
                do int::from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_to_end(_endpos); $($tail)*)
                }
            }
        });
        ($e:expr; uint -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do _line.scan_uint().map_default(false) |&_endpos| {
                let _prefix = _line.slice(0, _endpos);
                do uint::from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_to_end(_endpos); $($tail)*)
                }
            }
        });
        ($e:expr; float -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do _line.scan_float().map_default(false) |&_endpos| {
                let _prefix = _line.slice(0, _endpos);
                do float::from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_to_end(_endpos); $($tail)*)
                }
            }
        });
        ($e:expr; str -> $dst:expr, ws*, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() {
                $dst = _line.trim_right();
                lex!(""; $($tail)*) // optimization!
            } else {
                false
            }
        });
        ($e:expr; str -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() {
                $dst = _line.to_owned();
                lex!(""; $($tail)*) // optimization!
            } else {
                false
            }
        });
        ($e:expr; str* -> $dst:expr, ws*, $($tail:tt)*) => ({
            let _line: &str = $e;
            $dst = _line.trim_right();
            lex!(""; $($tail)*) // optimization!
        });
        ($e:expr; str* -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            $dst = _line.to_owned();
            lex!(""; $($tail)*) // optimization!
        });
        // start Angolmois-specific
        ($e:expr; Key -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do key2index_str(_line).map_default(false) |&_value| {
                $dst = Key(_value);
                lex!(_line.slice_to_end(2u); $($tail)*)
            }
        });
        ($e:expr; Measure -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            let _isdigit = |c| { '0' <= c && c <= '9' };
            // Rust: this is plain annoying.
            if _line.len() >= 3 && _isdigit(_line.char_at(0)) &&
               _isdigit(_line.char_at(1)) && _isdigit(_line.char_at(2)) {
                $dst = int::from_str(_line.slice(0u, 3u)).unwrap();
                lex!(_line.slice_to_end(3u); $($tail)*)
            } else {
                false
            }
        });
        // end Angolmois-specific

        ($e:expr; ws, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() && char::is_whitespace(_line.char_at(0)) {
                lex!(str::trim_left(_line); $($tail)*)
            } else {
                false
            }
        });
        ($e:expr; ws*, $($tail:tt)*) => ({
            let _line: &str = $e;
            lex!(str::trim_left(_line); $($tail)*)
        });
        ($e:expr; int, $($tail:tt)*) => ({
            let mut _dummy: int = 0;
            lex!($e; int -> _dummy, $($tail)*)
        });
        ($e:expr; uint, $($tail:tt)*) => ({
            let mut _dummy: uint = 0;
            lex!($e; uint -> _dummy, $($tail)*)
        });
        ($e:expr; float, $($tail:tt)*) => ({
            let mut _dummy: float = 0.0;
            lex!($e; float -> _dummy, $($tail)*)
        });
        ($e:expr; str, $($tail:tt)*) => ({
            !$e.is_empty() && lex!(""; $($tail)*) // optimization!
        });
        ($e:expr; str*, $($tail:tt)*) => ({
            lex!(""; $($tail)*) // optimization!
        });
        // start Angolmois-specific
        ($e:expr; Key, $($tail:tt)*) => ({
            let mut _dummy: Key = Key(0);
            lex!($e; Key -> _dummy, $($tail)*)
        });
        ($e:expr; Measure, $($tail:tt)*) => ({
            let mut _dummy: int = 0;
            lex!($e; Measure -> _dummy, $($tail)*)
        });
        // end Angolmois-specific
        ($e:expr; $lit:expr, $($tail:tt)*) => ({
            do $lit.prefix_shifted($e).map_default(false) |&_line| {
                lex!(_line; $($tail)*)
            }
        });

        ($e:expr; int -> $dst:expr) => (lex!($e; int -> $dst, ));
        ($e:expr; uint -> $dst:expr) => (lex!($e; uint -> $dst, ));
        ($e:expr; float -> $dst:expr) => (lex!($e; float -> $dst, ));
        ($e:expr; str -> $dst:expr) => (lex!($e; str -> $dst, ));
        ($e:expr; str -> $dst:expr, ws*) => (lex!($e; str -> $dst, ws*, ));
        ($e:expr; str* -> $dst:expr) => (lex!($e; str* -> $dst, ));
        ($e:expr; str* -> $dst:expr, ws*) => (lex!($e; str* -> $dst, ws*, ));
        // start Angolmois-specific
        ($e:expr; Key -> $dst:expr) => (lex!($e; Key -> $dst, ));
        ($e:expr; Measure -> $dst:expr) => (lex!($e; Measure -> $dst, ));
        // end Angolmois-specific

        ($e:expr; ws) => (lex!($e; ws, ));
        ($e:expr; ws*) => (lex!($e; ws*, ));
        ($e:expr; int) => (lex!($e; int, ));
        ($e:expr; uint) => (lex!($e; uint, ));
        ($e:expr; float) => (lex!($e; float, ));
        ($e:expr; str) => (lex!($e; str, ));
        ($e:expr; str*) => (lex!($e; str*, ));
        // start Angolmois-specific
        ($e:expr; Key) => (lex!($e; Key, ));
        ($e:expr; Measure) => (lex!($e; Measure, ));
        // end Angolmois-specific
        ($e:expr; $lit:expr) => (lex!($e; $lit, ))
    )

    /// Reads and parses the BMS file with given RNG from given reader.
    pub fn parse_bms_from_reader(bmspath: &str, f: @io::Reader,
                                 r: @rand::Rng) -> Result<Bms,~str> {
        // (C: bmsheader)
        let bmsheader = [
            "TITLE", "GENRE", "ARTIST", "STAGEFILE", "PATH_WAV", "BPM",
            "PLAYER", "PLAYLEVEL", "RANK", "LNTYPE", "LNOBJ", "WAV", "BMP",
            "BGA", "STOP", "STP", "RANDOM", "SETRANDOM", "ENDRANDOM", "IF",
            "ELSEIF", "ELSE", "ENDSW", "END"];

        let mut bms = Bms();

        enum RndState { Process = 0, Ignore = 1, NoFurther = -1 }
        struct Rnd { val: int, inside: bool, state: RndState, skip: bool }
        let rnd = ~[Rnd { val: 0, inside: false,
                          state: Process, skip: false }];

        struct BmsLine { measure: int, chan: Key, data: ~[Key] }
        let mut bmsline = ~[]; // (C: `bmsline`)

        let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
        for lines.each |&line| {
            let mut line =
                ::util::str::from_fixed_utf8_bytes(line, |_| ~"\ufffd");
            line = line.trim_left();
            if !line.starts_with(~"#") { loop; }
            line = line.slice_to_end(1);

            let mut prefix = "";
            for bmsheader.each |&header| {
                if line.len() >= header.len() &&
                   line.slice(0, header.len()).to_upper()
                        == header.to_owned() {
                    prefix = header;
                    line = line.slice_to_end(header.len());
                    break;
                }
            }

            macro_rules! read(
                (string $string:ident) => ({
                    let mut text = ~"";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        bms.$string = Some(text);
                    }
                });
                (value $value:ident) => ({
                    lex!(line; ws, int -> bms.$value);
                });
                (path $paths:ident) => ({
                    let mut key = Key(-1), path = ~"";
                    if lex!(line; Key -> key, ws, str -> path, ws*, !) {
                        let Key(key) = key;
                        bms.$paths[key] = Some(path);
                    }
                })
            )

            let state = if rnd.last().skip { Ignore }
                        else { rnd.last().state };
            match (prefix, state) {
                // #TITLE|#GENRE|#ARTIST|#STAGEFILE|#PATH_WAV <string>
                ("TITLE", Process) => read!(string title),
                ("GENRE", Process) => read!(string genre),
                ("ARTIST", Process) => read!(string artist),
                ("STAGEFILE", Process) => read!(string stagefile),
                ("PATH_WAV", Process) => read!(string basepath),

                // #BPM <float> or #BPMxx <float>
                ("BPM", Process) => {
                    let mut key = Key(-1), bpm = DefaultBPM;
                    if lex!(line; Key -> key, ws, float -> bpm) {
                        let Key(key) = key; bms.bpmtab[key] = bpm;
                    } else if lex!(line; ws, float -> bpm) {
                        bms.initbpm = bpm;
                    }
                }

                // #PLAYER|#PLAYLEVEL|#RANK <int>
                ("PLAYER", Process) => read!(value player),
                ("PLAYLEVEL", Process) => read!(value playlevel),
                ("RANK", Process) => read!(value rank),

                // #LNTYPE <int>
                ("LNTYPE", Process) => {
                    let mut lntype = 1;
                    if lex!(line; ws, int -> lntype) {
                        bms.consecutiveln = (lntype == 2);
                    }
                }
                // #LNOBJ <key>
                ("LNOBJ", Process) => {
                    let mut key = Key(-1);
                    if lex!(line; ws, Key -> key) { bms.lnobj = Some(key); }
                }

                // #WAVxx|#BMPxx <path>
                ("WAV", Process) => read!(path sndpath),
                ("BMP", Process) => read!(path imgpath),

                // #BGAxx yy <int> <int> <int> <int> <int> <int>
                ("BGA", Process) => {
                    let mut dst = Key(0), src = Key(0);
                    let mut bc = BlitCmd {
                        dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                        x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
                    if lex!(line; Key -> dst, ws, Key -> src, ws,
                            int -> bc.x1, ws, int -> bc.y1, ws,
                            int -> bc.x2, ws, int -> bc.y2, ws,
                            int -> bc.dx, ws, int -> bc.dy) {
                        bc.src = ImageRef(src);
                        bc.dst = ImageRef(dst);
                        bms.blitcmd.push(bc);
                    }
                }

                // #STOPxx <int>
                ("STOP", Process) => {
                    let mut key = Key(-1), duration = 0;
                    if lex!(line; Key -> key, ws, int -> duration) {
                        let Key(key) = key;
                        bms.stoptab[key] = duration as float / 192.0;
                    }
                }

                // #STP<int>.<int> <int>
                ("STP", Process) => {
                    let mut measure = 0, frac = 0, duration = 0;
                    if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                            int -> duration) {
                        // TODO
                        io::println("STP");
                    }
                }

                // #RANDOM <int>
                ("RANDOM", _) |
                // #SETRANDOM <int>
                ("SETRANDOM", _) => {
                    io::println("RANDOM|SETRANDOM");
                }

                // #ENDRANDOM
                ("ENDRANDOM", _) => {
                    io::println("ENDRANDOM");
                }

                // #IF <int>
                ("IF", _) |
                // #ELSEIF <int>
                ("ELSEIF", _) => {
                    io::println("IF|ELSEIF");
                }

                // #ELSE <int>
                ("ELSE", _) => {
                    io::println("ELSE");
                }

                // #END(IF)
                ("END", _) => {
                    io::println("END");
                }

                // #nnnmm:...
                ("", Process) => {
                    let mut measure = 0, chan = Key(0), data = ~"";
                    if lex!(line; Measure -> measure, Key -> chan, ':',
                            ws*, str -> data, ws*, !) {
                        let data = str::chars(data);
                        let max = data.len() / 2 * 2;
                        let mut values = ~[];
                        for uint::range_step(0, max, 2) |i| {
                            let v = key2index(data.view(i, i+2));
                            values.push(Key(v.get_or_default(-1)));
                        }
                        bmsline.push(BmsLine { measure: measure, chan: chan,
                                               data: values })
                    }
                }

                (_, _) => ()
            }
        }

        //::core::repr::write_repr(io::stdout(), &bmsline);
        Ok(bms)
    }

    /// Reads and parses the BMS file with given RNG. (C: `parse_bms`)
    pub fn parse_bms(bmspath: &str, r: @rand::Rng) -> Result<Bms,~str> {
        do io::file_reader(&Path(bmspath)).chain |f| {
            parse_bms_from_reader(bmspath, f, r)
        }
    }
}

//============================================================================
// game play

pub mod player {

    pub enum Mode { PlayMode, AutoPlayMode, ExclusiveMode }
    pub enum Modf { NoModf, MirrorModf, ShuffleModf, ShuffleExModf,
                    RandomModf, RandomExModf }
    pub enum Bga { BgaAndMovie, BgaButNoMovie, NoBga }

    pub struct Options {
        bmspath: Option<~str>,
        mode: Mode,
        modf: Modf,
        bga: Bga,
        showinfo: bool,
        fullscreen: bool,
        joystick: Option<int>,
        preset: Option<~str>,
        leftkeys: Option<~str>,
        rightkeys: Option<~str>,
        playspeed: float,
    }

    pub pure fn Options() -> ~Options {
        ~Options { bmspath: None, mode: PlayMode, modf: NoModf,
                   bga: BgaAndMovie, showinfo: true, fullscreen: true,
                   joystick: None, preset: None, leftkeys: None,
                   rightkeys: None, playspeed: 1.0 }
    }

    use core::repr;
    pub fn play(opts: &Options) {
        repr::write_repr(io::stdout(), opts);
        io::println("");

        let r = rand::task_rng();
        let bms = match opts.bmspath {
            Some(copy path) =>
                match ::parser::parse_bms(path, r) {
                    Ok(bms) => bms,
                    Err(err) => die!("Couldn't load BMS file: %s", err)
                },
            None => fail!(~"TODO")
        };
        //::core::repr::write_repr(io::stdout(), &bms);

        let bmspath = Path(copy *opts.bmspath.get_ref()).dir_path();
        do ::sdl::start {
            ::sdl::init([::sdl::InitAudio]);
            ::sdl::mixer::open(44100,
                               ::sdl::audio::S16AudioFormat,
                               ::sdl::audio::Stereo,
                               2048);
            for bms.sndpath.each |&path| {
                do path.iter |&path| {
                    let path = bmspath.push_rel(&Path(str::replace(path,~".wav",~".ogg")));
                    match ::sdl::mixer::Chunk::from_wav(&path) {
                        Ok(chunk) => {
                            chunk.play(None, 0);
                            while ::sdl::mixer::playing(None) { }
                        },
                        Err(err) => io::println(err)
                    }
                }
            }
            ::sdl::mixer::close();
        }
    }

}

//============================================================================
// entry point

/// Prints the usage. (C: `usage`)
fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
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
    exit(1)
}

/// The entry point. Parses the command line options and delegates other
/// things to `play`. (C: `main`)
fn main() {
    // Rust: this is quite delicate... moving this to outside won't work.
    use player::*;

    let longargs = std::oldmap::hash_from_vec::<~str,char>([
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'),
        (~"--fullscreen", ' '), (~"--info", ' '), (~"--no-info", 'q'),
        (~"--mirror", 'm'), (~"--shuffle", 's'), (~"--shuffle-ex", 'S'),
        (~"--random", 'r'), (~"--random-ex", 'R'), (~"--preset", 'k'),
        (~"--key-spec", 'K'), (~"--bga", ' '), (~"--no-bga", 'B'),
        (~"--movie", ' '), (~"--no-movie", 'M'), (~"--joystick", 'j'),
    ]);

    let args = os::args();
    let nargs = args.len();
    let mut opts = Options();

    let mut i = 1;
    while i < nargs {
        if !args[i].starts_with("-") {
            if opts.bmspath.is_none() { opts.bmspath = Some(copy args[i]); }
        } else if args[i] == ~"--" {
            i += 1;
            if opts.bmspath.is_none() && i < nargs {
                opts.bmspath = Some(copy args[i]);
            }
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
                        Some(n) if n >= 0 => { opts.joystick = Some(n); },
                        _ => die!("Invalid argument to option -j")
                    },
                    ' ' => {}, // for ignored long options
                    '1'..'9' => {
                        opts.playspeed = char::to_digit(c, 10).get() as float;
                    },
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