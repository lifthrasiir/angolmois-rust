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
// * XXX - should be fixed as soon as Rust issue is gone.

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
#[macro_escape]
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
                    fail_unless!(i != chend);
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
                Some(first) if first > 0u => Some(first),
                None if s.len() > 0u => Some(s.len()),
                _ => None
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `int::from_str` accepts without a failure, if any.
        pub pure fn scan_int(s: &str) -> Option<uint> {
            if s.starts_with(~"-") || s.starts_with(~"+") {
                scan_uint(s.slice_to_end(1u)).map(|&pos| pos + 1u)
            } else {
                scan_uint(s)
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `float::from_str` accepts without a failure, if any.
        pub pure fn scan_float(s: &str) -> Option<uint> {
            do scan_int(s).chain |pos| {
                if s.len() > pos && s.char_at(pos) == '.' {
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

        impl StrUtil for &'self str {
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

        impl ShiftablePrefix for &'self str {
            pure fn prefix_shifted(&self, s: &str) -> Option<~str> {
                if s.starts_with(*self) {
                    Some(s.slice_to_end(self.len()))
                } else {
                    None
                }
            }
        }

    }

    /// Option utilities for Rust. Parallels to `core::option`.
    ///
    /// NOTE: Some of these additions will be eventually sent to
    /// libcore/option.rs and are not subject to the above copyright notice.
    pub mod option {

        #[inline(always)]
        pub pure fn filter<T:Copy>(opt: Option<T>, f: fn(t: T) -> bool)
                                        -> Option<T> {
            match opt {
                Some(t) => if f(t) { Some(t) } else { None },
                None => None
            }
        }

        pub trait OptionUtil<T> {
            pure fn chain<U>(self, f: fn(x: T) -> Option<U>) -> Option<U>;
        }

        pub trait CopyableOptionUtil<T:Copy> {
            pure fn filter(self, f: fn(x: T) -> bool) -> Option<T>;
        }

        impl<T> OptionUtil<T> for Option<T> {
            #[inline(always)]
            pure fn chain<U>(self, f: fn(x: T) -> Option<U>) -> Option<U> {
                option::chain(self, f)
            }
        }

        impl<T:Copy> CopyableOptionUtil<T> for Option<T> {
            #[inline(always)]
            pure fn filter(self, f: fn(x: T) -> bool) -> Option<T> {
                filter(self, f)
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
                $dst = uint::from_str(_line.slice(0u, 3u)).unwrap();
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
            let mut _dummy: uint = 0;
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

}

use core::io::{ReaderUtil, WriterUtil};

use util::str::*;
use util::option::*;
use util::io::*;

//============================================================================
// bms parser

/// BMS parser module.
pub mod parser {

    /// Two-letter alphanumeric identifier (henceforth "alphanumeric key")
    /// used for virtually everything, including resource management, variable
    /// BPM and chart specification.
    #[deriving_eq]
    pub struct Key(int);

    /// The number of all possible alphanumeric keys. (C: `MAXKEY`)
    pub const MAXKEY: int = 36*36;

    pub impl Key {
        /// Returns if the alphanumeric key is in the proper range. Angolmois
        /// supports the full range of 00-ZZ (0-1295) for every case.
        pure fn is_valid(self) -> bool {
            0 <= *self && *self < MAXKEY
        }

        /// Re-reads the alphanumeric key as a hexadecimal number if possible.
        /// This is required due to handling of channel #03 (BPM is expected
        /// to be in hexadecimal).
        pure fn to_hex(self) -> Option<int> {
            let sixteens = *self / 36, ones = *self % 36;
            if sixteens < 16 && ones < 16 { Some(sixteens * 16 + ones) }
            else { None }
        }
    }

    impl Ord for Key {
        // Rust: it is very easy to make an infinite recursion here.
        pure fn lt(&self, other: &Key) -> bool { **self < **other }
        pure fn le(&self, other: &Key) -> bool { **self <= **other }
        pure fn ge(&self, other: &Key) -> bool { **self >= **other }
        pure fn gt(&self, other: &Key) -> bool { **self > **other }
    }

    impl ToStr for Key {
        /// Returns a two-letter representation of alphanumeric key.
        /// (C: `TO_KEY`)
        pure fn to_str(&self) -> ~str {
            fail_unless!(self.is_valid());
            let map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            fmt!("%c%c", map[*self / 36] as char, map[*self % 36] as char)
        }
    }

    /// A game play element mapped to the single input element (for example,
    /// button) and the screen area (henceforth "lane").
    #[deriving_eq]
    pub struct Lane(uint);

    /// The maximum number of lanes. (C: `NNOTECHANS`)
    pub const NLANES: uint = 72;

    pub impl Lane {
        /// Converts the channel number to the lane number.
        static pure fn from_channel(chan: Key) -> Lane {
            let player = match *chan / 36 {
                1 | 3 | 5 | 0xD => 0,
                2 | 4 | 6 | 0xE => 1,
                _ => fail!(~"non-object channel")
            };
            Lane(player * 36 + *chan as uint % 36)
        }
    }

    /// Sound reference.
    #[deriving_eq]
    pub struct SoundRef(Key);

    /// Image reference.
    #[deriving_eq]
    pub struct ImageRef(Key);

    /// BGA layers. (C: `enum BGA_type`)
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

    #[deriving_eq]
    pub struct BPM(float);

    /// (C: `MEASURE_TO_MSEC`)
    pub pure fn measure_to_msec(measure: float, bpm: float) -> float {
        measure * 240000.0 / bpm
    }

    /// (C: `MSEC_TO_MEASURE`)
    pub pure fn msec_to_measure(msec: float, bpm: float) -> float {
        msec * bpm / 240000.0
    }

    #[deriving_eq]
    pub enum Duration { Seconds(float), Measures(float) }

    #[deriving_eq]
    pub enum Damage { GaugeDamage(float), InstantDeath }

    #[deriving_eq]
    pub enum ObjData {
        /// Deleted object. Only used during various processing.
        Deleted,
        /// Visible object. Sound is played when the key is input inside the
        /// associated grading area. (C: `NOTE`)
        Visible(Lane, Option<SoundRef>),
        /// Invisible object. Sound is played when the key is input inside the
        /// associated grading area. No render nor grading performed.
        /// (C: `INVNOTE`)
        Invisible(Lane, Option<SoundRef>),
        /// Start of long note (LN). Sound is played when the key is down
        // inside the associated grading area. (C: `LNSTART`)
        LNStart(Lane, Option<SoundRef>),
        /// End of LN. Sound is played when the start of LN is graded, the key
        /// was down and now up inside the associated grading area.
        /// (C: `LNDONE`)
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

    pub trait ObjOps {
        pub pure fn is_visible(self) -> bool;
        pub pure fn is_invisible(self) -> bool;
        pub pure fn is_lnstart(self) -> bool;
        pub pure fn is_lndone(self) -> bool;
        pub pure fn is_ln(self) -> bool;
        pub pure fn is_bomb(self) -> bool;
        pub pure fn is_object(self) -> bool;
        pub pure fn is_bgm(self) -> bool;
        pub pure fn is_setbga(self) -> bool;
        pub pure fn is_setbpm(self) -> bool;
        pub pure fn is_stop(self) -> bool;

        pub pure fn to_visible(self) -> Self;
        pub pure fn to_invisible(self) -> Self;
        pub pure fn to_lnstart(self) -> Self;
        pub pure fn to_lndone(self) -> Self;

        pub pure fn object_lane(self) -> Option<Lane>;
        pub pure fn sounds(self) -> ~[SoundRef];
        pub pure fn keydown_sound(self) -> Option<SoundRef>;
        pub pure fn keyup_sound(self) -> Option<SoundRef>;
        pub pure fn through_sound(self) -> Option<SoundRef>;
        pub pure fn images(self) -> ~[ImageRef];
        pub pure fn through_damage(self) -> Option<Damage>;
    }

    impl ObjOps for ObjData {
        pub pure fn is_visible(self) -> bool {
            match self { Visible(*) => true, _ => false }
        }

        pub pure fn is_invisible(self) -> bool {
            match self { Invisible(*) => true, _ => false }
        }

        pub pure fn is_lnstart(self) -> bool {
            match self { LNStart(*) => true, _ => false }
        }

        pub pure fn is_lndone(self) -> bool {
            match self { LNDone(*) => true, _ => false }
        }

        pub pure fn is_ln(self) -> bool {
            match self { LNStart(*)|LNDone(*) => true, _ => false }
        }

        pub pure fn is_bomb(self) -> bool {
            match self { Bomb(*) => true, _ => false }
        }

        pub pure fn is_object(self) -> bool {
            match self {
                Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*)
                    => true,
                _ => false
            }
        }

        pub pure fn is_bgm(self) -> bool {
            match self { BGM(*) => true, _ => false }
        }

        pub pure fn is_setbga(self) -> bool {
            match self { SetBGA(*) => true, _ => false }
        }

        pub pure fn is_setbpm(self) -> bool {
            match self { SetBPM(*) => true, _ => false }
        }

        pub pure fn is_stop(self) -> bool {
            match self { Stop(*) => true, _ => false }
        }

        pub pure fn to_visible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Visible(lane,snd),
                _ => fail!(~"to_visible for non-object")
            }
        }

        pub pure fn to_invisible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Invisible(lane,snd),
                _ => fail!(~"to_invisible for non-object")
            }
        }

        pub pure fn to_lnstart(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNStart(lane,snd),
                _ => fail!(~"to_lnstart for non-object")
            }
        }

        pub pure fn to_lndone(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNDone(lane,snd),
                _ => fail!(~"to_lndone for non-object")
            }
        }

        pub pure fn object_lane(self) -> Option<Lane> {
            match self {
                Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
                LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
                _ => None
            }
        }

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

    /// Game play data associated to the time axis. Contrary to its name (I
    /// don't like naming this to `Data`) it is not limited to the objects
    /// (which are also associated to lanes) but also 
    #[deriving_eq]
    pub struct Obj {
        /// Time position in measures.
        time: float,
        /// Actual data.
        data: ObjData
    }

    pub impl Obj {
        /// Creates a `Visible` object.
        static pure fn Visible(time: float, lane: Lane,
                               sref: Option<Key>) -> Obj {
            // Rust: `SoundRef` itself cannot be used as a function (#5315)
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Visible(lane, sref) }
        }

        /// Creates an `Invisible` object.
        static pure fn Invisible(time: float, lane: Lane,
                                 sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Invisible(lane, sref) }
        }

        /// Creates a `LNStart` object.
        static pure fn LNStart(time: float, lane: Lane,
                               sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: LNStart(lane, sref) }
        }

        /// Creates a `LNDone` object.
        static pure fn LNDone(time: float, lane: Lane,
                              sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: LNDone(lane, sref) }
        }

        /// Creates a `Bomb` object.
        static pure fn Bomb(time: float, lane: Lane, sref: Option<Key>,
                            damage: Damage) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Bomb(lane, sref, damage) }
        }

        /// Creates a `BGM` object.
        static pure fn BGM(time: float, sref: Key) -> Obj {
            Obj { time: time, data: BGM(SoundRef(sref)) }
        }

        /// Creates a `SetBGA` object.
        static pure fn SetBGA(time: float, layer: BGALayer,
                              iref: Option<Key>) -> Obj {
            let iref = iref.map_consume(|i| ImageRef(i)); // XXX #5315
            Obj { time: time, data: SetBGA(layer, iref) }
        }

        /// Creates a `SetBPM` object.
        static pure fn SetBPM(time: float, bpm: BPM) -> Obj {
            Obj { time: time, data: SetBPM(bpm) }
        }

        /// Creates a `Stop` object.
        static pure fn Stop(time: float, duration: Duration) -> Obj {
            Obj { time: time, data: Stop(duration) }
        }
    }

    impl Ord for Obj {
        pure fn lt(&self, other: &Obj) -> bool { self.time < other.time }
        pure fn le(&self, other: &Obj) -> bool { self.time <= other.time }
        pure fn ge(&self, other: &Obj) -> bool { self.time >= other.time }
        pure fn gt(&self, other: &Obj) -> bool { self.time > other.time }
    }

    impl ObjOps for Obj {
        pub pure fn is_visible(self) -> bool { self.data.is_visible() }
        pub pure fn is_invisible(self) -> bool { self.data.is_invisible() }
        pub pure fn is_lnstart(self) -> bool { self.data.is_lnstart() }
        pub pure fn is_lndone(self) -> bool { self.data.is_lndone() }
        pub pure fn is_ln(self) -> bool { self.data.is_ln() }
        pub pure fn is_bomb(self) -> bool { self.data.is_bomb() }
        pub pure fn is_object(self) -> bool { self.data.is_object() }
        pub pure fn is_bgm(self) -> bool { self.data.is_bgm() }
        pub pure fn is_setbga(self) -> bool { self.data.is_setbga() }
        pub pure fn is_setbpm(self) -> bool { self.data.is_setbpm() }
        pub pure fn is_stop(self) -> bool { self.data.is_stop() }

        pub pure fn to_visible(self) -> Obj {
            Obj { time: self.time, data: self.data.to_visible() }
        }
        pub pure fn to_invisible(self) -> Obj {
            Obj { time: self.time, data: self.data.to_invisible() }
        }
        pub pure fn to_lnstart(self) -> Obj {
            Obj { time: self.time, data: self.data.to_lnstart() }
        }
        pub pure fn to_lndone(self) -> Obj {
            Obj { time: self.time, data: self.data.to_lndone() }
        }

        pub pure fn object_lane(self) -> Option<Lane> {
            self.data.object_lane()
        }
        pub pure fn sounds(self) -> ~[SoundRef] {
            self.data.sounds()
        }
        pub pure fn keydown_sound(self) -> Option<SoundRef> {
            self.data.keydown_sound()
        }
        pub pure fn keyup_sound(self) -> Option<SoundRef> {
            self.data.keyup_sound()
        }
        pub pure fn through_sound(self) -> Option<SoundRef> {
            self.data.through_sound()
        }
        pub pure fn images(self) -> ~[ImageRef] {
            self.data.images()
        }
        pub pure fn through_damage(self) -> Option<Damage> {
            self.data.through_damage()
        }
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

        /// Initial BPM. (C: `initbpm`)
        initbpm: float,
        /// Paths to sound file relative to `basepath` or BMS file.
        /// (C: `sndpath`)
        ///
        /// Rust: constant expression in the array size is unsupported.
        sndpath: ~[Option<~str> * 1296], // XXX 1296=MAXKEY
        /// Paths to image/movie file relative to `basepath` or BMS file.
        /// (C: `imgpath`)
        imgpath: ~[Option<~str> * 1296], // XXX 1296=MAXKEY
        /// List of blit commands to be executed after `imgpath` is loaded.
        /// (C: `blitcmd`)
        blitcmd: ~[BlitCmd],

        objs: ~[Obj],
        shorten: ~[float]
    }

    /// Creates a default value of BMS data.
    pub pure fn Bms() -> Bms {
        Bms { title: None, genre: None, artist: None, stagefile: None,
              basepath: None, player: SinglePlay, playlevel: 0, rank: 2,
              initbpm: DefaultBPM, sndpath: ~[None, ..MAXKEY],
              imgpath: ~[None, ..MAXKEY], blitcmd: ~[], objs: ~[],
              shorten: ~[] }
    }

    pub impl Bms {
        pure fn shorten_factor(&self, measure: int) -> float {
            if measure < 0 || measure as uint >= self.shorten.len() {
                1.0
            } else {
                self.shorten[measure as uint]
            }
        }

        /// (C: `adjust_object_time`)
        pure fn adjust_object_time(&self, base: float, offset: float)
                                        -> float {
            use core::num::Round;
            let basemeasure = base.floor() as int;
            let baseshorten = self.shorten_factor(basemeasure);
            let basefrac = base - basemeasure as float;
            let tonextmeasure = (1.0 - basefrac) * baseshorten;
            if tonextmeasure > offset {
                base + offset / baseshorten
            } else {
                let mut offset = offset - tonextmeasure;
                let mut i = basemeasure + 1;
                let mut curmeasure = self.shorten_factor(i);
                while curmeasure <= offset {
                    offset -= curmeasure;
                    i += 1;
                    curmeasure = self.shorten_factor(i);
                }
                i as float + offset / baseshorten
            }
        }

        /// (C: `adjust_object_position`)
        pub fn adjust_object_position(&self, base: float, time: float)
                                        -> float {
            use core::num::Round;
            let basemeasure = base.floor() as int;
            let timemeasure = time.floor() as int;
            let basefrac = base - basemeasure as float;
            let timefrac = time - timemeasure as float;
            let mut pos = timefrac * self.shorten_factor(timemeasure) -
                          basefrac * self.shorten_factor(basemeasure);
            let mut i = basemeasure;
            while i < timemeasure {
                pos += self.shorten_factor(i);
                i += 1;
            }
            pos
        }

        /// (C: `get_bms_duration`)
        pub fn duration(&self, originoffset: float, length: float,
                        sound_length: fn(SoundRef) -> float) -> float {
            let mut pos = originoffset;
            let mut bpm = self.initbpm;
            let mut time = 0.0, sndtime = 0.0;

            for self.objs.each |&obj| {
                let delta = self.adjust_object_position(pos, obj.time);
                time += measure_to_msec(delta, bpm);
                match obj.data {
                    Visible(_,Some(sref)) | LNStart(_,Some(sref)) | BGM(sref) =>
                        sndtime = cmp::max(sndtime, time + sound_length(sref)),
                    SetBPM(BPM(newbpm)) =>
                        if newbpm > 0.0 {
                            bpm = newbpm;
                        } else if newbpm < 0.0 {
                            bpm = newbpm;
                            let delta =
                                self.adjust_object_position(originoffset, pos);
                            time += measure_to_msec(delta, -newbpm);
                            break;
                        },
                    Stop(Seconds(secs)) =>
                        time += secs * 1000.0,
                    Stop(Measures(measures)) =>
                        time += measure_to_msec(measures, bpm),
                    _ => (),
                }
                pos = obj.time;
            }

            if bpm > 0.0 {
                let delta = self.adjust_object_position(pos, length);
                time += measure_to_msec(delta, bpm);
            }
            cmp::max(time, sndtime)
         }

    }

    /// Derived BMS information. Again, this is not a global state.
    pub struct BmsInfo {
        /// (C: `originoffset`)
        originoffset: float,
        /// (C: `hasbpmchange`)
        hasbpmchange: bool,
        /// (C: `haslongnote`)
        haslongnote: bool,
        /// (C: `nnotes`)
        nnotes: int,
        /// (C: `maxscore`)
        maxscore: int
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

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    pub pure fn key2index(s: &[char]) -> Option<int> {
        fail_unless!(s.len() >= 2);
        do getdigit(s[0]).chain |a| {
            do getdigit(s[1]).map |&b| { a * 36 + b }
        }
    }

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    pub pure fn key2index_str(s: &str) -> Option<int> {
        fail_unless!(s.len() >= 2);
        let str::CharRange {ch:c1, next:p1} = str::char_range_at(s, 0);
        do getdigit(c1).chain |a| {
            let str::CharRange {ch:c2, next:p2} = str::char_range_at(s, p1);
            do getdigit(c2).map |&b| {
                fail_unless!(p2 == 2); // both characters should be in ASCII
                a * 36 + b
            }
        }
    }

    /// Reads and parses the BMS file with given RNG from given reader.
    pub fn parse_bms_from_reader(bmspath: &str, f: @io::Reader,
                                 r: @rand::Rng) -> Result<Bms,~str> {
        // (C: `bmsheader`)
        let bmsheader = [
            "TITLE", "GENRE", "ARTIST", "STAGEFILE", "PATH_WAV", "BPM",
            "PLAYER", "PLAYLEVEL", "RANK", "LNTYPE", "LNOBJ", "WAV", "BMP",
            "BGA", "STOP", "STP", "RANDOM", "SETRANDOM", "ENDRANDOM", "IF",
            "ELSEIF", "ELSE", "ENDSW", "END"];

        let mut bms = Bms();

        enum RndState { Process = 0, Ignore = 1, NoFurther = -1 }
        struct Rnd {
            val: Option<int>,
            inside: bool,
            /// (C: `ignore` field)
            state: RndState,
            skip: bool
        }

        // Rust: #[deriving_eq] does not work inside the function. (#4913)
        impl Eq for RndState {
            pure fn eq(&self, other: &RndState) -> bool {
                match (*self, *other) {
                    (Process, Process) => true,
                    (Ignore, Ignore) => true,
                    (NoFurther, NoFurther) => true,
                    (_, _) => false
                }
            }
            pure fn ne(&self, other: &RndState) -> bool { !self.eq(other) }
        }
        impl Eq for Rnd {
            pure fn eq(&self, other: &Rnd) -> bool {
                // Rust: this is for using `ImmutableEqVector<T>::rposition`,
                //       which should have been in `ImmutableVector<T>`.
                self.val == other.val && self.inside == other.inside &&
                self.state == other.state && self.skip == other.skip
            }
            pure fn ne(&self, other: &Rnd) -> bool { !self.eq(other) }
        }

        let mut rnd = ~[Rnd { val: None, inside: false,
                              state: Process, skip: false }];

        struct BmsLine { measure: uint, chan: Key, data: ~str }
        impl Ord for BmsLine {
            pure fn lt(&self, other: &BmsLine) -> bool {
                self.measure < other.measure ||
                (self.measure == other.measure && self.chan < other.chan)
            }
            pure fn le(&self, other: &BmsLine) -> bool {
                self.measure < other.measure ||
                (self.measure == other.measure && self.chan <= other.chan)
            }
            pure fn ge(&self, other: &BmsLine) -> bool { !self.lt(other) }
            pure fn gt(&self, other: &BmsLine) -> bool { !self.le(other) }
        }

        // (C: `bmsline`)
        let mut bmsline = ~[];
        // (C: `bpmtab`)
        let mut bpmtab = ~[BPM(DefaultBPM), ..MAXKEY];
        // (C: `stoptab`)
        let mut stoptab = ~[Seconds(0.0), ..MAXKEY];

        // Allows LNs to be specified as a consecutive row of same or non-00
        // alphanumeric keys (MGQ type, #LNTYPE 2). The default is to specify
        // LNs as two endpoints (RDM type, #LNTYPE 1). (C: `value[V_LNTYPE]`)
        let mut consecutiveln = false;

        // An end-of-LN marker used in LN specification for channels #1x/2x.
        // Maps to BMS #LNOBJ command. (C: `value[V_LNOBJ]`)
        let mut lnobj = None;

        let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
        for lines.each |&line| {
            let mut line =
                ::util::str::from_fixed_utf8_bytes(line, |_| ~"\ufffd");

            // skip non-command lines
            line = line.trim_left();
            if !line.starts_with(~"#") { loop; }
            line = line.slice_to_end(1);

            // search for header prefix. the header list (`bmsheader`) is
            // in the decreasing order of prefix length.
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

            // Common readers.
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

            // Rust: mutable loan and immutable loan cannot coexist
            //       in the same block (although it's safe). (#4666)
            fail_unless!(!rnd.is_empty());
            let state = { if rnd.last().skip { Ignore }
                          else { rnd.last().state } }; // XXX #4666

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
                        let Key(key) = key; bpmtab[key] = BPM(bpm);
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
                        consecutiveln = (lntype == 2);
                    }
                }
                // #LNOBJ <key>
                ("LNOBJ", Process) => {
                    let mut key = Key(-1);
                    if lex!(line; ws, Key -> key) { lnobj = Some(key); }
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
                        stoptab[key] = Measures(duration as float / 192.0);
                    }
                }

                // #STP<int>.<int> <int>
                ("STP", Process) => {
                    let mut measure = 0, frac = 0, duration = 0;
                    if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                            int -> duration) && duration > 0 {
                        let pos = measure as float + frac as float * 0.001;
                        let dur = Seconds(duration as float * 0.001);
                        bms.objs.push(Obj::Stop(pos, dur));
                    }
                }

                // #RANDOM|#SETRANDOM <int>
                ("RANDOM", _) |
                ("SETRANDOM", _) => {
                    let mut val = 0;
                    if lex!(line; ws, int -> val) {
                        let val = if val <= 0 { None } else { Some(val) };

                        // do not generate a random value if the entire block
                        // is skipped (but it still marks the start of block)
                        let inactive = // XXX #4666
                            {rnd.last().state != Process || rnd.last().skip};
                        let generated =
                            // Rust: there should be `Option<T>::chain` if
                            //       `T` is copyable.
                            do val.chain |val| {
                                if prefix == "SETRANDOM" {
                                    Some(val)
                                } else if !inactive {
                                    // Rust: not an uniform distribution yet!
                                    Some(r.gen_int_range(1, val + 1))
                                } else {
                                    None
                                }
                            };
                        rnd.push(Rnd { val: generated, inside: false,
                                       state: Process, skip: inactive });
                    }
                }

                // #ENDRANDOM
                ("ENDRANDOM", _) => {
                    if rnd.len() > 1 { rnd.pop(); }
                }

                // #IF|#ELSEIF <int>
                ("IF", _) |
                ("ELSEIF", _) => {
                    let mut val = 0;
                    if lex!(line; ws, int -> val) {
                        let val = if val <= 0 { None } else { Some(val) };

                        // Rust: `rnd.last_ref()` may be useful?
                        let last = &mut rnd[rnd.len() - 1];
                        last.inside = true;
                        last.state =
                            if prefix == "IF" || last.state == NoFurther {
                                if val.is_none() || val == last.val {
                                    Ignore
                                } else {
                                    Process
                                }
                            } else {
                                NoFurther
                            };
                    }
                }

                // #ELSE
                ("ELSE", _) => {
                    let last = &mut rnd[rnd.len() - 1];
                    last.inside = true;
                    last.state = if last.state == Ignore { Process }
                                 else { NoFurther };
                }

                // #END(IF)
                ("END", _) => {
                    for rnd.rposition(|&i| i.inside).each |idx| {
                        rnd.truncate(idx + 1);
                    }

                    { // XXX #4666
                        let last = &mut rnd[rnd.len() - 1];
                        last.inside = false;
                        last.state = Process;
                    }
                }

                // #nnnmm:...
                ("", Process) => {
                    let mut measure = 0, chan = Key(0), data = ~"";
                    if lex!(line; Measure -> measure, Key -> chan, ':',
                            ws*, str -> data, ws*, !) {
                        bmsline.push(BmsLine { measure: measure, chan: chan,
                                               data: data })
                    }
                }

                (_, _) => ()
            }
        }

        ::std::sort::tim_sort(bmsline);

        // Poor BGA defined by #BMP00 wouldn't be played if it is a movie.
        // We can't just let it played at the beginning of the chart as the
        // "beginning" is not always 0.0 (actually, `originoffset`). Thus
        // we add an artificial BGA object at time 0.0 only when the other
        // poor BGA does not exist at this position. (C: `poorbgafix`)
        let mut poorbgafix = true;

        // Indices to last visible object per channels. A marker specified by
        // #LNOBJ will turn this last object to the start of LN.
        // (C: `prev12`)
        let mut lastvis: [Option<uint>*72] = [None, ..NLANES];

        // Indices to last LN start or end inserted (and not finalized yet)
        // per channels. If `consecutiveln` is on (#LNTYPE 2), the position
        // of referenced object gets updated during parsing; if off (#LNTYPE
        // 1), it is solely used for checking if we are inside the LN or not.
        // (C: `prev56`)
        let mut lastln: [Option<uint>*72] = [None, ..NLANES];

        // Handles a non-00 alphanumeric key `v` positioned at the particular
        // channel `chan` and particular position `t`. The position `t2`
        // next to `t` is used for some cases that an alphanumeric key
        // designates an area rather than a point.
        let handle_key = |chan: Key, t: float, t2: float, v: Key| {
            // Adds an object. Objects are sorted by its position later.
            let add = |obj: Obj| { bms.objs.push(obj); };
            // Adds an object and returns its position. LN parsing generally
            // mutates the existing object for simplicity.
            let mark = |obj: Obj| -> Option<uint> {
                let marked = bms.objs.len();
                bms.objs.push(obj);
                Some(marked)
            };

            match *chan {
                // channel #01: BGM
                1 => add(Obj::BGM(t, v)),

                // channel #03: BPM as an hexadecimal key
                3 =>
                    for v.to_hex().each |&v| {
                        add(Obj::SetBPM(t, BPM(v as float)))
                    },

                // channel #04: BGA layer 1
                4 => add(Obj::SetBGA(t, Layer1, Some(v))),

                // channel #06: POOR BGA
                6 => {
                    add(Obj::SetBGA(t, PoorBGA, Some(v)));
                    poorbgafix = false; // we don't add artificial BGA
                },

                // channel #07: BGA layer 2
                7 => add(Obj::SetBGA(t, Layer2, Some(v))),

                // channel #08: BPM defined by #BPMxx
                8 => add(Obj::SetBPM(t, bpmtab[*v])),

                // channel #09: chart stopper defined by #STOPxx
                9 => add(Obj::Stop(t, stoptab[*v])),

                // channel #0A: BGA layer 3
                10 => add(Obj::SetBGA(t, Layer3, Some(v))),

                // channels #1x/2x: visible object, possibly LNs when #LNOBJ
                // is in active
                1*36..3*36-1 => {
                    let lane = Lane::from_channel(chan);
                    if lnobj.is_some() && lnobj == Some(v) {
                        // change the last inserted visible object to the
                        // start of LN if any.
                        for {lastvis[*lane]}.each |&pos| { // XXX #4666
                            fail_unless!(bms.objs[pos].is_visible());
                            bms.objs[pos] = bms.objs[pos].to_lnstart();
                            add(Obj::LNDone(t, lane, Some(v)));
                            lastvis[*lane] = None;
                        }
                    } else {
                        lastvis[*lane] = mark(Obj::Visible(t, lane, Some(v)));
                    }
                },

                // channels #3x/4x: invisible object
                3*36..5*36-1 => {
                    let lane = Lane::from_channel(chan);
                    add(Obj::Invisible(t, lane, Some(v)));
                },

                // channels #5x/6x, #LNTYPE 1: LN endpoints
                5*36..7*36-1 if !consecutiveln => {
                    let lane = Lane::from_channel(chan);

                    // a pair of non-00 alphanumeric keys designate one LN.
                    // if there are an odd number of them, the last LN is
                    // implicitly closed later.
                    if lastln[*lane].is_some() {
                        lastln[*lane] = None;
                        add(Obj::LNDone(t, lane, Some(v)));
                    } else {
                        lastln[*lane] = mark(Obj::LNStart(t, lane, Some(v)));
                    }
                },

                // channels #5x/6x, #LNTYPE 2: LN areas
                5*36..7*36-1 if consecutiveln => {
                    let lane = Lane::from_channel(chan);

                    // one non-00 alphanumeric key, in the absence of other
                    // information, inserts one complete LN starting at `t`
                    // and ending at `t2`.
                    //
                    // the next non-00 alphanumeric key also inserts one
       	    // complete LN from `t` to `t2`, unless there is already
		    // an end of LN at `t` in which case the end of LN is
		    // simply moved from `t` to `t2` (effectively increasing
		    // the length of previous LN).
                    match lastln[*lane] {
                        Some(pos) if bms.objs[pos].time == t => {
                            fail_unless!(bms.objs[pos].is_lndone());
                            bms.objs[pos].time = t2;
                        }
                        _ => {
                            add(Obj::LNStart(t, lane, Some(v)));
                            lastln[*lane] =
                                mark(Obj::LNDone(t2, lane, Some(v)));
                        }
                    }
                },

                // channels #Dx/Ex: bombs, base-36 damage value (unit of 0.5%
                // of the full gauge) or instant death (ZZ)
                0xD*36..0xE*36-1 => {
                    let lane = Lane::from_channel(chan);
                    let damage = match *v {
                        1..200 => Some(GaugeDamage(*v as float / 200.0)),
                        1295 => Some(InstantDeath), // XXX 1295=MAXKEY-1
                        _ => None
                    };
                    for damage.each |&damage| {
                        add(Obj::Bomb(t, lane, Some(Key(0)), damage));
                    }
                },

                // unsupported: channels #0B/0C/0D/0E (BGA opacity),
                // #97/98 (sound volume), #99 (text), #A0 (dynamic #RANK),
                // #A1/A2/A3/A4 (BGA color key update), #A5 (BGA on keypress),
                // #A6 (player-specific option)
                _ => ()
            }
        };

        for bmsline.each |line| {
            if *line.chan == 2 {
                let mut shorten = 0.0;
                if lex!(line.data; ws*, float -> shorten) {
                    if shorten > 0.001 {
                        bms.shorten.grow_set(line.measure, &1.0, shorten);
                    }
                }
            } else {
                let measure = line.measure as float;
                let data = str::chars(line.data);
                let max = data.len() / 2 * 2;
                let count = max as float;
                for uint::range_step(0, max, 2) |i| {
                    let v = key2index(data.view(i, i+2));
                    for v.each |&v| {
                        if v != 0 { // ignores 00
                            let t = measure + i as float / count;
                            let t2 = measure + (i + 2) as float / count;
                            handle_key(line.chan, t, t2, Key(v));
                        }
                    }
                }
            }
        }

        if poorbgafix {
            bms.objs.push(Obj::SetBGA(0.0, PoorBGA, Some(Key(0))));
        }

        // fix the unterminated longnote
        let maxmeasure = if bmsline.is_empty() { 0 }
                         else { bmsline.last().measure };
        let endt = (maxmeasure + 1) as float;
        for uint::range(0, NLANES) |i| {
            if lastvis[i].is_some() ||
                    (!consecutiveln && lastln[i].is_some()) {
                bms.objs.push(Obj::LNDone(endt, Lane(i), None));
            }
        }

        Ok(bms)
    }

    /// Reads and parses the BMS file with given RNG. (C: `parse_bms`)
    pub fn parse_bms(bmspath: &str, r: @rand::Rng) -> Result<Bms,~str> {
        do io::file_reader(&Path(bmspath)).chain |f| {
            parse_bms_from_reader(bmspath, f, r)
        }
    }

    /// Updates the object in place to BGM or placeholder.
    /// (C: `remove_or_replace_note`)
    fn remove_or_replace_note(obj: &mut Obj) {
        obj.data = match obj.data {
            Visible(_,Some(sref)) | Invisible(_,Some(sref)) |
            LNStart(_,Some(sref)) | LNDone(_,Some(sref)) => BGM(sref),
            _ => Deleted
        };
    }

    /// Fixes a problematic data. (C: `sanitize_bms`)
    pub fn sanitize_bms(bms: &mut Bms) {
        ::std::sort::tim_sort(bms.objs);

        fn sanitize(objs: &mut [Obj], to_type: fn(&Obj) -> Option<uint>,
                    merge_types: fn(uint) -> uint) {
            let len = objs.len();
            let mut i = 0;
            while i < len {
                let cur = objs[i].time;
                let mut types = 0;
                let mut j = 0;
                while j < len && objs[j].time <= cur {
                    let obj = &mut objs[j];
                    for to_type(obj).each |&t| {
                        if (types & (1 << t)) != 0 {
                            // duplicate type
                            remove_or_replace_note(obj);
                        } else {
                            types |= 1 << t;
                        }
                    }
                    j += 1;
                }

                types = merge_types(types);

                while i < j {
                    let obj = &mut objs[i];
                    for to_type(obj).each |&t| {
                        if (types & (1 << t)) == 0 {
                            remove_or_replace_note(obj);
                        }
                    }
                    i += 1;
                }
            }
        }

        for uint::range(0, NLANES) |lane| {
            let lane0 = Lane(lane);

            const LNDONE: uint = 0;
            const LNSTART: uint = 1;
            const VISIBLE: uint = 2;
            const INVISIBLE: uint = 3;
            const BOMB: uint = 4;
            let to_type = |obj: &Obj| -> Option<uint> {
                match obj.data {
                    Visible(lane,_) if lane == lane0 => Some(VISIBLE),
                    Invisible(lane,_) if lane == lane0 => Some(INVISIBLE),
                    LNStart(lane,_) if lane == lane0 => Some(LNSTART),
                    LNDone(lane,_) if lane == lane0 => Some(LNDONE),
                    Bomb(lane,_,_) if lane == lane0 => Some(BOMB),
                    _ => None,
                }
            };

            let mut inside = false;
            do sanitize(bms.objs, to_type) |mut types| {
                const LNMASK: uint = (1 << LNSTART) | (1 << LNDONE);

                // remove overlapping LN endpoints altogether
                if (types & LNMASK) == LNMASK { types &= !LNMASK; }

                // remove prohibited types according to inside
                if inside {
                    types &= !((1 << LNSTART) | (1 << VISIBLE) | (1 << BOMB));
                } else {
                    types &= !(1 << LNDONE);
                }

                // invisible note cannot overlap with long note endpoints
                if (types & LNMASK) != 0 { types &= !(1 << INVISIBLE); }

                // keep the most important (lowest) type, except for
                // BOMB/INVISIBLE combination
                let lowest = types & -types;
                if lowest == (1 << INVISIBLE) {
                    types = lowest | (types & (1 << BOMB));
                } else {
                    types = lowest;
                }

                if (types & (1 << LNSTART)) != 0 {
                    inside = true;
                } else if (types & (1 << LNDONE)) != 0 {
                    inside = false;
                }

                types
            }

            if inside {
                // remove last starting longnote which is unfinished
                match bms.objs.rposition(|obj| to_type(obj).is_some()) {
                    Some(pos) if bms.objs[pos].is_lnstart() =>
                        remove_or_replace_note(&mut bms.objs[pos]),
                    _ => ()
                }
            }
        }

        sanitize(bms.objs,
                 |&obj| match obj.data {
                            SetBGA(Layer1,_) => Some(0),
                            SetBGA(Layer2,_) => Some(1),
                            SetBGA(Layer3,_) => Some(2),
                            SetBGA(PoorBGA,_) => Some(3),
                            SetBPM(*) => Some(4),
                            Stop(*) => Some(5),
                            _ => None,
                        },
                 |types| types);
    }

    pub struct KeySpec {
        /// The number of lanes on the left side. (C: `nleftkeys`) This number
        /// is significant only when Couple Play is used.
        keysplit: int,
        /// The order of significant lanes. The first `nleftkeys` lanes go to
        /// the left side and the remaining lanes (C: `nrightkeys`) go to
        /// the right side. (C: `keyorder`)
        keyorder: ~[Lane],
        /// The type of lanes. (C: `keykind`)
        keykind: ~[Option<int> * 72] // XXX 72=NLANES
    }

    /// Removes insignificant objects (i.e. not in visible lanes) and ensures
    /// that there is no `Deleted` object. (C: `analyze_and_compact_bms`)
    pub fn compact_bms(bms: &mut Bms, keyspec: &KeySpec) {
        for vec::each_mut(bms.objs) |obj| {
            match obj.object_lane() {
                Some(lane) =>
                    if keyspec.keykind[*lane].is_none() {
                        remove_or_replace_note(obj)
                    },
                None => ()
            }
        }

        do bms.objs.retain |&obj| { obj.data == Deleted }
    }

    /// Analyzes the loaded BMS file. (C: `analyze_and_compact_bms`)
    pub fn analyze_bms(bms: &Bms) -> BmsInfo {
        let mut infos = BmsInfo { originoffset: 0.0, hasbpmchange: false,
                                  haslongnote: false, nnotes: 0, maxscore: 0 };

        for bms.objs.each |&obj| {
            infos.haslongnote |= obj.is_lnstart();
            infos.hasbpmchange |= obj.is_setbpm();

            if obj.is_lnstart() || obj.is_visible() {
                infos.nnotes += 1;
                if obj.time < 1.0 { infos.originoffset = -1.0; }
            }
        }

        for int::range(0, infos.nnotes) |i| {
            let ratio = (i as float) / (infos.nnotes as float);
            infos.maxscore += (300.0 * (1.0 + ratio)) as int;
        }

        infos
    }

}

//============================================================================
// user interface

pub mod ui {
    use sdl::video::*;

    /// (C: `putpixel`)
    pub fn put_pixel(surface: &Surface, x: uint, y: uint, c: Color) {
        // TODO Angolmois doesn't really lock the surface since it assumes
        // SW surface or double-buffered HW surface...
        unsafe {
            let ll_surface = *surface.raw;
            let mapped = c.to_mapped(ll_surface.format);
            let pitch = ll_surface.pitch as uint;
            let len = ll_surface.h as uint * pitch / 4;
            let pixels: &mut [u32] =
                cast::transmute((ll_surface.pixels, len));
            pixels[x + y * pitch / 4] = mapped;
        }
    }

    /// 8x16 resizable bitmap font.
    pub struct Font {
        /// Font data used for zoomed font reconstruction. (C: `fontdata`)
        ///
        /// This array has ..
        glyphs: ~[u16],
        /// Precalculated zoomed font per zoom factor. (C: `zoomfont`)
        pixels: ~[~[~[u32]]]
    }

    pub fn Font() -> Font {
        // Delta-coded code words. (C: `words`)
        let dwords = [0, 2, 6, 2, 5, 32, 96, 97, 15, 497, 15, 1521, 15, 1537,
            16, 48, 176, 1, 3, 1, 3, 7, 1, 4080, 4096, 3, 1, 8, 3, 4097, 4080,
            16, 16128, 240, 1, 2, 9, 3, 8177, 15, 16385, 240, 15, 1, 47, 721,
            143, 2673, 2, 6, 7, 1, 31, 17, 16, 63, 64, 33, 0, 1, 2, 1, 8, 3];

        // LZ77-compressed indices to code words:
        // - Byte 33..97 encodes a literal code word 0..64;
        // - Byte 98..126 encodes an LZ77 length distance pair with length
        //   3..31; the following byte 33..126 encodes a distance 1..94.
        let indices = ~"!!7a/&/&s$7a!f!'M*Q*Qc$(O&J!!&J&Jc(e!2Q2Qc$-Bg2m!2bB["
            + ~"Q7Q2[e&2Q!Qi>&!&!>UT2T2&2>WT!c*T2GWc8icM2U2D!.8(M$UQCQ-jab!'U"
            + ~"*2*2*2TXbZ252>9ZWk@*!*!*8(J$JlWi@cxQ!Q!d$#Q'O*?k@e2dfejcNl!&J"
            + ~"TLTLG_&J>]c*&Jm@cB&J&J7[e(o>pJM$Qs<7[{Zj`Jm40!3!.8(M$U!C!-oR>"
            + ~"UQ2U2]2a9Y[S[QCQ2GWk@*M*Q*B*!*!g$aQs`G8.M(U$[!Ca[o@Q2Q!IJQ!Q!"
            + ~"c,GWk@787M6U2C2d!a[2!2k?!bnc32>[u`>Uc4d@b(q@abXU!D!.8(J&J&d$q"
            + ~"`Q2IXu`g@Q2aWQ!q@!!ktk,x@M$Qk@3!.8(M$U!H#W'O,?4m_f!7[i&n!:eX5"
            + ~"ghCk=>UQ2Q2U2Dc>J!!&J&b&k@J)LKg!GK!)7Wk@'8,M=UWCcfa[c&Q2l`f4I"
            + ~"f(Q2G[l@MSUQC!2!2c$Q:RWGOk@,[<2WfZQ2U2D2.l`a[eZ7f(!2b2|@b$j!>"
            + ~"MSUQCc6[2W2Q:RWGOk@Q2Q2c$a[g*Ql`7[&J&Jk$7[l`!Qi$d^GWk@U2D2.9("
            + ~"[$[#['[,@<2W2k@!2!2m$a[l`:^[a[a[T2Td~c$k@d2:R[V[a@_b|o@,M=UWC"
            + ~"gZU:EW.Ok@>[g<G[!2!2d$k@Ug@Q2V2a2IW_!Wt`Ih*q`!2>WQ!Q!c,Gk_!7["
            + ~"&J&Jm$k@gti$m`k:U:EW.O(?s@T2Tb$a[CW2Qk@M+U:^[GbX,M>U`[WCO-l@'"
            + ~"U,D<.W(O&J&Je$k@a[Q!U!]!G8.M(U$[!Ca[k@*Q!Q!l$b2m!+!:#W'O,?4!1"
            + ~"n;c`*!*!l$h`'8,M=UWCO-pWz!a[i,#Q'O,?4~R>QQ!Q!aUQ2Q2Q2aWl=2!2!"
            + ~"2>[e<c$G[p`dZcHd@l`czi|c$al@i`b:[!2Un`>8TJTJ&J7[&b&e$o`i~aWQ!"
            + ~"c(hd2!2!2>[g@e$k]epi|e0i!bph(d$dbGWhA2!2U2D2.9(['[,@<2W2k`*J*"
            + ~"?*!*!k$o!;[a[T2T2c$c~o@>[c6i$p@Uk>GW}`G[!2!2b$h!al`aWQ!Q!Qp`f"
            + ~"VlZf@UWb6>eX:GWk<&J&J7[c&&JTJTb$G?o`c~i$m`k@U:EW.O(v`T2Tb$a[F"
            + ~"p`M+eZ,M=UWCO-u`Q:RWGO.A(M$U!Ck@a[]!G8.M(U$[!Ca[i:78&J&Jc$%[g"
            + ~"*7?e<g0w$cD#iVAg*$[g~dB]NaaPGft~!f!7[.W(O";

        /// (C: `fontdecompress`)
        fn decompress(dwords: &[u16], indices: &str) -> ~[u16] {
            let mut words = ~[0];
            for dwords.each |&delta| {
                let last = {*words.last()}; // XXX #4666
                words.push(last + delta);
            }

            let nindices = indices.len();
            let mut i = 0;
            let mut glyphs = ~[];
            while i < nindices {
                let code = indices[i] as uint;
                i += 1;
                match code {
                    33..97 => glyphs.push(words[code - 33]),
                    98..126 => {
                        let length = code - 95; // code=98 -> length=3
                        let distance = indices[i] as uint - 32;
                        i += 1;
                        let start = glyphs.len() - distance;
                        for uint::range(start, start + length) |i| {
                            glyphs.push(glyphs[i]);
                        }
                    },
                    _ => fail!(~"unexpected codeword")
                }
            }
            glyphs
        }

        let glyphs = decompress(dwords, indices);
        fail_unless!(glyphs.len() == 3072);
        Font { glyphs: glyphs, pixels: ~[] }
    }

    pub impl Font {
        /// (C: `fontprocess`)
        fn create_zoomed_font(&mut self, zoom: uint) {
            fail_unless!(zoom > 0);
            if zoom < self.pixels.len() && !self.pixels[zoom].is_empty() {
                return;
            }

            let nrows = 16;
            let nglyphs = self.glyphs.len() / nrows / 2;
            let mut pixels = vec::from_elem(nglyphs,
                                            vec::from_elem(zoom*nrows, 0));

            let put_zoomed_pixel = |glyph: uint, row: uint, col: uint,
                                    v: u32| {
                let zoomrow = row * zoom;
                let zoomcol = col * zoom;
                for uint::range(0, zoom) |r| {
                    for uint::range(0, zoom) |c| {
                        let mut mask = 0;
                        if r + c >= zoom    { mask |= 1; } // lower right
                        if r > c            { mask |= 2; } // lower left
                        if r < c            { mask |= 4; } // upper right
                        if r + c < zoom - 1 { mask |= 8; } // upper left

                        // if `zoom` is odd, drawing four corner triangles
                        // leaves one center pixel intact since we don't draw
                        // diagonals for aesthetic reason. such case must be
                        // specially handled.
                        if (v & mask) != 0 || v == 15 {
                            pixels[glyph][zoomrow+r] |= 1 << (zoomcol+c);
                        }
                    }
                }
            };

            let mut i = 0;
            for uint::range(0, nglyphs) |glyph| {
                for uint::range(0, nrows) |row| {
                    let data = (self.glyphs[i] as u32 << 16) |
                               (self.glyphs[i+1] as u32);
                    i += 2;
                    for uint::range(0, 8) |col| {
                        let v = (data >> (4 * col)) & 15;
                        put_zoomed_pixel(glyph, row, col, v);
                    }
                }
            }
            self.pixels.grow_set(zoom, &~[], pixels);
		}

        /// Prints a glyph with given position and top/bottom color. This
        /// method is distinct from `print_glyph` since the glyph #95 is used
        /// for the tick marker (character code -1 in C).
        pub fn print_glyph(&self, surface: &Surface, x: uint, y: uint,
                           zoom: uint, glyph: uint, topcolor: Color,
                           bottomcolor: Color) {
            fail_unless!(!self.pixels[zoom].is_empty());
            for uint::range(0, 16 * zoom) |iy| {
                let row = self.pixels[zoom][glyph][iy];
                for uint::range(0, 8 * zoom) |ix| {
                    if ((row >> ix) & 1) != 0 {
                        // TODO blend
                        put_pixel(surface, x + ix, y + iy, topcolor);
                    }
                }
            }
        }

        /// (C: `printchar`)
        pub fn print_char(&self, surface: &Surface, x: uint, y: uint,
                          zoom: uint, c: char, topcolor: Color,
                          bottomcolor: Color) {
            if !char::is_whitespace(c) {
                let c = c as uint;
                let glyph = if 32 <= c && c < 126 { c - 32 } else { 0 };
                self.print_glyph(surface, x, y, zoom, glyph,
                                 topcolor, bottomcolor);
            }
        }

        /// (C: `printstr`)
        pub fn print_string(&self, surface: &Surface, x: uint, y: uint,
                            zoom: uint, s: &str, topcolor: Color,
                            bottomcolor: Color) {
            let mut x = x;
            for str::each_char(s) |c| { // Rust: `s.each_char` is ambiguous
                self.print_char(surface, x, y, zoom, c,
                                topcolor, bottomcolor);
                x += 8 * zoom;
            }
        }
    }

    pub fn init_video(exclusive: bool, fullscreen: bool) -> ~Surface {
        let result =
            if exclusive {
                set_video_mode(256, 256, 32, [SWSurface], [DoubleBuf])
            } else if !fullscreen {
                set_video_mode(800, 600, 32, [SWSurface], [DoubleBuf])
            } else {
                set_video_mode(800, 600, 32, [], [Fullscreen])
            };
        let screen =
            match result {
                Ok(screen) => screen,
                Err(err) => die!("SDL Video Initialization Failure: %s", err)
            };
        if !exclusive {
            // TODO: SDL_ShowCursor(SDL_DISABLE);
        }
        // TODO: SDL_WM_SetCaption(VERSION, 0);
        screen
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
        /// (C: `bmspath`)
        bmspath: Option<~str>,
        /// (C: `opt_mode`)
        mode: Mode,
        /// (C: `opt_modf`)
        modf: Modf,
        /// (C: `opt_bga`)
        bga: Bga,
        /// (C: `opt_showinfo`)
        showinfo: bool,
        /// (C: `opt_fullscreen`)
        fullscreen: bool,
        /// (C: `opt_joystick`)
        joystick: Option<int>,
        /// (C: `preset`)
        preset: Option<~str>,
        /// (C: `leftkeys`)
        leftkeys: Option<~str>,
        /// (C: `rightkeys`)
        rightkeys: Option<~str>,
        /// (C: `playspeed`)
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
        use sdl::video::{Color, RGB};
        use sdl::event::{KeyEvent, EscapeKey, wait_event};

        do ::sdl::start {
            let screen = ::ui::init_video(false, false);
            let mut font = ::ui::Font();
            font.create_zoomed_font(1);
            font.create_zoomed_font(2);
            font.create_zoomed_font(3);

            screen.fill(RGB(0, 0, 0));
            let white = RGB(255, 255, 255);
            font.print_string(screen, 20, 20, 1, "Angolmois Rust Edition", white, white);
            font.print_string(screen, 20, 40, 2, "Hello world!", white, white);
            font.print_string(screen, 20, 80, 3, "GREAT", white, white);
            screen.flip();

            loop {
                match ::sdl::event::wait_event() {
                    KeyEvent(EscapeKey, _, _, _) => break,
                    _ => ()
                }
            }
        }

        /*
        repr::write_repr(io::stdout(), opts);
        io::println("");

        let r = rand::task_rng();
        let mut bms = match opts.bmspath {
            Some(copy path) =>
                match ::parser::parse_bms(path, r) {
                    Ok(bms) => ~bms,
                    Err(err) => die!("Couldn't load BMS file: %s", err)
                },
            None => fail!(~"TODO")
        };

        ::core::repr::write_repr(io::stdout(), &bms.objs);
        io::println("");

        ::parser::sanitize_bms(bms);

        ::core::repr::write_repr(io::stdout(), &bms.objs);
        io::println("");
        */

        /*
        let bmspath = Path(copy *opts.bmspath.get_ref()).dir_path();
        do ::sdl::start {
            ::sdl::init([::sdl::InitAudio]);
            ::sdl::mixer::open(44100,
                               ::sdl::audio::S16AudioFormat,
                               ::sdl::audio::Stereo,
                               2048);
            for bms.sndpath.each |&path| {
                for path.each |&path| {
                    //let path = str::replace(path,~".wav",~".ogg");
                    let path = bmspath.push_rel(&Path(path));
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
        */
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