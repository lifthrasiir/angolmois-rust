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

/*!
 * This is a direct, one-to-one translation of Angolmois to Rust programming
 * language. [Angolmois][angolmois] is a [BM98][bm98]-like minimalistic music
 * video game which supports the [BMS format][bms] for playing.
 * 
 * [angolmois]: http://mearie.org/projects/angolmois/
 * [bms]: http://en.wikipedia.org/wiki/Be-Music_Source
 * [bm98]: http://bm98.yaneu.com/bm98/
 * 
 * Angolmois is a combination of string parsing, path manipulation,
 * two-dimensional graphics and complex game play carefully packed into some
 * thousand lines of code. This translation is intended to provide an example
 * of translating a moderately-sized C code to Rust, and also to investigate
 * additional library supports required for such moderately-sized programs.
 * 
 * Angolmois is distributed under GNU GPL version 2+, so is this translation.
 * The portions of it is intended to be sent as a patch to Rust, so those
 * portions are licensed under Apache License 2.0 and MIT license.
 * 
 * Unlike the original Angolmois code (which sacrifices most comments due to
 * code size concerns), the Rust version has much more comments which can be
 * beneficial for understanding Angolmois itself too.
 * 
 * # Key
 * 
 * The following notations are used in the comments and documentations.
 * 
 * * (C: ...) - variable/function corresponds to given name in the C code.
 * * Rust: ... - suboptimal translation with a room for improvement in Rust.
 *   often contains a Rust issue number like #1234.
 * * XXX - should be fixed as soon as Rust issue is gone.
 */

#[link(name = "angolmois",
       vers = "2.0.0-alpha2",
       uuid = "0E85EA95-BE62-4E0F-B811-8C1EC46C46EC",
       url = "https://github.com/lifthrasiir/angolmois/")];

#[comment = "Angolmois"];
#[license = "GPLv2+"];

extern mod std;
extern mod sdl;

use core::io::{ReaderUtil, WriterUtil};
use core::num::Round;

// see below for specifics.
use self::util::str::*;
use self::util::option::*;
use self::util::iter::*;
use self::util::io::*;

/// Returns a version string. (C: `VERSION`)
pub fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

//============================================================================
// utility declarations

/// Returns an executable name used in the command line if any. (C: `argv0`)
pub fn exename() -> ~str {
    let args = os::args();
    if args.is_empty() {~"angolmois"} else {copy args[0]}
}

/// Utility functions.
#[macro_escape]
pub mod util {

    /// Immediately terminates the program with given exit code.
    pub fn exit(exitcode: int) -> ! {
        // Rust: `os::set_exit_status` doesn't immediately terminate
        //       the program.
        unsafe { libc::exit(exitcode as libc::c_int); }
    }

    /// Exits with an error message. Internally used in the `die!` macro
    /// below.
    pub fn die(s: ~str) -> ! {
        ::core::io::stderr().write_line(fmt!("%s: %s", ::exename(), s));
        exit(1)
    }

    /// Prints an warning message. Internally used in the `warn!` macro below.
    pub fn warn(s: ~str) {
        ::core::io::stderr().write_line(fmt!("*** Warning: %s", s));
    }

    // Exits with a formatted error message. (C: `die`)
    //
    // Rust: this comment cannot be a doc comment (yet).
    macro_rules! die(
        ($($e:expr),+) => (::util::die(fmt!($($e),+)))
    )

    // Prints a formatted warning message. (C: `warn`)
    macro_rules! warn(
        ($($e:expr),+) => (::util::warn(fmt!($($e),+)))
    )

    /**
     * String utilities for Rust. Parallels to `core::str`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/str.rs` and are not subject to the above copyright notice.
     */
    pub mod str {
        use core::str::*;

        static tag_cont_u8: u8 = 128u8; // copied from libcore/str.rs

        /// Iterates over the chars in a string, with byte indices.
        pub fn each_chari_byte(s: &str, it: &fn(uint, char) -> bool) {
            let mut pos = 0u;
            let len = s.len();
            while pos < len {
                let CharRange {ch, next} = char_range_at(s, pos);
                if !it(pos, ch) { break; }
                pos = next;
            }
        }

        /// Given a potentially invalid UTF-8 byte sequence, fixes an invalid
        /// UTF-8 sequence with given error handler.
        pub fn fix_utf8(v: &[u8], handler: &fn(&[u8]) -> ~[u8]) -> ~[u8] {
            let mut i = 0u;
            let total = vec::len::<u8>(v);
            let mut result = ~[];
            while i < total {
                let chend = i + utf8_char_width(v[i]);
                let mut j = i + 1u;
                while j < total && j < chend && v[j] & 192u8 == tag_cont_u8 {
                    j += 1u;
                }
                if j == chend {
                    assert!(i != chend);
                    result = vec::append(result, v.slice(i, j));
                } else {
                    result = vec::append(result, handler(v.slice(i, j)));
                }
                i = j;
            }
            result
        }

        /// Given a potentially invalid UTF-8 string, fixes an invalid
        /// UTF-8 string with given error handler.
        pub fn fix_utf8_str(s: &str, handler: &fn(&[u8]) -> ~str) -> ~str {
            from_fixed_utf8_bytes(to_bytes(s), handler)
        }

        /// Converts a vector of bytes to a UTF-8 string. Any invalid UTF-8
        /// sequences are fixed with given error handler.
        pub fn from_fixed_utf8_bytes(v: &[u8],
                                     handler: &fn(&[u8]) -> ~str) -> ~str {
            let newhandler: &fn(&[u8]) -> ~[u8] =
                |v: &[u8]| -> ~[u8] { to_bytes(handler(v)) };
            let bytes = fix_utf8(v, newhandler);
            unsafe { raw::from_bytes(bytes) }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `uint::from_str` accepts without a failure, if any.
        pub fn scan_uint(s: &str) -> Option<uint> {
            match find(s, |c| !('0' <= c && c <= '9')) {
                Some(first) if first > 0u => Some(first),
                None if s.len() > 0u => Some(s.len()),
                _ => None
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `int::from_str` accepts without a failure, if any.
        pub fn scan_int(s: &str) -> Option<uint> {
            if s.starts_with(~"-") || s.starts_with(~"+") {
                scan_uint(s.slice_to_end(1u)).map(|&pos| pos + 1u)
            } else {
                scan_uint(s)
            }
        }

        /// Returns a length of the longest prefix of given string, which
        /// `float::from_str` accepts without a failure, if any.
        pub fn scan_float(s: &str) -> Option<uint> {
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
            fn slice_to_end(&self, begin: uint) -> &'self str;

            /// Iterates over the chars in a string, with byte indices.
            fn each_chari_byte(&self, it: &fn(uint, char) -> bool);

            /// Given a potentially invalid UTF-8 string, fixes an invalid
            /// UTF-8 string with given error handler.
            fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

            /// Returns a length of the longest prefix of given string, which
            /// `uint::from_str` accepts without a failure, if any.
            fn scan_uint(&self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which
            /// `int::from_str` accepts without a failure, if any.
            fn scan_int(&self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which
            /// `float::from_str` accepts without a failure, if any.
            fn scan_float(&self) -> Option<uint>;
        }

        impl<'self> StrUtil for &'self str {
            fn slice_to_end(&self, begin: uint) -> &'self str {
                self.slice(begin, self.len())
            }
            fn each_chari_byte(&self, it: &fn(uint, char) -> bool) {
                each_chari_byte(*self, it)
            }
            fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
                fix_utf8_str(*self, handler)
            }
            fn scan_uint(&self) -> Option<uint> { scan_uint(*self) }
            fn scan_int(&self) -> Option<uint> { scan_int(*self) }
            fn scan_float(&self) -> Option<uint> { scan_float(*self) }
        }

        /// A trait which provides `prefix_shifted` method. Similar to
        /// `str::starts_with`, but with swapped `self` and argument.
        pub trait ShiftablePrefix {
            /// Returns a slice of given string with `self` at the start of
            /// the string stripped only once, if any.
            fn prefix_shifted(&self, s: &str) -> Option<~str>;
        }

        impl ShiftablePrefix for char {
            fn prefix_shifted(&self, s: &str) -> Option<~str> {
                if !s.is_empty() {
                    let CharRange {ch, next} = char_range_at(s, 0u);
                    if ch == *self {
                        return Some(s.slice_to_end(next).to_owned());
                    }
                }
                None
            }
        }

        impl<'self> ShiftablePrefix for &'self str {
            fn prefix_shifted(&self, s: &str) -> Option<~str> {
                if s.starts_with(*self) {
                    Some(s.slice_to_end(self.len()).to_owned())
                } else {
                    None
                }
            }
        }

    }

    /**
     * Option utilities for Rust. Parallels to `core::option`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/option.rs` and are not subject to the above copyright notice.
     */
    pub mod option {
        use core::option::*;

        #[inline(always)]
        pub fn filter<T:Copy>(opt: Option<T>, f: &fn(t: T) -> bool)
                                        -> Option<T> {
            match opt {
                Some(t) => if f(t) {Some(t)} else {None},
                None => None
            }
        }

        pub trait CopyableOptionUtil<T:Copy> {
            fn filter(self, f: &fn(x: T) -> bool) -> Option<T>;
        }

        impl<T:Copy> CopyableOptionUtil<T> for Option<T> {
            #[inline(always)]
            fn filter(self, f: &fn(x: T) -> bool) -> Option<T> {
                filter(self, f)
            }
        }

    }

    /**
     * Iterator utilities for Rust. Parallels to `core::iter`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/iter.rs` and are not subject to the above copyright notice.
     */
    pub mod iter {

        pub trait OptionalIter<A> {
            /// Like `each()`, but only iterates through the value inside
            /// options.
            fn each_some(&self, blk: &fn(v: &A) -> bool);
        }

    }

    /**
     * Vector utilities for Rust. Parallels to `core::vec`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/vec.rs` and are not subject to the above copyright notice.
     */
    pub mod vec {
        use core::vec::*;

        /// Like `each()`, but only iterates through the value inside options.
        #[inline(always)]
        pub fn each_some<'r,A>(vec: &'r [Option<A>],
                               blk: &fn(v: &'r A) -> bool) {
            for each(vec) |e| {
                for e.each |v| {
                    if !blk(v) { return; }
                }
            }
        }

        impl<'self,A> ::util::iter::OptionalIter<A> for &'self [Option<A>] {
            #[inline(always)]
            fn each_some(&self, blk: &fn(v: &'self A) -> bool) {
                each_some(*self, blk)
            }
        }

        impl<A> ::util::iter::OptionalIter<A> for ~[Option<A>] {
            #[inline(always)]
            fn each_some(&self, blk: &fn(v: &'self A) -> bool) {
                each_some(*self, blk)
            }
        }

        impl<A> ::util::iter::OptionalIter<A> for @[Option<A>] {
            #[inline(always)]
            fn each_some(&self, blk: &fn(v: &'self A) -> bool) {
                each_some(*self, blk)
            }
        }

    }

    /**
     * I/O utilities for Rust. Parallels to `core::io`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/io.rs` and are not subject to the above copyright notice.
     */
    pub mod io {

        /// Extensions to `ReaderUtil`.
        pub trait ReaderUtilEx {
            /// Reads up until the first '\n' char (which is not returned),
            /// or EOF. Any invalid UTF-8 sequences are fixed with given
            /// error handler.
            fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str)
                                        -> ~str;

            /// Iterates over every line until the iterator breaks or EOF. Any
            /// invalid UTF-8 sequences are fixed with given error handler.
            fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str,
                                    it: &fn(&str) -> bool);
        }

        impl<T: Reader> ReaderUtilEx for T {
            fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str)
                                        -> ~str {
                let mut bytes = ~[];
                loop {
                    let ch = self.read_byte();
                    if ch == -1 || ch == 10 { break; }
                    bytes.push(ch as u8);
                }
                ::util::str::from_fixed_utf8_bytes(bytes, handler)
            }

            fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str,
                                    it: &fn(&str) -> bool) {
                while !self.eof() {
                    if !it(self.read_and_fix_utf8_line(handler)) { break; }
                }
            }
        }

    }

    /**
     * Comparison routines for Rust. Parallels to `core::cmp`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/cmp.rs` and are not subject to the above copyright notice.
     */
    pub mod cmp {

        #[inline(always)]
        pub fn clamp<T:Ord>(low: T, v: T, high: T) -> T {
            if v < low {low} else if v > high {high} else {v}
        }

    }

    /**
     * Hash table routines for Rust. Parallels to `core::hashmap`.
     *
     * NOTE: Some of these additions will be eventually sent to
     * `libcore/hashmap.rs` and are not subject to the above copyright notice.
     */
    pub mod hashmap {
        use core::hashmap::*;
        use core_compat::hashmap::*;

        // TODO make this constructible from any suitable iterator
        pub fn map_from_vec<K:Eq+Hash+IterBytes,V>(items: &[(K,V)])
                                        -> HashMap<K,V> {
            let mut map = HashMap::new();
            map.reserve_at_least(items.len());
            for items.each |&(k,v)| { map.insert(k, v); }
            map
        }

        // TODO make this constructible from any suitable iterator
        pub fn set_from_vec<V:Eq+Hash+IterBytes>(items: &[V]) -> HashSet<V> {
            let mut set = HashSet::new();
            set.reserve_at_least(items.len());
            for items.each |&v| { set.insert(v); }
            set
        }
    }

    pub mod sdl {
        #[doc(hidden)]
        pub extern {
            fn SDL_Delay(ms: u32);
            fn SDL_GetTicks() -> u32;
        }

        pub fn delay(msec: uint) {
            unsafe { SDL_Delay(msec as u32); }
        }

        pub fn ticks() -> uint {
            unsafe { SDL_GetTicks() as uint }
        }
    }

    /*
     * A lexer barely powerful enough to parse BMS format. Comparable to C's
     * `sscanf`.
     *
     * `lex!(e; fmt1, fmt2, ..., fmtN)` returns an expression that evaluates
     * to true if and only if all format specification is consumed. The format
     * specification (analogous to `sscanf`'s `%`-string) is as follows:
     *
     * - `ws`: Consumes one or more whitespace. (C: `%*[ \t\r\n]` or similar)
     * - `ws*`: Consumes zero or more whitespace. (C: ` `)
     * - `int [-> e2]`: Consumes an integer and optionally saves it to `e2`.
     *   (C: `%d` and `%*d`, but does not consume preceding whitespace)
     *   The integer syntax is slightly limited compared to `sscanf`.
     * - `float [-> e2]`: Consumes a real number and optionally saves it to
     *   `e2`. (C: `%f` etc.) Again, the real number syntax is slightly
     *   limited; especially an exponent support is missing.
     * - `str [-> e2]`: Consumes a remaining input as a string and optionally
     *   saves it to `e2`. The string is at least one character long.
     *   (C: not really maps to `sscanf`, similar to `fgets`) Implies `!`.
     *   It can be followed by `ws*` which makes the string right-trimmed.
     * - `str* [-> e2]`: Same as above but the string can be empty.
     * - `char [-> e2]`: Consumes exactly one character and optionally saves
     *   it to `e2`. Resulting character can be whitespace. (C: `%1c`)
     * - `!`: Ensures that the entire string has been consumed. Should be
     *   the last format specification.
     * - `"foo"` etc.: An ordinary expression is treated as a literal string
     *   or literal character.
     *
     * For the use in Angolmois, the following specifications have been added:
     *
     * - `Key [-> e2]`: Consumes a two-letter alphanumeric key and optionally
     *   saves it to `e2`. (C: `%2[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ]` etc.
     *   followed by a call to `key2index`)
     * - `Measure [-> e2]`: Consumes exactly three digits and optionally saves
     *   it to `e2`. (C: `%1[0123456789]%1[0123456789]%1[0123456789]` followed
     *   by a call to `atoi`)
     */
    // Rust: - there is no `libc::sscanf` due to the varargs. maybe regex
    //         support will make this obsolete in the future, but not now.
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
                // Rust: we should be able to avoid a copy here. (#5550)
                $dst = _line.trim_right().to_owned(); // XXX #5550
                lex!(""; $($tail)*) // optimization!
            } else {
                false
            }
        });
        ($e:expr; str -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() {
                $dst = _line.to_owned(); // XXX #5550
                lex!(""; $($tail)*) // optimization!
            } else {
                false
            }
        });
        ($e:expr; str* -> $dst:expr, ws*, $($tail:tt)*) => ({
            let _line: &str = $e;
            $dst = _line.trim_right().to_owned(); // XXX #5550
            lex!(""; $($tail)*) // optimization!
        });
        ($e:expr; str* -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            $dst = _line.to_owned(); // XXX #5550
            lex!(""; $($tail)*) // optimization!
        });
        ($e:expr; char -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() {
                let str::CharRange {ch:_ch, next:_pos} =
                    str::char_range_at(_line, 0);
                $dst = _ch;
                lex!(_line.slice_to_end(_pos); $($tail)*)
            } else {
                false
            }
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
        ($e:expr; char, $($tail:tt)*) => ({
            let mut _dummy: char = '\x00';
            lex!($e; char -> _dummy, $($tail)*)
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
        ($e:expr; char -> $dst:expr) => (lex!($e; char -> $dst, ));
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
        ($e:expr; char) => (lex!($e; char, ));
        // start Angolmois-specific
        ($e:expr; Key) => (lex!($e; Key, ));
        ($e:expr; Measure) => (lex!($e; Measure, ));
        // end Angolmois-specific
        ($e:expr; $lit:expr) => (lex!($e; $lit, ))
    )

}

/// For the compatibility with the 0.6 release.
mod core_compat {
    pub mod hashmap {
        pub use core::hashmap::*;
        #[cfg(legacy)] pub use HashMap = core::hashmap::linear::LinearMap;
        #[cfg(legacy)] pub use HashSet = core::hashmap::linear::LinearSet;
    }
}

//============================================================================
// bms parser

/**
 * BMS parser module.
 *
 * # Structure
 *
 * The BMS format is a plain text format with most directives start with
 * optional whitespace followed by `#`. Besides the metadata (title, artist
 * etc.), a BMS file is a map from the time position to various game play
 * elements (henceforth "objects") and other effects including BGM and BGA
 * changes. It also contains preprocessor directives used to randomize some or
 * all parts of the BMS file, which would only make sense in the loading time.
 *
 * The time position is a virtual time divided by an unit of (musical)
 * measure. It is related to the actual time by the current Beats Per Minute
 * (BPM) value which can, well, also change during the game play. Consequently
 * it is convenient to refer the position in terms of measures, which the BMS
 * format does: the lines `#xxxyy:AABBCC...` indicates that the measure number
 * `xxx` contains objects or object-like effects (of the type specified by
 * `yy`, henceforth "channels"), evenly spaced throughout the measure and
 * which data values are `AA`, `BB`, `CC` respectively.
 *
 * An alphanumeric identifier (henceforth "alphanumeric key") like `AA` or
 * `BB` may mean that the actual numeric value interpreted as base 16 or 36
 * (depending on the channel), or a reference to other assets (e.g. `#BMPAA
 * foo.png`) or complex values specified by other commands (e.g. `#BPMBB
 * 192.0`). In most cases, an identifier `00` indicates an absence of objects
 * or object-like effects at that position.
 *
 * More detailed information about BMS format, including surveys about
 * how different implementations (so called BMS players) react to
 * underspecified features or edge cases, can be found at [BMS command
 * memo][bmscmds].
 *
 * [bmscmds]: http://hitkey.nekokan.dyndns.info/cmds.htm
 */
pub mod parser {
    use core::rand::*;

    //------------------------------------------------------------------------
    // alphanumeric key

    /// Two-letter alphanumeric identifier used for virtually everything,
    /// including resource management, variable BPM and chart specification.
    #[deriving(Eq)]
    pub struct Key(int);

    /// The number of all possible alphanumeric keys. (C: `MAXKEY`)
    pub static MAXKEY: int = 36*36;

    pub impl Key {
        /// Returns if the alphanumeric key is in the proper range. Angolmois
        /// supports the full range of 00-ZZ (0-1295) for every case.
        fn is_valid(self) -> bool {
            0 <= *self && *self < MAXKEY
        }

        /// Re-reads the alphanumeric key as a hexadecimal number if possible.
        /// This is required due to handling of channel #03 (BPM is expected
        /// to be in hexadecimal).
        fn to_hex(self) -> Option<int> {
            let sixteens = *self / 36, ones = *self % 36;
            if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)}
            else {None}
        }
    }

    impl Ord for Key {
        // Rust: it is very easy to make an infinite recursion here.
        fn lt(&self, other: &Key) -> bool { **self < **other }
        fn le(&self, other: &Key) -> bool { **self <= **other }
        fn ge(&self, other: &Key) -> bool { **self >= **other }
        fn gt(&self, other: &Key) -> bool { **self > **other }
    }

    impl ToStr for Key {
        /// Returns a two-letter representation of alphanumeric key.
        /// (C: `TO_KEY`)
        fn to_str(&self) -> ~str {
            assert!(self.is_valid());
            let map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            fmt!("%c%c", map[**self / 36] as char, map[**self % 36] as char)
        }
    }

    //------------------------------------------------------------------------
    // lane and key kinds

    /// A game play element mapped to the single input element (for example,
    /// button) and the screen area (henceforth "lane").
    #[deriving(Eq)]
    pub struct Lane(uint);

    /// The maximum number of lanes. (C: `NNOTECHANS`)
    pub static NLANES: uint = 72;

    pub impl Lane {
        /// Converts the channel number to the lane number.
        fn from_channel(chan: Key) -> Lane {
            let player = match *chan / 36 {
                1 | 3 | 5 | 0xD => 0,
                2 | 4 | 6 | 0xE => 1,
                _ => fail!(~"non-object channel")
            };
            Lane(player * 36 + *chan as uint % 36)
        }
    }

    /**
     * Key kinds. They define an appearance of particular lane, but otherwise
     * ignored for the game play. Angolmois supports several key kinds in
     * order to cover many potential uses. (C: `KEYKIND_MNEMONICS`)
     *
     * # Defaults
     *
     * For BMS/BME, channels #11/13/15/19 and #21/23/25/29 use `WhiteKey`,
     * #12/14/18 and #22/24/28 use `BlackKey`, #16 and #26 use `Scratch` ,
     * #17 and #27 use `FootPedal`.
     *
     * For PMS, channels #11/17/25 use `Button1`, #12/16/24 use `Button2`,
     * #13/19/23 use `Button3`, #14/18/22 use `Button4`, #15 uses `Button5`.
     */
    #[deriving(Eq)]
    pub enum KeyKind {
        /// White key, which mimics a real white key in the musical keyboard.
        WhiteKey,
        /// White key, but rendered yellow. This is used for simulating
        /// the O2Jam interface which has one yellow lane (mapped to spacebar)
        /// in middle of six other lanes (mapped to normal keys).
        WhiteKeyAlt,
        /// Black key, which mimics a real black key in the keyboard but
        /// rendered light blue as in Beatmania and other games.
        BlackKey,
        /// Scratch, rendered red. Scratch lane is wider than other "keys" and
        /// normally doesn't count as a key.
        Scratch,
        /// Foot pedal, rendered green. Otherwise has the same properties as
        /// scratch. The choice of color follows that of EZ2DJ, one of
        /// the first games that used this game element.
        FootPedal,
        /// White button. This and following "button"s come from Pop'n Music,
        /// which has nine colored buttons. (White buttons constitute 1st and
        /// 9th of Pop'n Music buttons.) The "button"s are wider than
        /// aforementioned "keys" but narrower than scratch and foot pedal.
        Button1,
        /// Yellow button (2nd and 8th of Pop'n Music buttons).
        Button2,
        /// Green button (3rd and 7th of Pop'n Music buttons).
        Button3,
        /// Navy button (4th and 6th of Pop'n Music buttons).
        Button4,
        /// Red button (5th of Pop'n Music buttons).
        Button5,
    }

    pub impl KeyKind {
        /// Returns a list of all supported key kinds.
        //
        // Rust: can this method be generated on the fly?
        fn all() -> &'static [KeyKind] {
            &[WhiteKey, WhiteKeyAlt, BlackKey, Scratch, FootPedal,
              Button1, Button2, Button3, Button4, Button5]
        }

        /// Converts a mnemonic character to an appropriate key kind. Used
        /// for parsing a key specification (see also `KeySpec`).
        fn from_char(c: char) -> Option<KeyKind> {
            match c {
                'a' => Some(WhiteKey),
                'y' => Some(WhiteKeyAlt),
                'b' => Some(BlackKey),
                's' => Some(Scratch),
                'p' => Some(FootPedal),
                'q' => Some(Button1),
                'w' => Some(Button2),
                'e' => Some(Button3),
                'r' => Some(Button4),
                't' => Some(Button5),
                _   => None
            }
        }

        /// Converts an appropriate key kind to a mnemonic character. Used
        /// for environment variables (see also TODO).
        fn to_char(self) -> char {
            match self {
                WhiteKey    => 'a',
                WhiteKeyAlt => 'y',
                BlackKey    => 'b',
                Scratch     => 's',
                FootPedal   => 'p',
                Button1     => 'w',
                Button2     => 'e',
                Button3     => 'r',
                Button4     => 't',
                Button5     => 's'
            }
        }

        /**
         * Returns true if a kind counts as a "key". (C: `KEYKIND_IS_KEY`)
         *
         * This affects the number of keys displayed in the loading screen,
         * and reflects a common practice of counting "keys" in many games
         * (e.g. Beatmania IIDX has 8 lanes including one scratch but commonly
         * said to have 7 "keys").
         */
        fn counts_as_key(self) -> bool {
            self != Scratch && self != FootPedal
        }
    }

    //------------------------------------------------------------------------
    // object parameters

    /// Sound reference.
    #[deriving(Eq)]
    pub struct SoundRef(Key);

    /// Image reference.
    #[deriving(Eq)]
    pub struct ImageRef(Key);

    /// BGA layers. (C: `enum BGA_type`)
    #[deriving(Eq)]
    pub enum BGALayer {
        /// The lowest layer. BMS channel #04. (C: `BGA_LAYER`)
        Layer1 = 0,
        /// The middle layer. BMS channel #07. (C: `BGA2_LAYER`)
        Layer2 = 1,
        /// The highest layer. BMS channel #0A. (C: `BGA3_LAYER`)
        Layer3 = 2,
        /// The layer only displayed shortly after the MISS grade. It is
        /// technically not over `Layer3`, but several extensions to BMS
        /// assumes it. BMS channel #06. (C: `POORBGA_LAYER`)
        PoorBGA = 3
    }

    /// The number of BGA layers.
    static NLAYERS: uint = 4;

    /// Beats per minute. Used as a conversion factor between the time
    /// position and actual time in BMS.
    #[deriving(Eq)]
    pub struct BPM(float);

    pub impl BPM {
        /// Converts a measure to a millisecond. (C: `MEASURE_TO_MSEC`)
        fn measure_to_msec(self, measure: float) -> float {
            measure * 240000.0 / *self
        }

        /// Converts a millisecond to a measure. (C: `MSEC_TO_MEASURE`)
        fn msec_to_measure(self, msec: float) -> float {
            msec * *self / 240000.0
        }
    }

    /// A duration from the particular point. It may be specified in measures
    /// or seconds. Used in the `Stop` object.
    #[deriving(Eq)]
    pub enum Duration { Seconds(float), Measures(float) }

    /// A damage value upon the MISS grade. Normally it is specified in
    /// percents of the full gauge (as in `MAXGAUGE`), but sometimes it may
    /// cause an instant death. Used in the `Bomb` object (normal note objects
    /// have a fixed value).
    #[deriving(Eq)]
    pub enum Damage { GaugeDamage(float), InstantDeath }

    //------------------------------------------------------------------------
    // object

    /// A data for objects (or object-like effects). Does not include the time
    /// information.
    #[deriving(Eq)]
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
        /**
         * Sets the virtual BGA layer to given image. The layer itself may
         * not be displayed depending on the current game status.
         * (C: `BGA_CHANNEL`)
         *
         * If the reference points to a movie, the movie starts playing; if
         * the other layer had the same movie started, it rewinds to
         * the beginning. The resulting image from the movie can be shared
         * among multiple layers.
         */
        SetBGA(BGALayer, Option<ImageRef>),
        /// Sets the BPM. Negative BPM causes the chart scrolls backwards
        /// (and implicitly signals the end of the chart). (C: `BPM_CHANNEL`)
        SetBPM(BPM),
        /// Stops the scroll of the chart for given duration ("scroll
        /// stopper" hereafter). (C: `STOP_CHANNEL`)
        Stop(Duration)
    }

    /// Query operations for objects.
    pub trait ObjQueryOps {
        /// Returns true if the object is a visible object (`Visible`).
        /// (C: `obj->type == NOTE`)
        pub fn is_visible(self) -> bool;
        /// Returns true if the object is an invisible object (`Invisible`).
        /// (C: `obj->type == INVNOTE`)
        pub fn is_invisible(self) -> bool;
        /// Returns true if the object is a start of LN object (`LNStart`).
        /// (C: `obj->type == LNSTART`)
        pub fn is_lnstart(self) -> bool;
        /// Returns true if the object is an end of LN object (`LNEnd`).
        /// (C: `obj->type == LNDONE`)
        pub fn is_lndone(self) -> bool;
        /// Returns true if the object is either a start or an end of LN
        /// object. (C: `obj->type < NOTE`)
        pub fn is_ln(self) -> bool;
        /// Returns true if the object is a bomb (`Bomb`).
        /// (C: `obj->type == BOMB`)
        pub fn is_bomb(self) -> bool;
        /// Returns true if the object is soundable when it is the closest
        /// soundable object from the current position and the player pressed
        /// the key. Named "soundable" since it may choose not to play
        /// the associated sound. Note that not every object with sound is
        /// soundable. (C: `obj->type <= INVNOTE`)
        pub fn is_soundable(self) -> bool;
        /// Returns true if the object is subject to grading.
        /// (C: `obj->type < INVNOTE`)
        pub fn is_gradable(self) -> bool;
        /// Returns true if the object has a visible representation.
        /// (C: `obj->type != INVNOTE`)
        pub fn is_renderable(self) -> bool;
        /// Returns true if the data is an object.
        /// (C: `IS_NOTE_CHANNEL(obj->chan)`)
        pub fn is_object(self) -> bool;
        /// Returns true if the data is a BGM. (C: `obj->chan == BGM_CHANNEL`)
        pub fn is_bgm(self) -> bool;
        /// Returns true if the data is a BGA. (C: `obj->chan == BGA_CHANNEL`)
        pub fn is_setbga(self) -> bool;
        /// Returns true if the data is a BPM change.
        /// (C: `obj->chan == BPM_CHANNEL`)
        pub fn is_setbpm(self) -> bool;
        /// Returns true if the data is a scroll stopper.
        /// (C: `obj->chan == STOP_CHANNEL`)
        pub fn is_stop(self) -> bool;

        /// Returns an associated lane if the data is an object.
        pub fn object_lane(self) -> Option<Lane>;
        /// Returns all sounds associated to the data.
        pub fn sounds(self) -> ~[SoundRef];
        pub fn keydown_sound(self) -> Option<SoundRef>;
        pub fn keyup_sound(self) -> Option<SoundRef>;
        pub fn through_sound(self) -> Option<SoundRef>;
        pub fn images(self) -> ~[ImageRef];
        pub fn through_damage(self) -> Option<Damage>;
    }

    /// Conversion operations for objects.
    pub trait ObjConvOps: ObjQueryOps {
        /// Returns a visible object with the same time, lane and sound as
        /// given object.
        pub fn to_visible(self) -> Self;
        /// Returns an invisible object with the same time, lane and sound as
        /// given object.
        pub fn to_invisible(self) -> Self;
        /// Returns a start of LN object with the same time, lane and sound as
        /// given object.
        pub fn to_lnstart(self) -> Self;
        /// Returns an end of LN object with the same time, lane and sound as
        /// given object.
        pub fn to_lndone(self) -> Self;
    }

    impl ObjQueryOps for ObjData {
        pub fn is_visible(self) -> bool {
            match self { Visible(*) => true, _ => false }
        }

        pub fn is_invisible(self) -> bool {
            match self { Invisible(*) => true, _ => false }
        }

        pub fn is_lnstart(self) -> bool {
            match self { LNStart(*) => true, _ => false }
        }

        pub fn is_lndone(self) -> bool {
            match self { LNDone(*) => true, _ => false }
        }

        pub fn is_ln(self) -> bool {
            match self { LNStart(*)|LNDone(*) => true, _ => false }
        }

        pub fn is_bomb(self) -> bool {
            match self { Bomb(*) => true, _ => false }
        }

        pub fn is_soundable(self) -> bool {
            match self {
                Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) => true,
                _ => false
            }
        }

        pub fn is_gradable(self) -> bool {
            match self {
                Visible(*) | LNStart(*) | LNDone(*) => true,
                _ => false
            }
        }

        pub fn is_renderable(self) -> bool {
            match self {
                Visible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true,
                _ => false
            }
        }

        pub fn is_object(self) -> bool {
            match self {
                Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*)
                    => true,
                _ => false
            }
        }

        pub fn is_bgm(self) -> bool {
            match self { BGM(*) => true, _ => false }
        }

        pub fn is_setbga(self) -> bool {
            match self { SetBGA(*) => true, _ => false }
        }

        pub fn is_setbpm(self) -> bool {
            match self { SetBPM(*) => true, _ => false }
        }

        pub fn is_stop(self) -> bool {
            match self { Stop(*) => true, _ => false }
        }

        pub fn object_lane(self) -> Option<Lane> {
            match self {
                Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
                LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
                _ => None
            }
        }

        pub fn sounds(self) -> ~[SoundRef] {
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

        pub fn keydown_sound(self) -> Option<SoundRef> {
            match self {
                Visible(_,sref) | Invisible(_,sref) | LNStart(_,sref) => sref,
                _ => None
            }
        }

        pub fn keyup_sound(self) -> Option<SoundRef> {
            match self {
                LNDone(_,sref) => sref,
                _ => None
            }
        }

        pub fn through_sound(self) -> Option<SoundRef> {
            match self {
                Bomb(_,sref,_) => sref,
                _ => None
            }
        }

        pub fn images(self) -> ~[ImageRef] {
            match self {
                SetBGA(_,Some(iref)) => ~[iref],
                _ => ~[]
            }
        }

        pub fn through_damage(self) -> Option<Damage> {
            match self {
                Bomb(_,_,damage) => Some(damage),
                _ => None
            }
        }
    }

    impl ObjConvOps for ObjData {
        pub fn to_visible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Visible(lane,snd),
                _ => fail!(~"to_visible for non-object")
            }
        }

        pub fn to_invisible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Invisible(lane,snd),
                _ => fail!(~"to_invisible for non-object")
            }
        }

        pub fn to_lnstart(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNStart(lane,snd),
                _ => fail!(~"to_lnstart for non-object")
            }
        }

        pub fn to_lndone(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNDone(lane,snd),
                _ => fail!(~"to_lndone for non-object")
            }
        }
    }

    /// Game play data associated to the time axis. It contains both objects
    /// (which are also associated to lanes) and object-like effects.
    #[deriving(Eq)]
    pub struct Obj {
        /// Time position in measures.
        time: float,
        /// Actual data.
        data: ObjData
    }

    pub impl Obj {
        /// Creates a `Visible` object.
        fn Visible(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            // Rust: `SoundRef` itself cannot be used as a function (#5315)
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Visible(lane, sref) }
        }

        /// Creates an `Invisible` object.
        fn Invisible(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Invisible(lane, sref) }
        }

        /// Creates a `LNStart` object.
        fn LNStart(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: LNStart(lane, sref) }
        }

        /// Creates a `LNDone` object.
        fn LNDone(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: LNDone(lane, sref) }
        }

        /// Creates a `Bomb` object.
        fn Bomb(time: float, lane: Lane, sref: Option<Key>,
                       damage: Damage) -> Obj {
            let sref = sref.map_consume(|s| SoundRef(s)); // XXX #5315
            Obj { time: time, data: Bomb(lane, sref, damage) }
        }

        /// Creates a `BGM` object.
        fn BGM(time: float, sref: Key) -> Obj {
            Obj { time: time, data: BGM(SoundRef(sref)) }
        }

        /// Creates a `SetBGA` object.
        fn SetBGA(time: float, layer: BGALayer, iref: Option<Key>) -> Obj {
            let iref = iref.map_consume(|i| ImageRef(i)); // XXX #5315
            Obj { time: time, data: SetBGA(layer, iref) }
        }

        /// Creates a `SetBPM` object.
        fn SetBPM(time: float, bpm: BPM) -> Obj {
            Obj { time: time, data: SetBPM(bpm) }
        }

        /// Creates a `Stop` object.
        fn Stop(time: float, duration: Duration) -> Obj {
            Obj { time: time, data: Stop(duration) }
        }

        fn measure(&self) -> int { self.time.floor() as int }
    }

    impl Ord for Obj {
        fn lt(&self, other: &Obj) -> bool { self.time < other.time }
        fn le(&self, other: &Obj) -> bool { self.time <= other.time }
        fn ge(&self, other: &Obj) -> bool { self.time >= other.time }
        fn gt(&self, other: &Obj) -> bool { self.time > other.time }
    }

    impl ObjQueryOps for Obj {
        pub fn is_visible(self) -> bool { self.data.is_visible() }
        pub fn is_invisible(self) -> bool { self.data.is_invisible() }
        pub fn is_lnstart(self) -> bool { self.data.is_lnstart() }
        pub fn is_lndone(self) -> bool { self.data.is_lndone() }
        pub fn is_ln(self) -> bool { self.data.is_ln() }
        pub fn is_bomb(self) -> bool { self.data.is_bomb() }
        pub fn is_soundable(self) -> bool { self.data.is_soundable() }
        pub fn is_gradable(self) -> bool { self.data.is_gradable() }
        pub fn is_renderable(self) -> bool { self.data.is_renderable() }
        pub fn is_object(self) -> bool { self.data.is_object() }
        pub fn is_bgm(self) -> bool { self.data.is_bgm() }
        pub fn is_setbga(self) -> bool { self.data.is_setbga() }
        pub fn is_setbpm(self) -> bool { self.data.is_setbpm() }
        pub fn is_stop(self) -> bool { self.data.is_stop() }

        pub fn object_lane(self) -> Option<Lane> {
            self.data.object_lane()
        }
        pub fn sounds(self) -> ~[SoundRef] {
            self.data.sounds()
        }
        pub fn keydown_sound(self) -> Option<SoundRef> {
            self.data.keydown_sound()
        }
        pub fn keyup_sound(self) -> Option<SoundRef> {
            self.data.keyup_sound()
        }
        pub fn through_sound(self) -> Option<SoundRef> {
            self.data.through_sound()
        }
        pub fn images(self) -> ~[ImageRef] {
            self.data.images()
        }
        pub fn through_damage(self) -> Option<Damage> {
            self.data.through_damage()
        }
    }

    impl ObjConvOps for Obj {
        pub fn to_visible(self) -> Obj {
            Obj { time: self.time, data: self.data.to_visible() }
        }
        pub fn to_invisible(self) -> Obj {
            Obj { time: self.time, data: self.data.to_invisible() }
        }
        pub fn to_lnstart(self) -> Obj {
            Obj { time: self.time, data: self.data.to_lnstart() }
        }
        pub fn to_lndone(self) -> Obj {
            Obj { time: self.time, data: self.data.to_lndone() }
        }
    }

    //------------------------------------------------------------------------
    // BMS data

    pub static DefaultBPM: BPM = BPM(130.0);

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

    pub static SinglePlay: int = 1;
    pub static CouplePlay: int = 2;
    pub static DoublePlay: int = 3;

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
        initbpm: BPM,
        /// Paths to sound file relative to `basepath` or BMS file.
        /// (C: `sndpath`)
        //
        // Rust: constant expression in the array size is unsupported.
        sndpath: [Option<~str>, ..MAXKEY],
        /// Paths to image/movie file relative to `basepath` or BMS file.
        /// (C: `imgpath`)
        imgpath: [Option<~str>, ..MAXKEY],
        /// List of blit commands to be executed after `imgpath` is loaded.
        /// (C: `blitcmd`)
        blitcmd: ~[BlitCmd],

        /// List of objects sorted by the position. (C: `objs`)
        objs: ~[Obj],
        /// The scaling factor of measures. Defaults to 1.0. (C: `shorten`)
        shorten: ~[float],
        /// The number of measures after the origin, i.e. the length of
        /// the BMS file. The play stops after the last measure. (C: `length`)
        nmeasures: uint
    }

    /// Creates a default value of BMS data.
    pub fn Bms() -> Bms {
        Bms { title: None, genre: None, artist: None, stagefile: None,
              basepath: None, player: SinglePlay, playlevel: 0, rank: 2,
              initbpm: DefaultBPM, sndpath: [None, ..MAXKEY],
              imgpath: [None, ..MAXKEY], blitcmd: ~[], objs: ~[],
              shorten: ~[], nmeasures: 0 }
    }

    pub impl Bms {
        fn shorten_factor(&self, measure: int) -> float {
            if measure < 0 || measure as uint >= self.shorten.len() {
                1.0
            } else {
                self.shorten[measure as uint]
            }
        }

        /// (C: `adjust_object_time`)
        fn adjust_object_time(&self, base: float, offset: float) -> float {
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
    }

    //------------------------------------------------------------------------
    // parsing

    /// Converts a single alphanumeric (base-36) letter to an integer.
    /// (C: `getdigit`)
    fn getdigit(n: char) -> Option<int> {
        match n {
            '0'..'9' => Some((n as int) - ('0' as int)),
            'a'..'z' => Some((n as int) - ('a' as int) + 10),
            'A'..'Z' => Some((n as int) - ('A' as int) + 10),
            _ => None
        }
    }

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    pub fn key2index(s: &[char]) -> Option<int> {
        if s.len() < 2 { return None; }
        do getdigit(s[0]).chain |a| {
            do getdigit(s[1]).map |&b| { a * 36 + b }
        }
    }

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    pub fn key2index_str(s: &str) -> Option<int> {
        if s.len() < 2 { return None; }
        let str::CharRange {ch:c1, next:p1} = str::char_range_at(s, 0);
        do getdigit(c1).chain |a| {
            let str::CharRange {ch:c2, next:p2} = str::char_range_at(s, p1);
            do getdigit(c2).map |&b| {
                assert!(p2 == 2); // both characters should be in ASCII
                a * 36 + b
            }
        }
    }

    /// Reads and parses the BMS file with given RNG from given reader.
    pub fn parse_bms_from_reader(f: @io::Reader, r: @rand::Rng)
                                    -> Result<Bms,~str> {
        // (C: `bmsheader`)
        static bmsheader: &'static [&'static str] = &[
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

        // Rust: #[deriving(Eq)] does not work inside the function. (#4913)
        impl Eq for RndState {
            fn eq(&self, other: &RndState) -> bool {
                match (*self, *other) {
                    (Process, Process) => true,
                    (Ignore, Ignore) => true,
                    (NoFurther, NoFurther) => true,
                    (_, _) => false
                }
            }
            fn ne(&self, other: &RndState) -> bool { !self.eq(other) }
        }
        impl Eq for Rnd {
            fn eq(&self, other: &Rnd) -> bool {
                // Rust: this is for using `ImmutableEqVector<T>::rposition`,
                //       which should have been in `ImmutableVector<T>`.
                self.val == other.val && self.inside == other.inside &&
                self.state == other.state && self.skip == other.skip
            }
            fn ne(&self, other: &Rnd) -> bool { !self.eq(other) }
        }

        let mut rnd = ~[Rnd { val: None, inside: false,
                              state: Process, skip: false }];

        struct BmsLine { measure: uint, chan: Key, data: ~str }
        impl Ord for BmsLine {
            fn lt(&self, other: &BmsLine) -> bool {
                self.measure < other.measure ||
                (self.measure == other.measure && self.chan < other.chan)
            }
            fn le(&self, other: &BmsLine) -> bool {
                self.measure < other.measure ||
                (self.measure == other.measure && self.chan <= other.chan)
            }
            fn ge(&self, other: &BmsLine) -> bool { !self.lt(other) }
            fn gt(&self, other: &BmsLine) -> bool { !self.le(other) }
        }

        // (C: `bmsline`)
        let mut bmsline = ~[];
        // (C: `bpmtab`)
        let mut bpmtab = ~[DefaultBPM, ..MAXKEY];
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
        for lines.each |&line0| {
            let line0 = ::util::str::from_fixed_utf8_bytes(line0,
                                                           |_| ~"\ufffd");
            let line: &str = line0;

            // skip non-command lines
            let line = line.trim_left();
            if !line.starts_with(~"#") { loop; }
            let line = line.slice_to_end(1);

            // search for header prefix. the header list (`bmsheader`) is
            // in the decreasing order of prefix length.
            let mut prefix = "";
            for bmsheader.each |&header| {
                if line.len() >= header.len() &&
                   line.slice(0, header.len()).to_upper()
                        == header.to_owned() {
                    prefix = header;
                    break;
                }
            }
            let line = line.slice_to_end(prefix.len());

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
            assert!(!rnd.is_empty());
            let state = { if rnd.last().skip {Ignore}
                          else {rnd.last().state} }; // XXX #4666

            match (prefix, state) {
                // #TITLE|#GENRE|#ARTIST|#STAGEFILE|#PATH_WAV <string>
                ("TITLE", Process) => read!(string title),
                ("GENRE", Process) => read!(string genre),
                ("ARTIST", Process) => read!(string artist),
                ("STAGEFILE", Process) => read!(string stagefile),
                ("PATH_WAV", Process) => read!(string basepath),

                // #BPM <float> or #BPMxx <float>
                ("BPM", Process) => {
                    let mut key = Key(-1), bpm = 0.0;
                    if lex!(line; Key -> key, ws, float -> bpm) {
                        let Key(key) = key; bpmtab[key] = BPM(bpm);
                    } else if lex!(line; ws, float -> bpm) {
                        bms.initbpm = BPM(bpm);
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
                        let val = if val <= 0 {None} else {Some(val)};

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
                        let val = if val <= 0 {None} else {Some(val)};

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
                    last.state = if last.state == Ignore {Process}
                                 else {NoFurther};
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

                (_, _) => {}
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
        let mut lastvis: [Option<uint>, ..NLANES] = [None, ..NLANES];

        // Indices to last LN start or end inserted (and not finalized yet)
        // per channels. If `consecutiveln` is on (#LNTYPE 2), the position
        // of referenced object gets updated during parsing; if off (#LNTYPE
        // 1), it is solely used for checking if we are inside the LN or not.
        // (C: `prev56`)
        let mut lastln: [Option<uint>, ..NLANES] = [None, ..NLANES];

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
                // TODO bpmtab validity check
                8 => add(Obj::SetBPM(t, bpmtab[*v])),

                // channel #09: scroll stopper defined by #STOPxx
                // TODO stoptab validity check
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
                            assert!(bms.objs[pos].is_visible());
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
                            assert!(bms.objs[pos].is_lndone());
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
                _ => {}
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
                let data = str::to_chars(line.data);
                let max = data.len() / 2 * 2;
                let count = max as float;
                for uint::range_step(0, max, 2) |i| {
                    let v = key2index(data.slice(i, i+2));
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
        bms.nmeasures = bmsline.last_opt().map_default(0, |l| l.measure) + 1;
        let endt = bms.nmeasures as float;
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
            parse_bms_from_reader(f, r)
        }
    }

    //------------------------------------------------------------------------
    // key specification

    pub struct KeySpec {
        /// The number of lanes on the left side. This number is significant
        /// only when Couple Play is used. (C: `nleftkeys`)
        split: uint,
        /// The order of significant lanes. The first `nleftkeys` lanes go to
        /// the left side and the remaining lanes (C: `nrightkeys`) go to
        /// the right side. (C: `keyorder`)
        order: ~[Lane],
        /// The type of lanes. (C: `keykind`)
        kinds: ~[Option<KeyKind>]
    }

    pub impl KeySpec {
        /// Returns a number of lanes that count towards "keys". Notably
        /// scratches and pedals do not count as keys. (C: `nkeys`)
        fn nkeys(&self) -> uint {
            let mut nkeys = 0;
            for self.kinds.each_some |kind| {
                if kind.counts_as_key() { nkeys += 1; }
            }
            nkeys
        }

        fn left_lanes(&self) -> &'self [Lane] {
            assert!(self.split <= self.order.len());
            self.order.slice(0, self.split)
        }

        fn right_lanes(&self) -> &'self [Lane] {
            assert!(self.split <= self.order.len());
            self.order.slice(self.split, self.order.len())
        }
    }

    /// (C: `parse_key_spec`)
    pub fn parse_key_spec(s: &str) -> Option<~[(Lane, KeyKind)]> {
        let mut specs = ~[];
        let mut s = s.trim_left().to_owned();
        while !s.is_empty() {
            let mut chan = Key(0), kind = '\x00', s2 = ~"";
            if !lex!(s; Key -> chan, char -> kind, ws*, str* -> s2, !) {
                return None;
            }
            s = s2;
            match (chan, KeyKind::from_char(kind)) {
                (Key(1*36..3*36-1), Some(kind)) =>
                    specs.push((Lane(*chan as uint - 1*36), kind)),
                (_, _) => return None
            }
        }
        Some(specs)
    }

    /// (C: `presets`)
    static PRESETS: &'static [(&'static str, &'static str, &'static str)] = &[
        ("5",     "16s 11a 12b 13a 14b 15a", ""),
        ("10",    "16s 11a 12b 13a 14b 15a",
                  "21a 22b 23a 24b 25a 26s"),
        ("5/fp",  "16s 11a 12b 13a 14b 15a 17p", ""),
        ("10/fp", "16s 11a 12b 13a 14b 15a 17p",
                  "27p 21a 22b 23a 24b 25a 26s"),
        ("7",     "16s 11a 12b 13a 14b 15a 18b 19a", ""),
        ("14",    "16s 11a 12b 13a 14b 15a 18b 19a",
                  "21a 22b 23a 24b 25a 28b 29a 26s"),
        ("7/fp",  "16s 11a 12b 13a 14b 15a 18b 19a 17p", ""),
        ("14/fp", "16s 11a 12b 13a 14b 15a 18b 19a 17p",
                  "27p 21a 22b 23a 24b 25a 28b 29a 26s"),
        ("9",     "11q 12w 13e 14r 15t 22r 23e 24w 25q", ""),
        ("9-bme", "11q 12w 13e 14r 15t 18r 19e 16w 17q", ""),
    ];

    /// (C: `detect_preset`)
    pub fn preset_to_key_spec(bms: &Bms, preset: Option<~str>)
                                    -> Option<(~str, ~str)> {
        let mut present = [false, ..NLANES];
        for bms.objs.each |&obj| {
            for obj.object_lane().each |&Lane(lane)| {
                present[lane] = true;
            }
        }

        let preset =
            match preset.map(|s| s.to_lower()) {
                None | Some(~"bms") | Some(~"bme") | Some(~"bml") => {
                    let isbme = (present[8] || present[9] ||
                                 present[36+8] || present[36+9]);
                    let haspedal = (present[7] || present[36+7]);
                    let nkeys = match bms.player {
                        2 | 3 => if isbme {~"14"} else {~"10"},
                        _     => if isbme {~"7" } else {~"5" }
                    };
                    if haspedal {nkeys + ~"/fp"} else {nkeys}
                },
                Some(~"pms") => {
                    let isbme = (present[6] || present[7] ||
                                 present[8] || present[9]);
                    if isbme {~"9-bme"} else {~"9"}
                },
                Some(preset) => preset
            };

        for PRESETS.each |&(name, leftkeys, rightkeys)| {
            if name == preset {
                return Some((leftkeys.to_owned(), rightkeys.to_owned()));
            }
        }
        None
    }

    //------------------------------------------------------------------------
    // post-processing

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

        fn sanitize(objs: &mut [Obj], to_type: &fn(&Obj) -> Option<uint>,
                    merge_types: &fn(uint) -> uint) {
            let len = objs.len();
            let mut i = 0;
            while i < len {
                let cur = objs[i].time;
                let mut types = 0;
                let mut j = i;
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

            static LNDONE: uint = 0;
            static LNSTART: uint = 1;
            static VISIBLE: uint = 2;
            static INVISIBLE: uint = 3;
            static BOMB: uint = 4;
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
                static LNMASK: uint = (1 << LNSTART) | (1 << LNDONE);

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
                    _ => {}
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

    /// Removes insignificant objects (i.e. not in visible lanes) and ensures
    /// that there is no `Deleted` object. (C: `analyze_and_compact_bms`)
    pub fn compact_bms(bms: &mut Bms, keyspec: &KeySpec) {
        for vec::each_mut(bms.objs) |obj| {
            for obj.object_lane().each |&Lane(lane)| {
                if keyspec.kinds[lane].is_none() {
                    remove_or_replace_note(obj)
                }
            }
        }

        do bms.objs.retain |&obj| { obj.data != Deleted }
    }

    //------------------------------------------------------------------------
    // analysis

    /// Derived BMS information. Again, this is not a global state.
    pub struct BmsInfo {
        /// The start position of the BMS file. This is either -1.0 or 0.0
        /// depending on the first measure has any visible objects or not.
        /// (C: `originoffset`)
        originoffset: float,
        /// Set to true if the BMS file has a BPM change. (C: `hasbpmchange`)
        hasbpmchange: bool,
        /// Set to true if the BMS file has long note objects.
        /// (C: `haslongnote`)
        haslongnote: bool,
        /// The number of visible objects in the BMS file. A long note object
        /// counts as one object. (C: `nnotes`)
        nnotes: int,
        /// The maximum possible score. (C: `maxscore`)
        maxscore: int
    }

    /// Analyzes the loaded BMS file. (C: `analyze_and_compact_bms`)
    pub fn analyze_bms(bms: &Bms) -> BmsInfo {
        let mut infos = BmsInfo { originoffset: 0.0, hasbpmchange: false,
                                  haslongnote: false, nnotes: 0,
                                  maxscore: 0 };

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

    /// (C: `get_bms_duration`)
    pub fn bms_duration(bms: &Bms, originoffset: float,
                        sound_length: &fn(SoundRef) -> float) -> float {
        let mut pos = originoffset;
        let mut bpm = bms.initbpm;
        let mut time = 0.0, sndtime = 0.0;

        for bms.objs.each |&obj| {
            let delta = bms.adjust_object_position(pos, obj.time);
            time += bpm.measure_to_msec(delta);
            match obj.data {
                Visible(_,Some(sref)) | LNStart(_,Some(sref)) | BGM(sref) => {
                    sndtime = cmp::max(sndtime, time + sound_length(sref));
                }
                SetBPM(BPM(newbpm)) => {
                    if newbpm > 0.0 {
                        bpm = BPM(newbpm);
                    } else if newbpm < 0.0 {
                        bpm = BPM(newbpm);
                        let delta =
                            bms.adjust_object_position(originoffset, pos);
                        time += BPM(-newbpm).measure_to_msec(delta);
                        break;
                    }
                }
                Stop(Seconds(secs)) => {
                    time += secs * 1000.0;
                }
                Stop(Measures(measures)) => {
                    time += bpm.measure_to_msec(measures);
                }
                _ => {}
            }
            pos = obj.time;
        }

        if *bpm > 0.0 {
            let delta = bms.adjust_object_position(pos,
                                                   bms.nmeasures as float);
            time += bpm.measure_to_msec(delta);
        }
        cmp::max(time, sndtime)
     }

    //------------------------------------------------------------------------
    // modifiers

    fn update_object_lane(obj: &mut Obj, f: &fn(Lane) -> Lane) {
        obj.data = match obj.data {
            Visible(lane,sref) => Visible(f(lane),sref),
            Invisible(lane,sref) => Invisible(f(lane),sref),
            LNStart(lane,sref) => LNStart(f(lane),sref),
            LNDone(lane,sref) => LNDone(f(lane),sref),
            Bomb(lane,sref,damage) => Bomb(f(lane),sref,damage),
            objdata => objdata
        };
    }

    /// (C: `shuffle_bms` with `MIRROR_MODF`)
    pub fn apply_mirror_modf(bms: &mut Bms, lanes: &[Lane]) {
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        let rlanes = vec::reversed(lanes);
        for vec::zip_slice(lanes, rlanes).each |&(Lane(from), to)| {
            map[from] = to;
        }

        for vec::each_mut(bms.objs) |obj| {
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    /// (C: `shuffle_bms` with `SHUFFLE_MODF`/`SHUFFLEEX_MODF`)
    pub fn apply_shuffle_modf(bms: &mut Bms, r: @rand::Rng, lanes: &[Lane]) {
        let shuffled = r.shuffle(lanes);
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        for vec::zip_slice(lanes, shuffled).each |&(Lane(from), to)| {
            map[from] = to;
        }

        for vec::each_mut(bms.objs) |obj| {
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    /// (C: `shuffle_bms` with `RANDOM_MODF`/`RANDOMEX_MODF`)
    pub fn apply_random_modf(bms: &mut Bms, r: @rand::Rng, lanes: &[Lane]) {
        let mut movable = vec::from_slice(lanes);
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));

        let mut lasttime = float::neg_infinity;
        for vec::each_mut(bms.objs) |obj| {
            if obj.is_lnstart() {
                let lane = obj.object_lane().get();
                match movable.position_elem(&lane) {
                    Some(i) => { movable.swap_remove(i); }
                    None => fail!(~"non-sanitized BMS detected")
                }
            }
            if lasttime < obj.time { // reshuffle required
                lasttime = obj.time + 1e-4;
                let shuffled = r.shuffle(movable);
                for vec::zip_slice(movable,
                                   shuffled).each |&(Lane(from), to)| {
                    map[from] = to;
                }
            }
            if obj.is_lnstart() {
                let lane = obj.object_lane().get();
                movable.push(lane);
            }
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    //------------------------------------------------------------------------

}

//============================================================================
// graphics

pub mod gfx {
    use sdl::Rect;
    pub use sdl::video::*;

    //------------------------------------------------------------------------
    // `Rect` additions

    /// A trait that can be translated to point coordinates (`x` and `y`
    /// fields in `sdl::Rect`, hence the name). Also contains `()`.
    pub trait XyOpt {
        /// Returns point coordinates if any.
        fn xy_opt(&self) -> Option<(i16,i16)>;
    }

    /// Same as `XyOpt` but does not contain `()`.
    pub trait Xy: XyOpt {
        /// Returns point coordinates.
        fn xy(&self) -> (i16,i16);
    }

    /// A trait that can be translated to a rectangular area (`w` and `h`
    /// fields in `sdl::Rect`, hence the name). Also contains `()`.
    pub trait WhOpt {
        /// Returns a rectangular area if any.
        fn wh_opt(&self) -> Option<(u16,u16)>;
    }

    /// Same as `WhOpt` but does not contain `()`.
    pub trait Wh {
        /// Returns a rectangular area.
        fn wh(&self) -> (u16,u16);
    }

    impl XyOpt for () {
        #[inline(always)]
        fn xy_opt(&self) -> Option<(i16,i16)> { None }
    }

    // Rust: we can't define these with `impl<T:Xy> XyOpt for T` due to
    //       the ambiguity.
    impl XyOpt for Rect {
        #[inline(always)]
        fn xy_opt(&self) -> Option<(i16,i16)> { Some((self.x, self.y)) }
    }

    impl<'self,T:XyOpt> XyOpt for &'self T {
        #[inline(always)]
        fn xy_opt(&self) -> Option<(i16,i16)> { (*self).xy_opt() }
    }

    impl Xy for Rect {
        #[inline(always)]
        fn xy(&self) -> (i16,i16) { (self.x, self.y) }
    }

    impl<'self,T:Xy> Xy for &'self T {
        #[inline(always)]
        fn xy(&self) -> (i16,i16) { (*self).xy() }
    }

    impl WhOpt for () {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> { None }
    }

    impl WhOpt for Rect {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> { Some((self.w, self.h)) }
    }

    impl WhOpt for Surface {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> { Some(self.get_size()) }
    }

    impl<'self,T:WhOpt> WhOpt for &'self T {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> { (*self).wh_opt() }
    }

    impl Wh for Rect {
        #[inline(always)]
        fn wh(&self) -> (u16,u16) { (self.w, self.h) }
    }

    impl Wh for Surface {
        #[inline(always)]
        fn wh(&self) -> (u16,u16) { self.get_size() }
    }

    impl<'self,T:Wh> Wh for &'self T {
        #[inline(always)]
        fn wh(&self) -> (u16,u16) { (*self).wh() }
    }

    /// A helper trait for defining every implementations for types `(T1,T2)`
    /// where `T1` and `T2` is convertible to an integer.
    trait ToInt16 {
        /// Converts to `i16`.
        fn to_i16(&self) -> i16;
        /// Converts to `u16`.
        fn to_u16(&self) -> u16;
    }

    macro_rules! define_ToInt16(
        ($t:ty) => (impl ToInt16 for $t {
                        #[inline(always)]
                        fn to_i16(&self) -> i16 { *self as i16 }
                        #[inline(always)]
                        fn to_u16(&self) -> u16 { *self as u16 }
                    })
    )

    define_ToInt16!(int)
    define_ToInt16!(uint)
    define_ToInt16!(i8)
    define_ToInt16!(i16)
    define_ToInt16!(i32)
    define_ToInt16!(i64)
    define_ToInt16!(u8)
    define_ToInt16!(u16)
    define_ToInt16!(u32)
    define_ToInt16!(u64)

    impl<X:ToInt16+Copy,Y:ToInt16+Copy> XyOpt for (X,Y) {
        #[inline(always)]
        fn xy_opt(&self) -> Option<(i16,i16)> {
            let (x, y) = *self; Some((x.to_i16(), y.to_i16()))
        }
    }

    impl<X:ToInt16+Copy,Y:ToInt16+Copy> Xy for (X,Y) {
        #[inline(always)]
        fn xy(&self) -> (i16,i16) {
            let (x, y) = *self; (x.to_i16(), y.to_i16())
        }
    }

    impl<W:ToInt16+Copy,H:ToInt16+Copy> WhOpt for (W,H) {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> {
            let (w, h) = *self; Some((w.to_u16(), h.to_u16()))
        }
    }

    impl<W:ToInt16+Copy,H:ToInt16+Copy> Wh for (W,H) {
        #[inline(always)]
        fn wh(&self) -> (u16,u16) {
            let (w, h) = *self; (w.to_u16(), h.to_u16())
        }
    }

    /// Constructs an `sdl::Rect` from given point coordinates. Fills `w` and
    /// `h` fields to 0 as expected by the second `sdl::Rect` argument from
    /// `SDL_BlitSurface`.
    fn rect_from_xy<XY:Xy>(xy: XY) -> Rect {
        let (x, y) = xy.xy();
        Rect { x: x, y: y, w: 0, h: 0 }
    }

    /// Constructs an `sdl::Rect` from given point coordinates and optional
    /// rectangular area. `rect_from_xywh(xy, ())` equals to
    /// `rect_from_xy(xy)`.
    fn rect_from_xywh<XY:Xy,WH:WhOpt>(xy: XY, wh: WH) -> Rect {
        let (x, y) = xy.xy();
        let (w, h) = wh.wh_opt().get_or_default((0, 0));
        Rect { x: x, y: y, w: w, h: h }
    }

    /// Additions to `sdl::video::Surface`. They replace their `_rect`
    /// suffixed counterparts, which are generally annoying to work with.
    pub trait SurfaceAreaUtil {
        /// An alternative interface to `set_clip_rect`.
        fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH);
        /// An alternative interface to `blit_rect`.
        fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool;
        /// An alternative interface to `fill_rect`.
        fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH,
                                     color: Color) -> bool;
    }

    impl SurfaceAreaUtil for Surface {
        fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH) {
            let rect = rect_from_xywh(xy, wh);
            self.set_clip_rect(&rect)
        }

        fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool {
            let srcrect = rect_from_xywh(srcxy, wh);
            let dstrect =
                dstxy.xy_opt().map(|&xy| rect_from_xywh(xy, &srcrect));
            self.blit_rect(src, Some(srcrect), dstrect)
        }

        fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH,
                                     color: Color) -> bool {
            let rect = rect_from_xywh(xy, wh);
            self.fill_rect(Some(rect), color)
        }
    }

    //------------------------------------------------------------------------
    // color

    /// Extracts a red, green, blue components from given color.
    fn to_rgb(c: Color) -> (u8, u8, u8) {
        match c { RGB(r, g, b) | RGBA(r, g, b, _) => (r, g, b) }
    }

    /// Linear color gradient.
    pub struct Gradient {
        /// A color at the position 1.0 (!). This is because the gradient was
        /// historically intended for lane backgrounds only.
        top: Color,
        /// A color at the position 0.0.
        bottom: Color
    }

    /// Creates a new color gradient.
    pub fn Gradient(top: Color, bottom: Color) -> Gradient {
        Gradient { top: top, bottom: bottom }
    }

    /// A trait for color or color gradient. The color at the particular
    /// position can be calculated with `blend` method.
    // Rust: `Copy` can't be inherited even when it's specified. (#3984)
    pub trait Blend {
        /// Calculates the color at the position `num/denom`. (C: `blend`)
        fn blend(&self, num: int, denom: int) -> Color;
    }

    impl Blend for Color {
        fn blend(&self, _num: int, _denom: int) -> Color { *self }
    }

    impl Blend for Gradient {
        fn blend(&self, num: int, denom: int) -> Color {
            fn mix(x: u8, y: u8, num: int, denom: int) -> u8 {
                let x = x as int, y = y as int;
                (y + ((x - y) * num / denom)) as u8
            }

            let (r1, g1, b1) = to_rgb(self.top);
            let (r2, g2, b2) = to_rgb(self.bottom);
            RGB(mix(r1, r2, num, denom), mix(g1, g2, num, denom),
                mix(b1, b2, num, denom))
        }
    }

    //------------------------------------------------------------------------
    // surface utilities

    /// Creates a new RAM-backed surface. By design, Angolmois does not use
    /// a VRAM-backed surface except for the screen. (C: `newsurface`)
    pub fn new_surface(w: uint, h: uint) -> ~Surface {
        match Surface::new([SWSurface], w as int, h as int, 32,
                           0xff0000, 0xff00, 0xff, 0) {
            Ok(surface) => surface,
            Err(err) => die!("new_surface failed: %s", err)
        }
    }

    /// A proxy to `sdl::video::Surface` for the direct access to pixels.
    /// For now, it is for 32 bits per pixel only.
    pub struct SurfacePixels<'self> {
        fmt: *ll::SDL_PixelFormat,
        width: uint,
        height: uint,
        pitch: uint,
        pixels: &'self mut [u32]
    }

    /// A trait for the direct access to pixels.
    pub trait SurfacePixelsUtil {
        /// Grants the direct access to pixels. Also locks the surface
        /// as needed.
        fn with_pixels<R>(&self, f: &fn(pixels: &SurfacePixels) -> R) -> R;
    }

    impl SurfacePixelsUtil for Surface {
        fn with_pixels<R>(&self, f: &fn(pixels: &SurfacePixels) -> R) -> R {
            do self.with_lock |pixels| {
                let fmt = unsafe {(*self.raw).format};
                let pitch = unsafe {(*self.raw).pitch / 4 as uint};
                let pixels = unsafe {cast::transmute(pixels)};
                f(&SurfacePixels { fmt: fmt,
                                   width: self.get_width() as uint,
                                   height: self.get_height() as uint,
                                   pitch: pitch, pixels: pixels })
            }
        }
    }

    impl<'self> SurfacePixels<'self> {
        /// Returns a pixel at given position. (C: `getpixel`)
        pub fn get_pixel(&self, x: uint, y: uint) -> Color {
            Color::from_mapped(self.pixels[x + y * self.pitch], self.fmt)
        }

        /// Sets a pixel to given position. (C: `putpixel`)
        pub fn put_pixel(&self, x: uint, y: uint, c: Color) {
            self.pixels[x + y * self.pitch] = c.to_mapped(self.fmt);
        }

        /// Sets or blends (if `c` is `RGBA`) a pixel to given position.
        /// (C: `putblendedpixel`)
        pub fn put_blended_pixel(&self, x: uint, y: uint, c: Color) {
            match c {
                RGB(*) => self.put_pixel(x, y, c),
                RGBA(r,g,b,a) => match self.get_pixel(x, y) {
                    RGB(r2,g2,b2) | RGBA(r2,g2,b2,_) => {
                        let grad = Gradient(RGB(r2,g2,b2), RGB(r,g,b));
                        self.put_pixel(x, y, grad.blend(a as int, 255));
                    }
                }
            }
        }
    }

    static FP_SHIFT1: int = 11;
    static FP_SHIFT2: int = 16;

    /// (C: `bicubic_kernel`)
    fn bicubic_kernel(x: int, y: int) -> int {
        let x = num::abs(x);
        if x < y {
            ((y*y - 2*x*x + x*x/y*x) << FP_SHIFT1) / (y*y)
        } else if x < y * 2 {
            ((4*y*y - 8*x*y + 5*x*x - x*x/y*x) << FP_SHIFT1) / (y*y)
        } else {
            0
        }
    }

    /// (C: `bicubic_interpolation`)
    pub fn bicubic_interpolation(src: &SurfacePixels, dest: &SurfacePixels) {
        let w = dest.width as int - 1;
        let h = dest.height as int - 1;
        let ww = src.width as int - 1;
        let hh = src.height as int - 1;

        let mut dx = 0, x = 0;
        for int::range(0, w + 1) |i| {
            let mut dy = 0, y = 0;
            for int::range(0, h + 1) |j| {
                let mut r = 0, g = 0, b = 0;
                let a0 = [bicubic_kernel((x-1) * w - i * ww, w),
                          bicubic_kernel( x    * w - i * ww, w),
                          bicubic_kernel((x+1) * w - i * ww, w),
                          bicubic_kernel((x+2) * w - i * ww, w)];
                let a1 = [bicubic_kernel((y-1) * h - j * hh, h),
                          bicubic_kernel( y    * h - j * hh, h),
                          bicubic_kernel((y+1) * h - j * hh, h),
                          bicubic_kernel((y+2) * h - j * hh, h)];
                for int::range(0, 4) |k0| {
                    for int::range(0, 4) |k1| {
                        let xx = x + k0 - 1, yy = y + k1 - 1;
                        if 0 <= xx && xx <= ww && 0 <= yy && yy <= hh {
                            let (r2,g2,b2) =
                                to_rgb(src.get_pixel(xx as uint, yy as uint));
                            let d = (a0[k0] * a1[k1]) >> (FP_SHIFT1*2 -
                                                          FP_SHIFT2);
                            r += r2 as int * d;
                            g += g2 as int * d;
                            b += b2 as int * d;
                        }
                    }
                }

                let r = ::util::cmp::clamp(0, r >> FP_SHIFT2, 255) as u8;
                let g = ::util::cmp::clamp(0, g >> FP_SHIFT2, 255) as u8;
                let b = ::util::cmp::clamp(0, b >> FP_SHIFT2, 255) as u8;
                dest.put_pixel(i as uint, j as uint, RGB(r, g, b));

                dy += hh;
                if dy > h {
                    y += 1;
                    dy -= h;
                }
            }

            dx += ww;
            if dx > w {
                x += 1;
                dx -= w;
            }
        }
    }

    //------------------------------------------------------------------------
    // bitmap font

    /// Bit vector which represents one row of zoomed font.
    type ZoomedFontRow = u32;

    /// 8x16 resizable bitmap font.
    pub struct Font {
        /**
         * Font data used for zoomed font reconstruction. This is actually
         * an array of `u32` elements, where the first `u16` element forms
         * upper 16 bits and the second forms lower 16 bits. It is
         * reinterpreted for better compression. (C: `fontdata`)
         *
         * One glyph has 16 `u32` elements for each row from the top to
         * the bottom. One `u32` element contains eight four-bit groups for
         * each column from the left (lowermost group) to the right
         * (uppermost group). Each group is a bitwise OR of following bits:
         *
         * - 1: the lower right triangle of the zoomed pixel should be drawn.
         * - 2: the lower left triangle of the zoomed pixel should be drawn.
         * - 4: the upper left triangle of the zoomed pixel should be drawn.
         * - 8: the upper right triangle of the zoomed pixel should be drawn.
         *
         * So for example, if the group bits read 3 (1+2), the zoomed pixel
         * would be drawn as follows (in the zoom factor 5):
         *
         *     .....
         *     #...#
         *     ##.##
         *     #####
         *     #####
         *
         * The group bits 15 (1+2+4+8) always draw the whole square, so in
         * the zoom factor 1 only pixels with group bits 15 will be drawn.
         */
        glyphs: ~[u16],

        /// Precalculated zoomed font per zoom factor. It is three-dimensional
        /// array which indices are zoom factor, glyph number and row
        /// respectively. Assumes that each element has at least zoom factor
        /// times 8 (columns per row) bits. (C: `zoomfont`)
        pixels: ~[~[~[ZoomedFontRow]]]
    }

    pub enum Alignment { LeftAligned, Centered, RightAligned }

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
        // (C: `indices`)
        let indices =
            ~"!!7a/&/&s$7a!f!'M*Q*Qc$(O&J!!&J&Jc(e!2Q2Qc$-Bg2m!2bB[Q7Q2[e&2Q!\
              Qi>&!&!>UT2T2&2>WT!c*T2GWc8icM2U2D!.8(M$UQCQ-jab!'U*2*2*2TXbZ25\
              2>9ZWk@*!*!*8(J$JlWi@cxQ!Q!d$#Q'O*?k@e2dfejcNl!&JTLTLG_&J>]c*&J\
              m@cB&J&J7[e(o>pJM$Qs<7[{Zj`Jm40!3!.8(M$U!C!-oR>UQ2U2]2a9Y[S[QCQ\
              2GWk@*M*Q*B*!*!g$aQs`G8.M(U$[!Ca[o@Q2Q!IJQ!Q!c,GWk@787M6U2C2d!a\
              [2!2k?!bnc32>[u`>Uc4d@b(q@abXU!D!.8(J&J&d$q`Q2IXu`g@Q2aWQ!q@!!k\
              tk,x@M$Qk@3!.8(M$U!H#W'O,?4m_f!7[i&n!:eX5ghCk=>UQ2Q2U2Dc>J!!&J&\
              b&k@J)LKg!GK!)7Wk@'8,M=UWCcfa[c&Q2l`f4If(Q2G[l@MSUQC!2!2c$Q:RWG\
              Ok@,[<2WfZQ2U2D2.l`a[eZ7f(!2b2|@b$j!>MSUQCc6[2W2Q:RWGOk@Q2Q2c$a\
              [g*Ql`7[&J&Jk$7[l`!Qi$d^GWk@U2D2.9([$[#['[,@<2W2k@!2!2m$a[l`:^[\
              a[a[T2Td~c$k@d2:R[V[a@_b|o@,M=UWCgZU:EW.Ok@>[g<G[!2!2d$k@Ug@Q2V\
              2a2IW_!Wt`Ih*q`!2>WQ!Q!c,Gk_!7[&J&Jm$k@gti$m`k:U:EW.O(?s@T2Tb$a\
              [CW2Qk@M+U:^[GbX,M>U`[WCO-l@'U,D<.W(O&J&Je$k@a[Q!U!]!G8.M(U$[!C\
              a[k@*Q!Q!l$b2m!+!:#W'O,?4!1n;c`*!*!l$h`'8,M=UWCO-pWz!a[i,#Q'O,?\
              4~R>QQ!Q!aUQ2Q2Q2aWl=2!2!2>[e<c$G[p`dZcHd@l`czi|c$al@i`b:[!2Un`\
              >8TJTJ&J7[&b&e$o`i~aWQ!c(hd2!2!2>[g@e$k]epi|e0i!bph(d$dbGWhA2!2\
              U2D2.9(['[,@<2W2k`*J*?*!*!k$o!;[a[T2T2c$c~o@>[c6i$p@Uk>GW}`G[!2\
              !2b$h!al`aWQ!Q!Qp`fVlZf@UWb6>eX:GWk<&J&J7[c&&JTJTb$G?o`c~i$m`k@\
              U:EW.O(v`T2Tb$a[Fp`M+eZ,M=UWCO-u`Q:RWGO.A(M$U!Ck@a[]!G8.M(U$[!C\
              a[i:78&J&Jc$%[g*7?e<g0w$cD#iVAg*$[g~dB]NaaPGft~!f!7[.W(O";

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
        assert!(glyphs.len() == 3072);
        Font { glyphs: glyphs, pixels: ~[] }
    }

    pub impl Font {
        /// (C: `fontprocess`)
        fn create_zoomed_font(&mut self, zoom: uint) {
            assert!(zoom > 0);
            assert!(zoom <= (8 * sys::size_of::<ZoomedFontRow>()) / 8);
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
        pub fn print_glyph<ColorT:Blend+Copy>(&self, // XXX #3984
                pixels: &SurfacePixels, x: uint, y: uint, zoom: uint,
                glyph: uint, color: ColorT) {
            assert!(!self.pixels[zoom].is_empty());
            for uint::range(0, 16 * zoom) |iy| {
                let row = self.pixels[zoom][glyph][iy];
                let rowcolor = color.blend(iy as int, 16 * zoom as int);
                for uint::range(0, 8 * zoom) |ix| {
                    if ((row >> ix) & 1) != 0 {
                        pixels.put_pixel(x + ix, y + iy, rowcolor);
                    }
                }
            }
        }

        /// (C: `printchar`)
        pub fn print_char<ColorT:Blend+Copy>(&self, // XXX #3984
                pixels: &SurfacePixels, x: uint, y: uint, zoom: uint,
                c: char, color: ColorT) {
            if !char::is_whitespace(c) {
                let c = c as uint;
                let glyph = if 32 <= c && c < 126 {c-32} else {0};
                self.print_glyph(pixels, x, y, zoom, glyph, color);
            }
        }

        /// (C: `printstr`)
        pub fn print_string<ColorT:Blend+Copy>(&self, // XXX #3984
                pixels: &SurfacePixels, x: uint, y: uint, zoom: uint,
                align: Alignment, s: &str, color: ColorT) {
            let mut x = match align {
                LeftAligned  => x,
                Centered     => x - s.char_len() * (8 * zoom) / 2,
                RightAligned => x - s.char_len() * (8 * zoom),
            };
            // Rust: `s.each_char` is ambiguous here.
            for str::each_char(s) |c| {
                let nextx = x + 8 * zoom;
                if nextx >= pixels.width { break; }
                self.print_char(pixels, x, y, zoom, c, color);
                x = nextx;
            }
        }
    }

    //------------------------------------------------------------------------

}

//============================================================================
// game play

pub mod player {
    use sdl::*;
    use sdl::video::*;
    use sdl::event::*;
    use sdl::mixer::*;
    use util::sdl::*;
    use parser::*;
    use gfx::*;

    /// The width of screen, unless the exclusive mode.
    static SCREENW: uint = 800;
    /// The height of screen, unless the exclusive mode.
    static SCREENH: uint = 600;
    /// The width of BGA, or the width of screen for the exclusive mode.
    static BGAW: uint = 256;
    /// The height of BGA, or the height of screen for the exclusive mode.
    static BGAH: uint = 256;

    //------------------------------------------------------------------------
    // options

    /// (C: `enum mode`)
    #[deriving(Eq)]
    pub enum Mode {
        /// (C: `PLAY_MODE`)
        PlayMode,
        /// (C: `AUTOPLAY_MODE`)
        AutoPlayMode,
        /// (C: `EXCLUSIVE_MODE`)
        ExclusiveMode
    }

    /// (C: `enum modf`)
    #[deriving(Eq)]
    pub enum Modf {
        /// (C: `MIRROR_MODF`)
        MirrorModf,
        /// (C: `SHUFFLE_MODF`)
        ShuffleModf,
        /// (C: `SHUFFLEEX_MODF`)
        ShuffleExModf,
        /// (C: `RANDOM_MODF`)
        RandomModf,
        /// (C: `RANDOMEX_MODF`)
        RandomExModf
    }

    /// (C: `enum bga`)
    #[deriving(Eq)]
    pub enum Bga {
        /// (C: `BGA_AND_MOVIE`)
        BgaAndMovie,
        /// (C: `BGA_BUT_NO_MOVIE`)
        BgaButNoMovie,
        /// (C: `NO_BGA`)
        NoBga
    }

    pub struct Options {
        /// (C: `bmspath`)
        bmspath: ~str,
        /// (C: `opt_mode`)
        mode: Mode,
        /// (C: `opt_modf`)
        modf: Option<Modf>,
        /// (C: `opt_bga`)
        bga: Bga,
        /// (C: `opt_showinfo`)
        showinfo: bool,
        /// (C: `opt_fullscreen`)
        fullscreen: bool,
        /// (C: `opt_joystick`)
        joystick: Option<uint>,
        /// (C: `preset`)
        preset: Option<~str>,
        /// (C: `leftkeys`)
        leftkeys: Option<~str>,
        /// (C: `rightkeys`)
        rightkeys: Option<~str>,
        /// (C: `playspeed`)
        playspeed: float,
    }

    pub impl Options {
        fn rel_path(&self, path: &str) -> Path {
            Path(self.bmspath).dir_path().push_rel(&Path(path))
        }

        /// (C: `opt_mode >= EXCLUSIVE_MODE`)
        fn is_exclusive(&self) -> bool { self.mode == ExclusiveMode }
        /// (C: `!!opt_mode`)
        fn is_autoplay(&self) -> bool { self.mode != PlayMode }
        /// (C: `opt_bga < NO_BGA`)
        fn has_bga(&self) -> bool { self.bga != NoBga }
        /// (C: `opt_bga < BGA_BUT_NO_MOVIE`)
        fn has_movie(&self) -> bool { self.bga == BgaAndMovie }
        /// (C: `opt_mode < EXCLUSIVE_MODE || opt_bga < NO_BGA`)
        fn has_screen(&self) -> bool {
            !self.is_exclusive() || self.has_bga()
        }
    }

    //------------------------------------------------------------------------
    // bms utilities

    fn key_spec(bms: &Bms, opts: &Options) -> Result<~KeySpec,~str> {
        let (leftkeys, rightkeys) =
            if opts.leftkeys.is_none() && opts.rightkeys.is_none() {
                let preset =
                    if opts.preset.is_none() && 
                           opts.bmspath.to_lower().ends_with(~".pms") {
                        Some(~"pms")
                    } else {
                        copy opts.preset
                    };
                match preset_to_key_spec(bms, preset) {
                    Some(leftright) => leftright,
                    None => return Err(fmt!("Invalid preset name: %s",
                                            opts.preset.map_default(~"",
                                                |&v| copy v)))
                }
            } else {
                // Rust: `Option` of managed pointer is not easy to use due to
                //       implicit move. `Option<T>::clone_default` maybe?
                (opts.leftkeys.map_default(~"", |&v| copy v),
                 opts.rightkeys.map_default(~"", |&v| copy v))
            };

        let mut keyspec = ~KeySpec { split: 0, order: ~[],
                                     kinds: ~[None, ..NLANES] };
        let parse_and_add = |keys: &str| -> Option<uint> {
            match parse_key_spec(keys) {
                None | Some([]) => None,
                Some(left) => {
                    for left.each |&(lane,kind)| {
                        if keyspec.kinds[*lane].is_some() { return None; }
                        keyspec.order.push(lane);
                        keyspec.kinds[*lane] = Some(kind);
                    }
                    Some(left.len())
                }
            }
        };

        if !leftkeys.is_empty() {
            match parse_and_add(leftkeys) {
                None =>
                    return Err(fmt!("Invalid key spec for left \
                                     hand side: %s", leftkeys)),
                Some(nkeys) => keyspec.split += nkeys
            }
        } else {
            return Err(fmt!("No key model is specified using -k or -K"));
        }
        if !rightkeys.is_empty() {
            match parse_and_add(rightkeys) {
                None =>
                    return Err(fmt!("Invalid key spec for right \
                                     hand side: %s", rightkeys)),
                Some(nkeys) => // no split panes except for #PLAYER 2
                    if bms.player != 2 { keyspec.split += nkeys; }
            }
        }
        Ok(keyspec)
    }

    /// (C: `shuffle_bms`)
    fn apply_modf(bms: &mut Bms, modf: Modf, r: @rand::Rng,
                   keyspec: &KeySpec, begin: uint, end: uint) {
        let mut lanes = ~[];
        for uint::range(begin, end) |i| {
            let lane = keyspec.order[i];
            let kind = keyspec.kinds[*lane];
            if modf == ShuffleExModf || modf == RandomExModf ||
                    kind.map_default(false, |&kind| kind.counts_as_key()) {
                lanes.push(lane);
            }
        }

        match modf {
            MirrorModf => apply_mirror_modf(bms, lanes),
            ShuffleModf | ShuffleExModf => apply_shuffle_modf(bms, r, lanes),
            RandomModf | RandomExModf => apply_random_modf(bms, r, lanes)
        }
    }

    //------------------------------------------------------------------------
    // utilities

    fn check_exit(atexit: &fn()) {
        loop {
            match poll_event() {
                KeyEvent(EscapeKey,_,_,_)|QuitEvent => {
                    atexit();
                    ::util::exit(0);
                },
                NoEvent => { break; },
                _ => {}
            }
        }
    }

    fn update_line(s: &str) {
        io::stderr().write_str(fmt!("\r%s\r%s", str::repeat(~" ", 72), s));
    }

    //------------------------------------------------------------------------
    // initialization

    /// (C: `init_video`)
    fn init_video(exclusive: bool, fullscreen: bool) -> ~Surface {
        let result =
            if exclusive {
                set_video_mode(BGAW as int, BGAH as int, 32,
                               [SWSurface], [DoubleBuf])
            } else if !fullscreen {
                set_video_mode(SCREENW as int, SCREENH as int, 32,
                               [SWSurface], [DoubleBuf])
            } else {
                set_video_mode(SCREENW as int, SCREENH as int, 32,
                               [], [Fullscreen])
            };
        let screen =
            match result {
                Ok(screen) => screen,
                Err(err) => die!("SDL Video Initialization Failure: %s", err)
            };
        if !exclusive {
            mouse::set_cursor_visible(false);
        }
        wm::set_caption(::version(), ~"");
        screen
    }

    /// (C: `init_ui`)
    fn init_sdl() {
        if !init([InitVideo, InitAudio, InitJoystick]) {
            die!("SDL Initialization Failure: %s", get_error());
        }
        // TODO opt_joystick
        img::init([img::InitJPG, img::InitPNG]);
        //mixer::init([mixer::InitOGG, mixer::InitMP3]); // TODO
        if mixer::open(44100, audio::S16AudioFormat,
                       audio::Stereo, 2048).is_err() {
            die!("SDL Mixer Initialization Failure");
        }
    }

    fn init_joystick(joyidx: uint) -> ~joy::Joystick {
        // TODO rust-sdl patch
        unsafe { joy::ll::SDL_JoystickEventState(1); }
        match joy::Joystick::open(joyidx as int) {
            Ok(joy) => joy,
            Err(err) => die!("SDL Joystick Initialization Failure: %s", err)
        }
    }

    //------------------------------------------------------------------------
    // virtual input

    /// Actual input. Mapped to zero or more virtual inputs by keymap.
    #[deriving(Eq)]
    enum Input {
        /// Keyboard input.
        KeyInput(event::Key),
        /// Joystick axis input.
        JoyAxisInput(uint),
        /// Joystick button input.
        JoyButtonInput(uint)
    }

    impl IterBytes for Input {
        fn iter_bytes(&self, lsb0: bool, f: ::core::to_bytes::Cb) {
            use core::to_bytes::iter_bytes_2;
            match *self {
                KeyInput(key) => iter_bytes_2(&0u8, &(key as uint), lsb0, f),
                JoyAxisInput(axis) => iter_bytes_2(&1u8, &axis, lsb0, f),
                JoyButtonInput(button) => iter_bytes_2(&2u8, &button, lsb0, f)
            }
        }
    }

    /// Virtual input.
    #[deriving(Eq)]
    enum VirtualInput {
        LaneInput(Lane),
        SpeedDownInput,
        SpeedUpInput
    }

    /**
     * State of virtual input elements. There are three states: neutral, and
     * positive or negative. There is no difference between positive and
     * negative states (the naming is arbitrary) except for that they are
     * distinct.
     *
     * The states should really be one of pressed (non-neutral) or unpressed
     * (neutral) states, but we need two non-neutral states since the actual
     * input device with continuous values (e.g. joystick axes) can trigger
     * the state transition *twice* without hitting the neutral state.
     * We solve this problem by making the transition from negative to
     * positive (and vice versa) temporarily hit the neutral state.
     */
    #[deriving(Eq)]
    enum InputState {
        /// Positive input state. Occurs when the button is pressed or
        /// the joystick axis is moved in the positive direction.
        Positive = 1,
        /// Neutral input state. Occurs when the button is not pressed or
        /// the joystick axis is moved back to the origin.
        Neutral = 0,
        /// Negative input state. Occurs when the joystick axis is moved
        /// in the negative direction.
        Negative = -1
    }

    pub impl VirtualInput {
        fn active_in_key_spec(&self, kind: KeyKind,
                              keyspec: &KeySpec) -> bool {
            match *self {
                LaneInput(Lane(lane)) => keyspec.kinds[lane] == Some(kind),
                SpeedDownInput|SpeedUpInput => true
            }
        }
    }

    /// An information about an environment variable for multiple keys.
    // Rust: static struct seems not working somehow... (#5688)
    /*
    struct KeySet {
        envvar: &'static str,
        default: &'static str,
        mapping: &'static [(Option<KeyKind>, &'static [VirtualInput])],
    }
    */
    type KeySet = (
        &'static str,
        &'static str,
        &'static [(Option<KeyKind>, &'static [VirtualInput])]);

    /// (C: `envvars`)
    static KEYSETS: &'static [KeySet] = &[
        (/*KeySet { envvar:*/ &"ANGOLMOIS_1P_KEYS",
                 /*default:*/ &"left shift%axis 3|z%button 3|s%button 6|\
                            x%button 2|d%button 7|c%button 1|f%button 4|\
                            v%axis 2|left alt",
                 /*mapping:*/ &[(Some(Scratch),   &[LaneInput(Lane(6))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(1))]),
                            (Some(BlackKey),  &[LaneInput(Lane(2))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(3))]),
                            (Some(BlackKey),  &[LaneInput(Lane(4))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(5))]),
                            (Some(BlackKey),  &[LaneInput(Lane(8))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(9))]),
                            (Some(FootPedal), &[LaneInput(Lane(7))])] /*}*/),
        (/*KeySet { envvar:*/ &"ANGOLMOIS_2P_KEYS",
                 /*default:*/ &"right alt|m|k|,|l|.|;|/|right shift",
                 /*mapping:*/ &[(Some(FootPedal), &[LaneInput(Lane(36+7))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(36+1))]),
                            (Some(BlackKey),  &[LaneInput(Lane(36+2))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(36+3))]),
                            (Some(BlackKey),  &[LaneInput(Lane(36+4))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(36+5))]),
                            (Some(BlackKey),  &[LaneInput(Lane(36+8))]),
                            (Some(WhiteKey),  &[LaneInput(Lane(36+9))]),
                            (Some(Scratch),   &[LaneInput(Lane(36+6))])] ),
        (/*KeySet { envvar:*/ &"ANGOLMOIS_PMS_KEYS",
                 /*default:*/ &"z|s|x|d|c|f|v|g|b",
                 /*mapping:*/ &[(Some(Button1), &[LaneInput(Lane(1))]),
                            (Some(Button2), &[LaneInput(Lane(2))]),
                            (Some(Button3), &[LaneInput(Lane(3))]),
                            (Some(Button4), &[LaneInput(Lane(4))]),
                            (Some(Button5), &[LaneInput(Lane(5))]),
                            (Some(Button4), &[LaneInput(Lane(8)),
                                              LaneInput(Lane(36+2))]),
                            (Some(Button3), &[LaneInput(Lane(9)),
                                              LaneInput(Lane(36+3))]),
                            (Some(Button2), &[LaneInput(Lane(6)),
                                              LaneInput(Lane(36+4))]),
                            (Some(Button1), &[LaneInput(Lane(7)),
                                              LaneInput(Lane(36+5))])] ),
        (/*KeySet { envvar:*/ &"ANGOLMOIS_SPEED_KEYS",
                 /*default:*/ &"f3|f4",
                 /*mapping:*/ &[(None, &[SpeedDownInput]),
                            (None, &[SpeedUpInput])] ),
    ];

    type KeyMap = ::core_compat::hashmap::HashMap<Input,VirtualInput>;

    /// (C: `read_keymap`)
    fn read_keymap(keyspec: &KeySpec,
                   getenv: &fn(&str) -> Option<~str>) -> KeyMap {
        fn sdl_key_from_name(name: &str) -> Option<event::Key> {
            let name = name.to_lower();
            unsafe {
                let firstkey = 0;
                let lastkey = cast::transmute(event::LastKey);
                for uint::range(firstkey, lastkey) |keyidx| {
                    let key = cast::transmute(keyidx);
                    let keyname = event::get_key_name(key).to_lower();
                    if keyname == name { return Some(key) }
                }
            }
            None
        }

        fn parse_input(s: &str) -> Option<Input> {
            let mut idx = 0;
            let s = s.trim();
            if lex!(s; "button", ws, uint -> idx) {
                Some(JoyButtonInput(idx))
            } else if lex!(s; "axis", ws, uint -> idx) {
                Some(JoyAxisInput(idx))
            } else {
                sdl_key_from_name(s).map(|&key| KeyInput(key))
            }
        }

        let mut map = ::core_compat::hashmap::HashMap::new();
        let add_mapping = |kind: Option<KeyKind>, input: Input,
                           vinput: VirtualInput| {
            if kind.map_default(true,
                    |&kind| vinput.active_in_key_spec(kind, keyspec)) {
                map.insert(input, vinput);
            }
        };

        for KEYSETS.each |&keyset| {
            let (envvar, default, mapping) = keyset; // XXX
            let spec = getenv(/*keyset.*/envvar);
            let spec = spec.get_or_default(/*keyset.*/default.to_owned());

            let mut i = 0;
            for spec.each_split_char('|') |part| {
                let (kind, vinputs) = /*keyset.*/mapping[i];
                for part.each_split_char('%') |s| {
                    match parse_input(s) {
                        Some(input) => {
                            for vinputs.each |&vinput| {
                                add_mapping(kind, input, vinput);
                            }
                        },
                        None => die!("Unknown key name in the environment \
                                      variable %s: %s", /*keyset.*/envvar, s)
                    }
                }

                i += 1;
                if i >= /*keyset.*/mapping.len() { break; }
            }
        }

        for keyspec.order.each |&lane| {
            let key = Key(36 + *lane as int);
            let kind = keyspec.kinds[*lane].get();
            let envvar = fmt!("ANGOLMOIS_%s%c_KEY", key.to_str(),
                              kind.to_char());
            for getenv(envvar).each |&s| {
                match parse_input(s) {
                    Some(input) => add_mapping(Some(kind), input,
                                               LaneInput(lane)),
                    None => die!("Unknown key name in the environment \
                                  variable %s: %s", envvar, s)
                }
            }
        }

        map
    }

    //------------------------------------------------------------------------
    // loading

    fn displayed_info(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec)
                                    -> (~str, ~str, ~str, ~str) {
        let meta = fmt!("Level %d | BPM %.2f%s | %d note%s [%uKEY%s]",
                        bms.playlevel, *bms.initbpm,
                        if infos.hasbpmchange {~"?"} else {~""},
                        infos.nnotes,
                        if infos.nnotes == 1 {~""} else {~"s"},
                        keyspec.nkeys(),
                        if infos.haslongnote {~"-LN"} else {~""});
        let title = (copy bms.title).get_or_default(~"");
        let genre = (copy bms.genre).get_or_default(~"");
        let artist = (copy bms.artist).get_or_default(~"");
        (meta, title, genre, artist)
    }

    /// (C: `play_show_stagefile` when `opt_mode < EXCLUSIVE_MODE`)
    fn show_stagefile_screen(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec,
                             opts: &Options, screen: &Surface, font: &Font) {
        let (meta, title, genre, artist) =
            displayed_info(bms, infos, keyspec);

        do screen.with_pixels |pixels| {
            font.print_string(pixels, SCREENW/2, SCREENH/2-16,
                              2, Centered, ~"loading bms file...",
                              Gradient(RGB(0x20,0x20,0x20),
                                       RGB(0x80,0x80,0x80)));
        }
        screen.flip();

        do screen.with_pixels |pixels| {
            for bms.stagefile.each |&path| {
                let path = opts.rel_path(path);
                match img::load(&path).chain(|s| s.display_format()) {
                    Ok(surface) => {
                        do surface.with_pixels |srcpixels| {
                            bicubic_interpolation(srcpixels, pixels);
                        }
                    }
                    Err(_) => {}
                }
            }

            if opts.showinfo {
                let bg = RGBA(0x10,0x10,0x10,0x40);
                let fg = Gradient(RGB(0x80,0x80,0x80), RGB(0xff,0xff,0xff));
                for uint::range(0, SCREENW) |i| {
                    for uint::range(0, 42) |j| {
                        pixels.put_blended_pixel(i, j, bg);
                    }
                    for uint::range(SCREENH-20, SCREENH) |j| {
                        pixels.put_blended_pixel(i, j, bg);
                    }
                }
                let right = SCREENW-8, bottom = SCREENH-18;
                font.print_string(pixels, 6, 4, 2, LeftAligned, title, fg);
                font.print_string(pixels, right, 4, 1,
                                  RightAligned, genre, fg);
                font.print_string(pixels, right, 20, 1,
                                  RightAligned, artist, fg);
                font.print_string(pixels, 3, bottom, 1,
                                  LeftAligned, meta, fg);
            }
        }

        screen.flip();
    }

    /// (C: `play_show_stagefile` when `opt_mode >= EXCLUSIVE_MODE`)
    fn show_stagefile_noscreen(bms: &Bms, infos: &BmsInfo,
                               keyspec: &KeySpec, opts: &Options) {
        if opts.showinfo {
            let (meta, title, genre, artist) =
                displayed_info(bms, infos, keyspec);
            io::stderr().write_line(fmt!("\
------------------------------------------------------------------------
Title:    %s\nGenre:    %s\nArtist:   %s\n%s
------------------------------------------------------------------------",
                title, genre, artist, meta));
        }
    }

    enum SoundResource {
        NoSound,
        // Rust: ideally this should be just a ~-ptr, but the current borrowck
        //       is very constrained in this aspect. after several attempts
        //       I finally sticked to delegate the ownership to a managed box.
        Sound(@~mixer::Chunk), // XXX borrowck
        SoundPlaying(@~mixer::Chunk, uint) // XXX borrowck
    }

    pub impl SoundResource {
        fn chunk(&self) -> Option<@~mixer::Chunk> {
            match *self {
                NoSound => None,
                Sound(chunk) | SoundPlaying(chunk, _) => Some(chunk)
            }
        }

        fn channel(&self) -> Option<uint> {
            match *self {
                NoSound | Sound(_) => None,
                SoundPlaying(_, channel) => Some(channel)
            }
        }

        fn start_playing(&mut self, channel: uint) {
            *self = match *self {
                NoSound => NoSound,
                Sound(chunk) | SoundPlaying(chunk, _) =>
                    SoundPlaying(chunk, channel)
            };
        }

        fn stop_playing(&mut self) {
            *self = match *self {
                NoSound => NoSound,
                Sound(chunk) | SoundPlaying(chunk, _) => Sound(chunk)
            };
        }
    }

    fn load_sound(key: Key, path: &str, opts: &Options) -> SoundResource {
        let fullpath = opts.rel_path(path); // TODO SOUND_EXTS
        match mixer::Chunk::from_wav(&fullpath) {
            Ok(res) => Sound(@res),
            Err(_) => {
                warn!("failed to load sound #WAV%s (%s)", key.to_str(), path);
                NoSound
            }
        }
    }

    enum ImageResource {
        NoImage,
        Image(@~Surface), // XXX borrowck
        Movie(@~Surface) // XXX borrowck TODO smpeg
    }

    pub impl ImageResource {
        fn surface(&self) -> Option<@~Surface> {
            match *self {
                NoImage => None,
                Image(surface) | Movie(surface) => Some(surface)
            }
        }
    }

    fn load_image(key: Key, path: &str, opts: &Options) -> ImageResource {
        fn to_display_format(surface: ~Surface) -> Result<~Surface,~str> {
            if unsafe {(*(*surface.raw).format).Amask} != 0 {
                let res = surface.display_format_alpha();
                // Rust: `|&surface|` causes an unchecked copy. (#3224)
                do res.iter |surface| { // XXX #3224
                    (*&surface).set_alpha([SrcAlpha, RLEAccel], 255);
                }
                res
            } else {
                let res = surface.display_format();
                do res.iter |surface| { // XXX #3224
                    (*&surface).set_alpha([SrcColorKey, RLEAccel], 0);
                }
                res
            }
        }

        if path.to_lower().ends_with(~".mpg") {
            if opts.has_movie() {
                warn!("skipping #BMP%s", key.to_str());
                NoImage // TODO smpeg
            } else {
                NoImage
            }
        } else if opts.has_bga() {
            let fullpath = opts.rel_path(path); // TODO SOUND_EXTS
            let res = do img::load(&fullpath).chain |surface| {
                do to_display_format(surface).chain |surface| {
                    Ok(Image(@surface))
                }
            };
            match res {
                Ok(res) => res,
                Err(_) => { warn!("failed to load image #BMP%s (%s)",
                                  key.to_str(), path);
                            NoImage }
            }
        } else {
            NoImage
        }
    }

    fn apply_blitcmd(imgres: &mut [ImageResource], bc: &BlitCmd) {
        let origin: @~Surface = match imgres[**bc.src] {
            Image(src) => src,
            _ => return
        };
        let target: @~Surface = match imgres[**bc.dst] {
            Image(dst) => dst,
            NoImage => {
                let surface = @new_surface(BGAW, BGAH);
                surface.fill(RGB(0, 0, 0));
                surface.set_color_key([SrcColorKey, RLEAccel], RGB(0, 0, 0));
                imgres[**bc.dst] = Image(surface);
                surface
            },
            _ => return
        };

        let x1 = cmp::max(bc.x1, 0);
        let y1 = cmp::max(bc.y1, 0);
        let x2 = cmp::min(bc.x2, bc.x1 + BGAW as int);
        let y2 = cmp::min(bc.y2, bc.y1 + BGAH as int);
        target.blit_area(*origin, (x1,y1), (x2,y2), (x2-x1,y2-y1));
    }

    /// (C: `load_resource`)
    fn load_resource(bms: &Bms, opts: &Options, callback: &fn(Option<~str>))
            -> (~[SoundResource], ~[ImageResource]) {
        let sndres =
            do bms.sndpath.mapi |i, &path| {
                match path {
                    Some(path) => {
                        callback(Some(copy path));
                        load_sound(Key(i as int), path, opts)
                    },
                    None => NoSound
                }
            };
        let mut imgres =
            do bms.imgpath.mapi |i, &path| {
                match path {
                    Some(path) => {
                        callback(Some(copy path));
                        load_image(Key(i as int), path, opts)
                    },
                    None => NoImage
                }
            };

        for bms.blitcmd.each |bc| {
            apply_blitcmd(imgres, bc);
        }
        (sndres, imgres)
    }

    //------------------------------------------------------------------------
    // sound management

    /// (C: `create_beep`)
    fn create_beep() -> ~mixer::Chunk {
        let samples = vec::from_fn::<i32>(12000, // approx. 0.14 seconds
            // sawtooth wave at 3150 Hz, quadratic decay after 0.02 seconds.
            |i| { let i = i as i32;
                  (i%28-14) * cmp::min(2000, (12000-i)*(12000-i)/50000) });
        mixer::Chunk::new(unsafe { cast::transmute(samples) }, 128)
    }

    //------------------------------------------------------------------------
    // pointers

    #[deriving(Clone)]
    struct Pointer {
        bms: @mut ~Bms,
        pos: uint
    }

    impl ObjQueryOps for Pointer {
        pub fn is_visible(self) -> bool {
            self.bms.objs[self.pos].is_visible()
        }
        pub fn is_invisible(self) -> bool {
            self.bms.objs[self.pos].is_invisible()
        }
        pub fn is_lnstart(self) -> bool {
            self.bms.objs[self.pos].is_lnstart()
        }
        pub fn is_lndone(self) -> bool {
            self.bms.objs[self.pos].is_lndone()
        }
        pub fn is_ln(self) -> bool {
            self.bms.objs[self.pos].is_ln()
        }
        pub fn is_bomb(self) -> bool {
            self.bms.objs[self.pos].is_bomb()
        }
        pub fn is_soundable(self) -> bool {
            self.bms.objs[self.pos].is_soundable()
        }
        pub fn is_gradable(self) -> bool {
            self.bms.objs[self.pos].is_gradable()
        }
        pub fn is_renderable(self) -> bool {
            self.bms.objs[self.pos].is_renderable()
        }
        pub fn is_object(self) -> bool {
            self.bms.objs[self.pos].is_object()
        }
        pub fn is_bgm(self) -> bool {
            self.bms.objs[self.pos].is_bgm()
        }
        pub fn is_setbga(self) -> bool {
            self.bms.objs[self.pos].is_setbga()
        }
        pub fn is_setbpm(self) -> bool {
            self.bms.objs[self.pos].is_setbpm()
        }
        pub fn is_stop(self) -> bool {
            self.bms.objs[self.pos].is_stop()
        }

        pub fn object_lane(self) -> Option<Lane> {
            self.bms.objs[self.pos].object_lane()
        }
        pub fn sounds(self) -> ~[SoundRef] {
            self.bms.objs[self.pos].sounds()
        }
        pub fn keydown_sound(self) -> Option<SoundRef> {
            self.bms.objs[self.pos].keydown_sound()
        }
        pub fn keyup_sound(self) -> Option<SoundRef> {
            self.bms.objs[self.pos].keyup_sound()
        }
        pub fn through_sound(self) -> Option<SoundRef> {
            self.bms.objs[self.pos].through_sound()
        }
        pub fn images(self) -> ~[ImageRef] {
            self.bms.objs[self.pos].images()
        }
        pub fn through_damage(self) -> Option<Damage> {
            self.bms.objs[self.pos].through_damage()
        }
    }

    pub impl Pointer {
        #[inline(always)]
        fn time(&self) -> float { self.bms.objs[self.pos].time }

        #[inline(always)]
        fn data(&self) -> ObjData { self.bms.objs[self.pos].data }

        fn seek_until(&mut self, limit: float) {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            while self.pos < nobjs {
                if bms.objs[self.pos].time >= limit { break; }
                self.pos += 1;
            }
        }

        fn iter_until(&mut self, limit: float, f: &fn(&Obj) -> bool) {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            while self.pos < nobjs {
                let current = &bms.objs[self.pos];
                if current.time >= limit { break; }
                if !f(current) { break; }
                self.pos += 1;
            }
        }

        fn seek_to(&mut self, limit: uint) {
            let bms = &*self.bms;
            assert!(limit <= bms.objs.len());
            self.pos = limit;
        }

        fn iter_to(&mut self, limit: uint, f: &fn(&Obj) -> bool) {
            let bms = &*self.bms;
            assert!(limit <= bms.objs.len());
            while self.pos < limit {
                let current = &bms.objs[self.pos];
                if !f(current) { break; }
                self.pos += 1;
            }
        }

        fn find_next_of_type(&self,
                             cond: &fn(&Obj) -> bool) -> Option<Pointer> {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            let mut i = self.pos;
            while i < nobjs {
                let current = &bms.objs[i];
                if cond(current) {
                    return Some(Pointer { bms: self.bms, pos: i });
                }
                i += 1;
            }
            None
        }

        fn find_previous_of_type(&self,
                                 cond: &fn(&Obj) -> bool) -> Option<Pointer> {
            let bms = &*self.bms;
            let mut i = self.pos;
            while i > 0 {
                i -= 1;
                let current = &bms.objs[i];
                if cond(current) {
                    return Some(Pointer { bms: self.bms, pos: i });
                }
            }
            None
        }

        fn find_closest_of_type(&self, base: float,
                                cond: &fn(&Obj) -> bool) -> Option<Pointer> {
            let previous = self.find_previous_of_type(cond);
            let next = self.find_next_of_type(cond);
            match (previous, next) {
                (None, None) => None,
                (None, Some(next)) => Some(next),
                (Some(previous), None) => Some(previous),
                (Some(previous), Some(next)) =>
                    if num::abs(previous.time() - base) <
                       num::abs(next.time() - base) { Some(previous) }
                    else { Some(next) }
            }
        }
    }

    fn Pointer(bms: @mut ~Bms) -> Pointer {
        Pointer { bms: bms, pos: 0 }
    }

    fn pointer_with_pos(bms: @mut ~Bms, pos: uint) -> Pointer {
        Pointer { bms: bms, pos: pos }
    }

    priv fn has_same_bms(lhs: &Pointer, rhs: &Pointer) -> bool {
        ::core::managed::mut_ptr_eq(lhs.bms, rhs.bms)
    }

    impl Eq for Pointer {
        fn eq(&self, other: &Pointer) -> bool {
            has_same_bms(self, other) && self.pos == other.pos
        }
        fn ne(&self, other: &Pointer) -> bool {
            !has_same_bms(self, other) || self.pos != other.pos
        }
    }

    impl Ord for Pointer {
        fn lt(&self, other: &Pointer) -> bool {
            assert!(has_same_bms(self, other));
            self.pos < other.pos
        }
        fn le(&self, other: &Pointer) -> bool {
            assert!(has_same_bms(self, other));
            self.pos <= other.pos
        }
        fn ge(&self, other: &Pointer) -> bool {
            assert!(has_same_bms(self, other));
            self.pos >= other.pos
        }
        fn gt(&self, other: &Pointer) -> bool {
            assert!(has_same_bms(self, other));
            self.pos > other.pos
        }
    }

    //------------------------------------------------------------------------
    // game play logics

    #[deriving(Eq)]
    enum Grade {
        MISS  = 0,
        BAD   = 1,
        GOOD  = 2,
        GREAT = 3,
        COOL  = 4,
    }

    static COOL_CUTOFF: float = 14.4;
    static GREAT_CUTOFF: float = 48.0;
    static GOOD_CUTOFF: float = 84.0;
    static BAD_CUTOFF: float = 144.0;

    static NGRADES: uint = 5;

    static MAXGAUGE: int = 512;
    static SCOREPERNOTE: float = 300.0;

    static MISS_DAMAGE: Damage = GaugeDamage(0.059);
    static BAD_DAMAGE: Damage = GaugeDamage(0.030);

    /// Game play states independent to the display.
    struct Player {
        /// The game play options.
        opts: ~Options,
        /// The current BMS data.
        // Rust: this should have been just `~Bms`, and `Pointer` should have
        //       received a lifetime parameter (for `&'self Bms` things).
        //       in reality, though, a lifetime parameter made borrowck much
        //       stricter and I ended up with wrapping `bms` to a mutable
        //       managed box.
        bms: @mut ~Bms,
        /// The derived BMS information.
        infos: ~BmsInfo,
        /// The length of BMS file in seconds as calculated by `bms_duration`.
        /// (C: `duration`)
        duration: float,
        /// The key specification.
        keyspec: ~KeySpec,
        /// The input mapping.
        keymap: ~KeyMap,

        /// Set to true if the corresponding object in `bms.objs` had graded
        /// and should not be graded twice. Its length equals to that of
        /// `bms.objs`. (C: `nograding` field in `struct obj`)
        nograding: ~[bool],

        /// The chart expansion rate, or "play speed". One measure has
        /// the length of 400 pixels times the play speed, so higher play
        /// speed means that objects will fall much more quickly (hence
        /// the name). (C: `playspeed`)
        playspeed: float,
        /// The play speed targeted for speed change if any. It is also
        /// the value displayed while the play speed is changing.
        /// (C: `targetspeed`)
        targetspeed: Option<float>,
        /// The current BPM. Can be negative, in that case the chart will
        /// scroll backwards. (C: `bpm`)
        bpm: BPM,
        /// The timestamp at the last tick. It is a return value from
        /// `util::sdl::ticks` and measured in milliseconds. (C: `now`)
        now: uint,
        /// The timestamp at the first tick. (C: `origintime`)
        origintime: uint,
        /// The timestamp at the last discontinuity that breaks a linear
        /// relationship between the virtual time and actual time.
        /// (C: `starttime`) Currently the following are considered
        /// a discontinuity:
        ///
        /// * `origintime`
        /// * A change in BPM
        /// * A change in scaling factor of measure
        /// * A scroll stopper (in this case, `stoptime` is first updated and
        ///   `starttime` is updated at the end of stop)
        starttime: uint,
        /// The timestamp at the end of ongoing scroll stopper, if any.
        /// (C: `stoptime`)
        stoptime: Option<uint>,
        /// The virtual time at the last discontinuity. (C: `startoffset`)
        startoffset: float,
        /// The current scaling factor of measure. (C: `startshorten`)
        startshorten: float,

        /// The virtual time at the bottom of the visible chart. (C: `bottom`)
        bottom: float,
        /// The virtual time at the top of the visible chart. (C: `top`)
        top: float,
        /// A pointer to the first `Obj` after `bottom`. (C: `pfront`)
        pfront: Pointer,
        /// A pointer to the first `Obj` after the grading line, which is
        /// currently same as `bottom`. (C: `pcur`)
        pcur: Pointer,
        /// A pointer to the first `Obj` that haven't escaped the grading
        /// area. It is possible that this `Obj` haven't reached the grading
        /// area either. (C: `pcheck`)
        pcheck: Pointer,
        /// Pointers to `Obj`s for the start of LN which grading is
        /// in progress. (C: `pthru`)
        pthru: ~[Option<Pointer>],

        /// The scale factor for grading area. The factor less than 1 causes
        /// the grading area shrink. (C: `gradefactor`)
        gradefactor: float,
        /// (C: `grademode`)
        lastgrade: Grade,
        /// The numbers of each grades. (C: `scocnt`)
        gradecounts: [uint, ..NGRADES],
        /// The last combo number, i.e. the number of objects graded at least
        /// GREAT. GOOD doesn't cause the combo number reset; BAD and MISS do.
        /// (C: `scombo`)
        lastcombo: uint,
        /// The best combo number so far. If the player manages to get no BADs
        /// and MISSes, then the combo number should end up with the number of
        /// note and LN objects (`BMSInfo::nnotes`). (C: `smaxcombo`)
        bestcombo: uint,
        /// The current score. (C: `score`)
        score: uint,
        /// The current health gauge. Should be no larger than `MAXGAUGE`.
        /// This can go negative (not displayed directly), which will require
        /// players much more efforts to survive. (C: `gauge`)
        gauge: int,
        /// The health gauge required to survive at the end of the song.
        /// Note that the gauge less than this value (or even zero) doesn't
        /// cause the instant game over; only `InstantDeath` value from
        /// `Damage` does. (C: `survival`)
        survival: int,

        /// The number of keyboard or joystick keys, mapped to each lane and
        /// and currently pressed. (C: `keypressed[0]`)
        keymultiplicity: [uint, ..NLANES],
        /// The state of joystick axes. (C: `keypressed[1]`)
        joystate: [InputState, ..NLANES],
    }

    fn Player(opts: ~Options, bms: ~Bms, infos: ~BmsInfo, duration: float,
              keyspec: ~KeySpec, keymap: ~KeyMap) -> Player {
        let now = ticks();
        let originoffset = infos.originoffset;
        let startshorten = bms.shorten_factor(originoffset as int);
        let gradefactor = 1.5 - cmp::min(bms.rank, 5) as float * 0.25;
        let initialgauge = MAXGAUGE * 500 / 1000;
        let survival = MAXGAUGE * 293 / 1000;
        let initbpm = bms.initbpm;
        let nobjs = bms.objs.len();

        let bms = @mut bms;
        let initptr = Pointer(bms);
        Player {
            opts: opts, bms: bms, infos: infos, duration: duration,
            keyspec: keyspec, keymap: keymap,

            nograding: vec::from_elem(nobjs, false),

            playspeed: 1.0, targetspeed: None, bpm: initbpm, now: now,
            origintime: now, starttime: now, stoptime: None,
            startoffset: originoffset, startshorten: startshorten,

            bottom: originoffset, top: originoffset, pfront: initptr,
            pcur: initptr, pcheck: initptr, pthru: ~[None, ..NLANES],

            gradefactor: gradefactor, lastgrade: MISS,
            gradecounts: [0, ..NGRADES], lastcombo: 0, bestcombo: 0, score: 0,
            gauge: initialgauge, survival: survival,

            keymultiplicity: [0, ..NLANES], joystate: [Neutral, ..NLANES],
        }
    }

    pub impl Player {
        fn key_pressed(&self, lane: Lane) -> bool {
            self.keymultiplicity[*lane] > 0 || self.joystate[*lane] != Neutral
        }

        /// (C: `update_grade`)
        fn update_grade(&mut self, grade: Grade, scoredelta: float,
                        damage: Option<Damage>) -> bool {
            self.gradecounts[grade as uint] += 1;
            self.lastgrade = grade;
            //gradetime = now + 700; /* disappears after 700ms */
            self.score += (scoredelta * SCOREPERNOTE *
                           (1.0 + (self.lastcombo as float) /
                                  (self.infos.nnotes as float))) as uint;

            match grade {
                MISS => {
                    self.lastcombo = 0;
                    //poorlimit = now + 600; /* switches to the normal BGA after 600ms */
                }
                BAD => { self.lastcombo = 0; }
                GOOD => {}
                GREAT|COOL => {
                    // at most 5/512(1%) recover when the combo is topped
                    let weight = if grade == GREAT {2} else {3};
                    let cmbbonus = cmp::min(self.lastcombo as int, 100) / 50;
                    self.lastcombo += 1;
                    self.gauge = cmp::min(self.gauge + weight + cmbbonus,
                                          MAXGAUGE);
                }
            }
            self.bestcombo = cmp::max(self.bestcombo, self.lastcombo);

            match damage {
                Some(GaugeDamage(ratio)) => {
                    self.gauge -= (MAXGAUGE as float * ratio) as int; true
                }
                Some(InstantDeath) => {
                    self.gauge = cmp::min(self.gauge, 0); false
                }
                None => true
            }
        }

        fn update_grade_from_distance(&mut self, dist: float) {
            let dist = num::abs(dist);
            let (grade, damage) =
                if      dist <  COOL_CUTOFF {(COOL,None)}
                else if dist < GREAT_CUTOFF {(GREAT,None)}
                else if dist <  GOOD_CUTOFF {(GOOD,None)}
                else if dist <   BAD_CUTOFF {(BAD,Some(BAD_DAMAGE))}
                else                        {(MISS,Some(MISS_DAMAGE))};
            let scoredelta = cmp::max(1.0 - dist / BAD_CUTOFF, 0.0);
            let keepgoing = self.update_grade(grade, scoredelta, damage);
            assert!(keepgoing);
        }

        fn update_grade_from_damage(&mut self, damage: Damage) -> bool {
            self.update_grade(MISS, 0.0, Some(damage))
        }

        fn update_grade_to_miss(&mut self) {
            let keepgoing = self.update_grade(MISS, 0.0, Some(MISS_DAMAGE));
            assert!(keepgoing);
        }

        /// (C: `play_process`)
        fn tick(&mut self) -> bool {
            // Rust: this is very extreme case of loan conflict. (#4666)
            let bms = &*self.bms;
            let mut pfront = self.pfront.clone();
            let mut pcur = self.pcur.clone();
            let mut pcheck = self.pcheck.clone();
            let mut pthru = self.pthru.clone();

            if self.targetspeed.is_some() {
                let target = self.targetspeed.get();
                let delta = target - self.playspeed;
                if num::abs(delta) < 0.001 {
                    self.playspeed = target;
                    self.targetspeed = None;
                } else {
                    self.playspeed += delta * 0.1;
                }
            }

            let now = ticks();
            let bottom = match self.stoptime {
                Some(t) => {
                    if now >= t {
                        self.starttime = t;
                        self.stoptime = None;
                    }
                    self.startoffset
                }
                None => {
                    let msecdiff = (now - self.starttime) as float;
                    let measurediff = self.bpm.msec_to_measure(msecdiff);
                    self.startoffset + measurediff / self.startshorten
                }
            };

            let bottommeasure = bottom.floor();
            let curshorten = bms.shorten_factor(bottommeasure as int);
            if bottommeasure >= -1.0 && self.startshorten != curshorten {
                let measurediff = bottommeasure as float - self.startoffset;
                self.starttime += (self.bpm.measure_to_msec(measurediff) *
                                   self.startshorten) as uint;
                self.startoffset = bottommeasure;
                self.startshorten = curshorten;
            }

            //let line = bms.adjust_object_time(bottom, 0.03/self.playspeed);
            let line = bottom;
            let lineshorten = bms.shorten_factor(line.floor() as int);
            let top = bms.adjust_object_time(bottom, 1.25 / self.playspeed);

            pfront.seek_until(bottom);
            let mut prevpcur = pointer_with_pos(self.bms, pcur.pos);
            for pcur.iter_until(line) |&obj| {
                match obj.data {
                    BGM(ref sref) => {
                        //if (index) play_sound(index, 1);
                    }
                    SetBGA(layer, ref iref) => {
                        //if (bga[type] >= 0 && imgres[bga[type]].movie) {
                        //    SMPEG_stop(imgres[bga[type]].movie);
                        //}
                        //bga[type] = index;
                        //if (index >= 0 && imgres[index].movie) {
                        //    SMPEG_rewind(imgres[index].movie);
                        //    SMPEG_play(imgres[index].movie);
                        //}
                    }
                    SetBPM(newbpm) => {
                        self.starttime = now;
                        self.startoffset = bottom;
                        self.bpm = newbpm;
                    }
                    Stop(dur) => {
                        let msecs = match dur {
                            Seconds(t) => t * 1000.0,
                            Measures(t) => self.bpm.measure_to_msec(t)
                        };
                        let newstoptime = msecs as uint + now;
                        self.stoptime = match self.stoptime {
                            None => Some(newstoptime),
                            Some(t) => Some(cmp::max(t, newstoptime))
                        };
                        self.startoffset = obj.time;
                    }
                    Visible(_,sref)|LNStart(_,sref) => {
                        if self.opts.is_autoplay() {
                            //if (index) play_sound(index, 0);
                            self.update_grade_from_distance(0.0);
                        }
                    }
                    _ => {}
                }
            }

            if !self.opts.is_autoplay() {
                for pcheck.iter_to(pcur.pos) |&obj| {
                    let dist = self.bpm.measure_to_msec(line - obj.time) *
                               bms.shorten_factor(obj.measure()) *
                               self.gradefactor;
                    if dist < 144.0 { break; }
                    if !self.nograding[pcheck.pos] {
                        for obj.object_lane().each |&Lane(lane)| {
                            let missable =
                                match obj.data {
                                    Visible(_,_)|LNStart(_,_) => true,
                                    LNDone(_,_) => pthru[lane].is_some(),
                                    _ => false,
                                };
                            if missable {
                                self.update_grade_to_miss();
                                pthru[lane] = None;
                            }
                        }
                    }
                }
            }

            loop {
                let (key, state) = match poll_event() {
                    NoEvent => break,
                    QuitEvent | KeyEvent(EscapeKey,_,_,_) => return false,
                    KeyEvent(key,true,_,_) => (KeyInput(key), Positive),
                    KeyEvent(key,false,_,_) => (KeyInput(key), Neutral),
                    JoyButtonEvent(_which,button,true) =>
                        (JoyButtonInput(button as uint), Positive),
                    JoyButtonEvent(_which,button,false) =>
                        (JoyButtonInput(button as uint), Neutral),
                    JoyAxisEvent(_which,axis,delta) if delta > 3200 =>
                        (JoyAxisInput(axis as uint), Positive),
                    JoyAxisEvent(_which,axis,delta) if delta < -3200 =>
                        (JoyAxisInput(axis as uint), Negative),
                    JoyAxisEvent(_which,axis,_) =>
                        (JoyAxisInput(axis as uint), Neutral),
                    _ => loop
                };
                let vkey = match self.keymap.find(&key) {
                    Some(&vkey) => vkey,
                    None => loop
                };
                let continuous = match key {
                    KeyInput(*) | JoyButtonInput(*) => false,
                    JoyAxisInput(*) => true
                };

                if self.opts.is_exclusive() { loop; }

                static speeds: &'static [float] = &[0.1, 0.2, 0.4, 0.6, 0.8,
                    1.0, 1.2, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5,
                    6.0, 7.0, 8.0, 10.0, 15.0, 25.0, 40.0, 60.0, 99.0];

                fn next_speed_mark(current: float) -> Option<float> {
                    let mut prev = None;
                    for speeds.each |&speed| {
                        if speed < current - 0.001 {
                            prev = Some(speed);
                        } else {
                            return prev;
                        }
                    }
                    None
                }

                fn previous_speed_mark(current: float) -> Option<float> {
                    let mut next = None;
                    for speeds.each_reverse |&speed| {
                        if speed > current + 0.001 {
                            next = Some(speed);
                        } else {
                            return next;
                        }
                    }
                    None
                }

                let is_unpressed = |lane: Lane, continuous: bool,
                                    state: InputState| {
                    if state == Neutral || (continuous &&
                                            self.joystate[*lane] != state) {
                        if continuous {
                            self.joystate[*lane] = state; true
                        } else {
                            if (self.keymultiplicity[*lane] > 0) {
                                self.keymultiplicity[*lane] -= 1;
                            }
                            (self.keymultiplicity[*lane] > 0)
                        }
                    } else {
                        false
                    }
                };

                let is_pressed = |lane: Lane, continuous: bool,
                                  state: InputState| {
                    if state != Neutral {
                        if continuous {
                            self.joystate[*lane] = state; true
                        } else {
                            self.keymultiplicity[*lane] += 1;
                            (self.keymultiplicity[*lane] == 1)
                        }
                    } else {
                        false
                    }
                };

                let process_unpress = |lane: Lane| {
                    for pthru[*lane].each |&thru| {
                        let nextlndone = do thru.find_next_of_type |&obj| {
                            obj.object_lane() == Some(lane) &&
                            obj.is_lndone()
                        };
                        for nextlndone.each |&p| {
                            let delta =
                                self.bpm.measure_to_msec(p.time() - line) *
                                lineshorten * self.gradefactor;
                            if num::abs(delta) < 144.0 {
                                self.nograding[p.pos] = true;
                            } else {
                                self.update_grade_to_miss();
                            }
                        }
                    }
                    pthru[*lane] = None;
                };

                let process_press = |lane: Lane| {
                    let soundable =
                        do pcur.find_closest_of_type(line) |&obj| {
                            obj.object_lane() == Some(lane) &&
                            obj.is_soundable()
                        };
                    for soundable.each |&p| {
                        for p.sounds().each |&sref| {
                            //play_sound(objs[l].index, 0);
                        }
                    }

                    let gradable =
                        do pcur.find_closest_of_type(line) |&obj| {
                            obj.object_lane() == Some(lane) &&
                            obj.is_gradable()
                        };
                    for gradable.each |&p| {
                        if p.pos >= pcheck.pos && !self.nograding[p.pos] &&
                                !p.is_lndone() {
                            let dist =
                                self.bpm.measure_to_msec(p.time() - line) *
                                lineshorten * self.gradefactor;
                            if num::abs(dist) < 144.0 {
                                if p.is_lnstart() {
                                    pthru[*lane] =
                                        Some(pointer_with_pos(self.bms,
                                                              p.pos));
                                }
                                self.nograding[p.pos] = true;
                                self.update_grade_from_distance(dist);
                            }
                        }
                    }
                    true
                };

                match (vkey, state) {
                    (SpeedDownInput, Positive) |
                    (SpeedDownInput, Negative) => {
                        let current =
                            self.targetspeed.get_or_default(self.playspeed);
                        for next_speed_mark(current).each |&newspeed| {
                            self.targetspeed = Some(newspeed);
                            //Mix_PlayChannel(0, beep, 0);
                        }
                    }
                    (SpeedUpInput, Positive) |
                    (SpeedUpInput, Negative) => {
                        let current =
                            self.targetspeed.get_or_default(self.playspeed);
                        for previous_speed_mark(current).each |&newspeed| {
                            self.targetspeed = Some(newspeed);
                            //Mix_PlayChannel(0, beep, 0);
                        }
                    }
                    (LaneInput(lane), state) => {
                        if !self.opts.is_autoplay() {
                            if is_unpressed(lane, continuous, state) {
                                process_unpress(lane);
                            }
                            if is_pressed(lane, continuous, state) {
                                process_press(lane);
                            }
                        }
                    }
                    (_, _) => {}
                }

            }

            if !self.opts.is_autoplay() {
                for prevpcur.iter_to(pcur.pos) |&obj| {
                    match obj.data {
                        Bomb(lane,sref,damage) if self.key_pressed(lane) => {
                            // ongoing long note is not graded twice
                            pthru[*lane] = None;
                            //play_sound(objs[i].index, 0);
                            if !self.update_grade_from_damage(damage) {
                                // instant death
                                pcur.seek_to(bms.objs.len());
                                return false;
                            }
                        }
                        _ => {}
                    }
                }
            }

            // used for reference time & positions for rendering
            self.now = now;
            self.bottom = bottom;
            self.top = top;
            self.pfront = pfront;
            self.pcur = pcur;
            self.pcheck = pcheck;
            self.pthru = pthru;

            if bottom > bms.nmeasures as float {
                //if (opt_mode ? Mix_Playing(-1)==Mix_Playing(0) : Mix_GroupNewer(1)==-1) return 0;
                false
            } else if bottom < self.infos.originoffset {
                false
            } else {
                true
            }
        }
    }

    //------------------------------------------------------------------------
    // display

    /// (C: `struct tkeykind` and `tkeyleft`)
    struct LaneStyle {
        left: uint,
        spriteleft: uint,
        spritebombleft: uint,
        width: uint,
        basecolor: Color
    }

    pub impl LaneStyle {
        /// (C: `tkeykinds`)
        fn from_kind(kind: KeyKind, pos: Either<uint,uint>) -> LaneStyle {
            let (spriteleft, spritebombleft, width, color) = match kind {
                WhiteKey    => ( 25,   0, 25, RGB(0x80,0x80,0x80)),
                WhiteKeyAlt => ( 50,   0, 25, RGB(0xf0,0xe0,0x80)),
                BlackKey    => ( 75,   0, 25, RGB(0x80,0x80,0xff)),
                Button1     => (130, 100, 30, RGB(0xe0,0xe0,0xe0)),
                Button2     => (160, 100, 30, RGB(0xff,0xff,0x40)),
                Button3     => (190, 100, 30, RGB(0x80,0xff,0x80)),
                Button4     => (220, 100, 30, RGB(0x80,0x80,0xff)),
                Button5     => (250, 100, 30, RGB(0xff,0x40,0x40)),
                Scratch     => (320, 280, 40, RGB(0xff,0x80,0x80)),
                FootPedal   => (360, 280, 40, RGB(0x80,0xff,0x80)),
            };
            let left = pos.either(|&left| left, |&right| right - width);
            LaneStyle { left: left, spriteleft: spriteleft,
                        spritebombleft: spritebombleft,
                        width: width, basecolor: color }
        }

        fn note_color(&self) -> Gradient {
            Gradient(self.basecolor, RGB(0xff,0xff,0xff))
        }

        fn bomb_color(&self) -> Gradient {
            Gradient(RGB(0xc0,0,0), RGB(0,0,0))
        }

        fn back_color(&self) -> Gradient {
            Gradient(self.basecolor, RGB(0,0,0))
        }

        fn render_to_sprite(&self, sprite: &Surface) {
            let left = self.spriteleft;
            let noteleft = self.spriteleft + SCREENW;
            let bombleft = self.spritebombleft + SCREENW;
            assert!(sprite.get_width() as uint >=
                    cmp::max(noteleft, bombleft) + self.width);

            let notecolor = self.note_color();
            let bombcolor = self.bomb_color();
            let backcolor = self.back_color();

            // render a background sprite
            for uint::range(140, SCREENH - 80) |i| {
                sprite.fill_area((left, i), (self.width, 1),
                                 backcolor.blend(i as int - 140, 1000));
            }

            // render note and bomb sprites
            let denom = self.width as int;
            for uint::range(0, self.width / 2) |i| {
                let num = (self.width - i) as int;
                sprite.fill_area((noteleft+i, 0), (self.width-i*2, SCREENH),
                                 notecolor.blend(num, denom));
                sprite.fill_area((bombleft+i, 0), (self.width-i*2, SCREENH),
                                 bombcolor.blend(num, denom));
            }
        }

        fn render_back(&self, screen: &Surface, sprite: &Surface,
                       pressed: bool) {
            screen.fill_area((self.left, 30), (self.width, SCREENH-110),
                             RGB(0,0,0));
            if pressed {
                screen.blit_area(sprite,
                                 (self.spriteleft, 140), (self.left, 140),
                                 (self.width, SCREENH-220));
            }
        }

        fn render_note(&self, screen: &Surface, sprite: &Surface,
                       top: uint, bottom: uint) {
            screen.blit_area(sprite, (self.spriteleft + SCREENW, 0),
                             (self.left, top), (self.width, bottom - top));
        }

        fn render_bomb(&self, screen: &Surface, sprite: &Surface,
                       top: uint, bottom: uint) {
            screen.blit_area(sprite, (self.spritebombleft + SCREENW, 0),
                             (self.left, top), (self.width, bottom - top));
        }
    }

    fn build_lane_styles(keyspec: &KeySpec) -> (uint, Option<uint>,
                                                ~[(Lane,LaneStyle)]) {
        let mut leftmost = 0, rightmost = SCREENW;
        let mut styles = ~[];
        for keyspec.left_lanes().each |&lane| {
            let kind = keyspec.kinds[*lane];
            assert!(kind.is_some());
            let kind = kind.get();
            let style = LaneStyle::from_kind(kind, Left(leftmost));
            styles.push((lane, style));
            leftmost += style.width + 1;
        }
        for keyspec.right_lanes().each |&lane| {
            let kind = keyspec.kinds[*lane];
            assert!(kind.is_some());
            let kind = kind.get();
            let style = LaneStyle::from_kind(kind, Right(rightmost));
            styles.push((lane, style));
            rightmost -= style.width + 1;
        }

        let rightmost = if rightmost == SCREENW {None}
                        else {Some(rightmost)};
        (leftmost, rightmost, styles)
    }

    /// (C: sprite construction portion of `play_prepare`)
    fn create_sprite(opts: &Options, leftmost: uint, rightmost: Option<uint>,
                     styles: &[(Lane,LaneStyle)]) -> ~Surface {
        let sprite = new_surface(SCREENW + 400, SCREENH);
        let black = RGB(0,0,0);
        let gray = RGB(0x40,0x40,0x40); // gray used for separators

        // render notes and lane backgrounds
        for styles.each |&(_,style)| {
            style.render_to_sprite(sprite);
        }

        // render panels
        do sprite.with_pixels |&pixels| {
            let topgrad = Gradient(RGB(0x60,0x60,0x60), RGB(0xc0,0xc0,0xc0));
            let botgrad = Gradient(RGB(0x40,0x40,0x40), RGB(0xc0,0xc0,0xc0));
            for int::range(-244, 556) |j| {
                for int::range(-10, 20) |i| {
                    let c = (i*2+j*3+750) % 2000;
                    pixels.put_pixel((j+244) as uint, (i+10) as uint,
                        topgrad.blend(850 - num::abs(c-1000), 700));
                }
                for int::range(-20, 60) |i| {
                    let c = (i*3+j*2+750) % 2000;
                    let bottom = (SCREENH - 60) as int;
                    pixels.put_pixel((j+244) as uint, (i+bottom) as uint,
                        botgrad.blend(850 - num::abs(c-1000), 700));
                }
            }
        }
        sprite.fill_area((10, SCREENH-36), (leftmost, 1), gray);

        // erase portions of panels left unused
        let leftgap = leftmost + 20;
        let rightgap = rightmost.map_default(SCREENW, |x| x - 20);
        let gapwidth = rightgap - leftgap;
        sprite.fill_area((leftgap, 0), (gapwidth, 30), black);
        sprite.fill_area((leftgap, SCREENH-80), (gapwidth, 80), black);
        do sprite.with_pixels |&pixels| {
            for uint::range(0, 20) |i| {
                for uint::range_rev(20, 0) |j| {
                    if i*i + j*j <= 400 { break; } // circled border
                    pixels.put_pixel(leftmost + j, 10 + i, black);
                    pixels.put_pixel(leftmost + j, (SCREENH-61) - i, black);
                    for rightmost.each |&right| {
                        pixels.put_pixel((right-j) - 1, 10 + i, black);
                        pixels.put_pixel((right-j) - 1, (SCREENH-61) - i,
                                         black);
                    }
                }
            }
        }

        // draw the gauge bar if needed
        if !opts.is_autoplay() {
            sprite.fill_area((0, SCREENH-16), (368, 16), gray);
            sprite.fill_area((4, SCREENH-12), (360, 8), black);
        }

        sprite
    }

    trait Display {
        pub fn render(&mut self, player: &Player);
    }

    struct GraphicDisplay {
        /// (C: `sprite`)
        sprite: ~Surface,
        /// (C: `screen`)
        screen: ~Surface,
        ///
        font: ~Font,
        /// (C: `tpanel1`)
        leftmost: uint,
        /// (C: `tpanel2`)
        rightmost: Option<uint>,
        /// (C: `tkey` and `tkeyleft`)
        lanestyles: ~[(Lane,LaneStyle)],
        /// (C: `tbgax`)
        bgax: uint,
        /// (C: `tbgay`)
        bgay: uint,

        /// (C: `poorlimit`)
        poorlimit: Option<uint>,
        /// (C: `gradetime`)
        gradelimit: Option<uint>,
    }

    fn GraphicDisplay(opts: &Options, keyspec: &KeySpec, screen: ~Surface,
                      font: ~Font) -> GraphicDisplay {
        let (leftmost, rightmost, styles) = build_lane_styles(keyspec);
        let centerwidth = rightmost.get_or_default(SCREENW) - leftmost;
        let bgax = leftmost + (centerwidth - BGAW) / 2;
        let bgay = (SCREENH - BGAH) / 2;
        let sprite = create_sprite(opts, leftmost, rightmost, styles);

        let display = GraphicDisplay {
            sprite: sprite, screen: screen, font: font, leftmost: leftmost,
            rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
            poorlimit: None, gradelimit: None,
        };

        display.screen.fill(RGB(0,0,0));
        display.restore_panel();
        display.screen.flip();

        display
    }

    /// (C: `tgradestr` and `tgradecolor`)
    static GRADES: &'static [(&'static str,Gradient)] = &[
        // Rust: can we just use `Gradient()`???
        ("MISS",  Gradient { top:    RGB(0xff,0x80,0x80),
                             bottom: RGB(0xff,0x40,0x40) }),
        ("BAD",   Gradient { top:    RGB(0xff,0x80,0xff),
                             bottom: RGB(0xff,0x40,0xff) }),
        ("GOOD",  Gradient { top:    RGB(0xff,0xff,0x80),
                             bottom: RGB(0xff,0xff,0x40) }),
        ("GREAT", Gradient { top:    RGB(0x80,0xff,0x80),
                             bottom: RGB(0x40,0xff,0x40) }),
        ("COOL",  Gradient { top:    RGB(0x80,0x80,0xff),
                             bottom: RGB(0x40,0x40,0xff) }),
    ];

    impl GraphicDisplay {
        fn restore_panel(&self) {
            let screen: &Surface = self.screen;
            let sprite: &Surface = self.sprite;
            screen.blit_area(sprite, (0,0), (0,0), (SCREENW,30));
            screen.blit_area(sprite,
                             (0,SCREENH-80), (0,SCREENH-80), (SCREENW,80));
        }
    }

    impl Display for GraphicDisplay {
        fn render(&mut self, player: &Player) {
            let screen = &*self.screen;
            let sprite = &*self.sprite;
            let font = &*self.font;
            let bms = &*player.bms;

            // fill the lanes to the border color
            screen.fill_area((0, 30), (self.leftmost, SCREENH-110),
                             RGB(0x40,0x40,0x40));
            for self.rightmost.each |&rightmost| {
                screen.fill_area((rightmost, 30), (SCREENH-rightmost, 490),
                                 RGB(0x40,0x40,0x40));
            }
            for self.lanestyles.each |&(lane,style)| {
                style.render_back(screen, sprite, player.key_pressed(lane));
            }

            screen.set_clip_area((0, 30), (SCREENW, SCREENH-110));

            // render objects
            let time_to_y = |time| {
                let adjusted = bms.adjust_object_position(player.bottom,
                                                          time);
                (SCREENH-70) - (400.0 * player.playspeed * adjusted) as uint
            };
            for self.lanestyles.each |&(lane,style)| {
                let front = do player.pfront.find_next_of_type |&obj| {
                    obj.object_lane() == Some(lane) && obj.is_renderable()
                };
                if front.is_none() { loop; }
                let front = front.get();

                // LN starting before the bottom and ending after the top
                if front.time() > player.top && front.is_lndone() {
                    style.render_note(screen, sprite, 30, SCREENH - 80);
                } else {
                    let mut i = front.pos;
                    let mut nextbottom = None;
                    let nobjs = bms.objs.len(), top = player.top;
                    while i < nobjs && bms.objs[i].time <= top {
                        let y = time_to_y(bms.objs[i].time);
                        match bms.objs[i].data {
                            LNStart(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                nextbottom = Some(y);
                            }
                            LNDone(lane0,_) if lane0 == lane => {
                                let bottom = SCREENH-80;
                                style.render_note(screen, sprite, y,
                                    nextbottom.get_or_default(bottom));
                                nextbottom = None;
                            }
                            Visible(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_note(screen, sprite, y-5, y);
                            }
                            Bomb(lane0,_,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_bomb(screen, sprite, y-5, y);
                            }
                            _ => {}
                        }
                        i += 1;
                    }

                    for nextbottom.each |&y| {
                        style.render_note(screen, sprite, SCREENH-80, y);
                    }
                }
            }

            // render measure bars
            for int::range(player.bottom.floor() as int,
                           player.top.floor() as int + 1) |i| {
                let y = time_to_y(i as float);
                screen.fill_area((0, y), (self.leftmost, 1),
                                 RGB(0xc0,0xc0,0xc0));
                for self.rightmost.each |&rightmost| {
                    screen.fill_area((rightmost, y), (800-rightmost, 1),
                                     RGB(0xc0,0xc0,0xc0));
                }
            }

            // render grading text
            if self.gradelimit.is_some() {
                let gradelimit = self.gradelimit.get();
                let (gradename,gradecolor) = GRADES[player.lastgrade as uint];
                let delta = cmp::max(0, (gradelimit - player.now - 400) / 15);
                do screen.with_pixels |pixels| {
                    font.print_string(pixels, self.leftmost/2,
                                      SCREENH/2 - 40 - delta, 2, Centered,
                                      gradename, gradecolor);
                    if player.lastcombo > 1 {
                        font.print_string(pixels, self.leftmost/2,
                                          SCREENH/2 - 12 - delta, 1, Centered,
                                          fmt!("%u COMBO", player.lastcombo),
                                          Gradient(RGB(0x80,0x80,0x80),
                                                   RGB(0xff,0xff,0xff)));
                    }
                    if player.opts.is_autoplay() {
                        font.print_string(pixels, self.leftmost/2,
                                          SCREENH/2 + 2 - delta, 1,
                                          Centered, "(AUTO)",
                                          Gradient(RGB(0x40,0x40,0x40),
                                                   RGB(0xc0,0xc0,0xc0)));
                    }
                }
            }

            screen.set_clip_rect(&screen.get_rect());

            self.restore_panel();

            // render panel
            let elapsed = (player.now - player.origintime) / 1000;
            let duration = (player.duration / 1000.0) as uint;
            let durationmsec = player.duration as uint;
            do screen.with_pixels |pixels| {
                let black = RGB(0,0,0);
                font.print_string(pixels, 10, 8, 1, LeftAligned,
                                  fmt!("SCORE %07u", player.score), black);
                let nominalplayspeed =
                    player.targetspeed.get_or_default(player.playspeed);
                font.print_string(pixels, 5, SCREENH-78, 2, LeftAligned,
                                  fmt!("%4.1fx", nominalplayspeed), black);
                font.print_string(pixels, self.leftmost-94, SCREENH-35,
                                  1, LeftAligned,
                                  fmt!("%02u:%02u / %02u:%02u",
                                       elapsed/60, elapsed%60,
                                       duration/60, duration%60), black);
                font.print_string(pixels, 95, SCREENH-62, 1, LeftAligned,
                                  fmt!("@%9.4f", player.bottom), black);
                font.print_string(pixels, 95, SCREENH-78, 1, LeftAligned,
                                  fmt!("BPM %6.2f", *player.bpm), black);
                let timetick =
                    cmp::min(self.leftmost, (player.now - player.origintime) *
                                            self.leftmost / durationmsec);
                font.print_glyph(pixels, 6 + timetick, SCREENH-52, 1,
                                 95, RGB(0x40,0x40,0x40)); // glyph #95: tick
            }

            // render gauge
            if !player.opts.is_autoplay() {
                // cycles four times per measure, [0,40)
                let cycle = (160.0 * player.startshorten *
                             player.bottom).floor() % 40.0;
                let width =
                    if player.gauge < 0 {0}
                    else {player.gauge * 400 / MAXGAUGE - (cycle as int)};
                let width = ::util::cmp::clamp(5, width, 360);
                let color =
                    if player.gauge >= player.survival {RGB(0xc0,0,0)}
                    else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
                screen.fill_area((4, SCREENH-12), (width, 8), color);
            }

            // TODO bga

            screen.flip();
        }
    }

    //------------------------------------------------------------------------
    // driver

    pub fn play(opts: ~Options) {
        let r = rand::task_rng();
        let mut bms: ~Bms = match parse_bms(opts.bmspath, r) {
            Ok(bms) => ~bms,
            Err(err) => die!("Couldn't load BMS file: %s", err)
        };
        sanitize_bms(bms);

        let keyspec: ~KeySpec = match key_spec(bms, opts) {
            Ok(keyspec) => keyspec,
            Err(err) => die!("%s", err)
        };
        compact_bms(bms, keyspec);
        let infos: ~BmsInfo = ~analyze_bms(bms);

        for opts.modf.each |&modf| {
            apply_modf(bms, modf, r, keyspec, 0, keyspec.split);
            if keyspec.split < keyspec.order.len() {
                apply_modf(bms, modf, r, keyspec,
                           keyspec.split, keyspec.order.len());
            }
        }

        let (port, chan) = comm::stream();
        chan.send(~(opts, bms, infos, keyspec));

        do start {
            let ~(opts, bms, infos, keyspec) = port.recv();

            init_sdl();
            for opts.joystick.each |&joyidx| { init_joystick(joyidx); }

            // this has to be done after SDL initialization.
            let keymap = ~read_keymap(keyspec, os::getenv);

            let mut font = ~Font();
            font.create_zoomed_font(1);
            font.create_zoomed_font(2);
            let font = font;

            let mut screen = None;
            if opts.has_screen() {
                screen = Some(init_video(opts.is_exclusive(),
                                         opts.fullscreen));
            }

            // show stagefile
            let mut saved_screen = None;
            if !opts.is_exclusive() {
                let screen: &Surface = *screen.get_ref();
                show_stagefile_screen(bms, infos, keyspec, opts,
                                      screen, font);
                let surface = new_surface(SCREENW, 20);
                surface.blit_area(screen,
                                  (0,SCREENH-20), (0,0), (SCREENW,20));
                saved_screen = Some(surface);
            } else if opts.showinfo {
                show_stagefile_noscreen(bms, infos, keyspec, opts);
            }

            // wait for resources
            let atexit = || {
                if opts.is_exclusive() { update_line(~""); }
            };
            let mut lastinfo = None;
            // (C: `resource_loaded`)
            let update_status = |path: Option<~str>| {
                let now = ticks();
                static INFO_INTERVAL: uint = 47;
                if opts.showinfo && lastinfo.map_default(true,
                                        |&t| now - t >= INFO_INTERVAL) {
                    lastinfo = Some(now);
                    match saved_screen {
                        Some(ref saved) => {
                            let screen: &Surface = *screen.get_ref();
                            let msg = path.get_or_default(~"loading...");
                            screen.blit_at(*saved, 0, (SCREENH-20) as i16);
                            do screen.with_pixels |pixels| {
                                font.print_string(
                                    pixels, SCREENW-3, SCREENH-18,
                                    1, RightAligned, msg,
                                    Gradient(RGB(0x80,0x80,0x80),
                                             RGB(0xc0,0xc0,0xc0)));
                            }
                            screen.flip();
                        }
                        None => {
                            match path {
                                Some(path) => {
                                    let path =
                                        if path.len() > 63 {path}
                                        else {path.slice(0, 63).to_owned()};
                                    update_line(~"Loading: " + path);
                                }
                                None => update_line(~"Loading done.")
                            }
                        }
                    }
                }
                check_exit(atexit);
            };

            let start = ticks() + 3000;
            let (sndres, imgres) = load_resource(bms, opts, update_status);
            if opts.showinfo {
                lastinfo = None; // force update
                let _ = lastinfo; // Rust: avoids incorrect warning. (#3796)
                update_status(None);
            }
            while ticks() < start { check_exit(atexit); }

            // TODO sound_length
            let duration = bms_duration(bms, infos.originoffset, |_| 0.0);
            let mut player = Player(opts, bms, infos, duration,
                                    keyspec, keymap);
            let display =
                match screen {
                    Some(screen) =>
                        @mut GraphicDisplay(player.opts, player.keyspec,
                                            screen, font),
                    None => fail!(~"TODO")
                };

            while player.tick() {
                display.render(&player);
            }

            atexit();
        }
    }

    //------------------------------------------------------------------------

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
    util::exit(1)
}

/// The entry point. Parses the command line options and delegates other
/// things to `play`. (C: `main`)
fn main() {
    use player::*;

    let longargs = util::hashmap::map_from_vec([
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

    let mut bmspath = None;
    let mut mode = PlayMode;
    let mut modf = None;
    let mut bga = BgaAndMovie;
    let mut showinfo = true;
    let mut fullscreen = true;
    let mut joystick = None;
    let mut preset = None;
    let mut leftkeys = None;
    let mut rightkeys = None;
    let mut playspeed = 1.0;

    let mut i = 1;
    while i < nargs {
        if !args[i].starts_with("-") {
            if bmspath.is_none() { bmspath = Some(copy args[i]); }
        } else if args[i] == ~"--" {
            i += 1;
            if bmspath.is_none() && i < nargs {
                bmspath = Some(copy args[i]);
            }
            break;
        } else {
            let shortargs =
                if args[i].starts_with("--") {
                    match longargs.find(&args[i]) {
                        Some(&c) => str::from_char(c),
                        None => die!("Invalid option: %s", args[i])
                    }
                } else {
                    args[i].slice_to_end(1).to_owned()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for shortargs.each_chari_byte |j, c| {
                let fetch_arg = |opt| {
                    let off = if inside {j+1} else {j};
                    let nextarg =
                        if inside && off < nshortargs {
                            shortargs.slice_to_end(off)
                        } else {
                            i += 1;
                            if i < nargs {
                                let arg: &str = args[i];
                                arg
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
                    'v' => { mode = AutoPlayMode; },
                    'x' => { mode = ExclusiveMode; },
                    'X' => { mode = ExclusiveMode; bga = NoBga; },
                    'w' => { fullscreen = false; },
                    'q' => { showinfo = false; },
                    'm' => { modf = Some(MirrorModf); },
                    's' => { modf = Some(ShuffleModf); },
                    'S' => { modf = Some(ShuffleExModf); },
                    'r' => { modf = Some(RandomModf); },
                    'R' => { modf = Some(RandomExModf); },
                    'k' => { preset = Some(fetch_arg('k').to_owned()); },
                    'K' => {
                        leftkeys = Some(fetch_arg('K').to_owned());
                        rightkeys = Some(fetch_arg('K').to_owned());
                    },
                    'a' => match float::from_str(fetch_arg('a')) {
                        Some(speed) if speed > 0.0 => {
                            playspeed =
                                if speed < 0.1 {0.1}
                                else if speed > 99.0 {99.0}
                                else {speed};
                        },
                        _ => die!("Invalid argument to option -a")
                    },
                    'B' => { bga = NoBga; },
                    'M' => { bga = BgaButNoMovie; },
                    'j' => match uint::from_str(fetch_arg('j')) {
                        Some(n) => { joystick = Some(n); },
                        _ => die!("Invalid argument to option -j")
                    },
                    ' ' => {}, // for ignored long options
                    '1'..'9' => {
                        playspeed = char::to_digit(c, 10).get() as float;
                    },
                    _ => die!("Invalid option: -%c", c),
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    //if bmspath.is_none() { bmspath = filedialog(); }
    match bmspath {
        None => usage(),
        Some(bmspath) => {
            let opts = ~Options {
                bmspath: bmspath, mode: mode, modf: modf, bga: bga,
                showinfo: showinfo, fullscreen: fullscreen,
                joystick: joystick, preset: preset, leftkeys: leftkeys,
                rightkeys: rightkeys, playspeed: playspeed
            };
            play(opts);
        }
    }
}
