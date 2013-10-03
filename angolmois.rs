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
 * This is a direct, one-to-one translation of Angolmois to Rust programming language.
 * [Angolmois](http://mearie.org/projects/angolmois/) is
 * a [BM98](http://bm98.yaneu.com/bm98/)-like minimalistic music video game which supports
 * the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.
 * 
 * Angolmois is a combination of string parsing, path manipulation, two-dimensional graphics and
 * complex game play carefully packed into some thousand lines of code. This translation is intended
 * to provide an example of translating a moderately-sized C code to Rust, and also to investigate
 * additional library supports required for such moderately-sized programs.
 * 
 * Angolmois is distributed under GNU GPL version 2+, so is this translation. The portions of it is
 * intended to be sent as a patch to Rust, so those portions are licensed under Apache License 2.0
 * and MIT license. Also note that:
 *
 * - This code is known to compile with the following combinations of rustc and rust-sdl:
 *     - rustc 0.7 + rust-sdl `48cb490` 2013-07-02 (an unmerged branch from rossmurray/rust-sdl)
 *
 * - Unlike the original Angolmois code (which sacrifices most comments due to code size concerns),
 *   the Rust version has much more comments which can be beneficial for understanding Angolmois
 *   itself too.
 *
 * # Key
 * 
 * The following notations are used in the comments and documentations.
 * 
 * * (C: ...) - variable/function corresponds to given name in the C code.
 * * Rust: ... - suboptimal translation with a room for improvement in Rust. often contains a Rust
 *   issue number like #1234.
 * * XXX - should be fixed as soon as Rust issue is gone.
 * * TODO - other problems unrelated to Rust.
 *
 * # Common Issues
 *
 * Those issues are common enough that they have to be discussed before the source code.
 *
 * * #3511 - iterator needs to ensure its underlying object available but rvalue lifetime is too
 *           short for it. rooting the underlying object is necessary for now.
 * * #7363 - implicit borrowing of stack closures is currently disabled due to the soundness issue.
 *           can be worked around by wrapping a reference to the closure to another closure.
 */

#[link(name = "angolmois",
       vers = "2.0.0-alpha2",
       uuid = "0E85EA95-BE62-4E0F-B811-8C1EC46C46EC",
       url = "https://github.com/lifthrasiir/angolmois-rust/")];

#[comment = "Angolmois"];
#[license = "GPLv2+"];

extern mod extra;
extern mod sdl;

use std::{char, str};
use std::num::Round;

// see below for specifics.
use self::util::str::*;
use self::util::option::*;
use self::util::io::*;

/// Returns a version string. (C: `VERSION`)
pub fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

//==================================================================================================
// utility declarations

/// Returns an executable name used in the command line if any. (C: `argv0`)
pub fn exename() -> ~str {
    let args = std::os::args();
    if args.is_empty() {~"angolmois"} else {args[0].clone()}
}

/// Utility functions.
#[macro_escape]
pub mod util {
    /**
     * String utilities for Rust. Parallels to `std::str`.
     *
     * NOTE: Some of these additions will be eventually sent to `libstd/str.rs` and are not subject
     * to the above copyright notice.
     */
    pub mod str {
        use std::str::*;

        static tag_cont_u8: u8 = 128u8; // copied from libstd/str.rs

        /// Given a potentially invalid UTF-8 byte sequence, fixes an invalid UTF-8 sequence with
        /// given error handler.
        pub fn fix_utf8(v: &[u8], handler: &fn(&[u8]) -> ~[u8]) -> ~[u8] {
            let mut i = 0u;
            let total = v.len();
            let mut result = ~[];
            while i < total {
                let chend = i + utf8_char_width(v[i]);
                let mut j = i + 1u;
                while j < total && j < chend && v[j] & 192u8 == tag_cont_u8 {
                    j += 1u;
                }
                if j == chend {
                    assert!(i != chend);
                    result.push_all(v.slice(i, j));
                } else {
                    result.push_all(handler(v.slice(i, j)));
                }
                i = j;
            }
            result
        }

        /// Converts a vector of bytes to a UTF-8 string. Any invalid UTF-8 sequences are fixed with
        /// given error handler.
        pub fn from_fixed_utf8_bytes(v: &[u8], handler: &fn(&[u8]) -> ~str) -> ~str {
            let newhandler: &fn(&[u8]) -> ~[u8] = |v: &[u8]| -> ~[u8] {
                let ret = handler(v);
                ret.as_bytes().to_owned()
            };
            let bytes = fix_utf8(v, newhandler);
            unsafe { raw::from_utf8(bytes) }
        }

        /// Extensions to `str`.
        pub trait StrUtil<'self> {
            /// Returns a slice of the given string starting from `begin` and up to the byte
            /// position `end`. `end` doesn't have to point to valid characters.
            ///
            /// # Failure
            ///
            /// If `begin` does not point to valid characters or beyond the last character of
            /// the string, or `end` points beyond the last character of the string
            fn slice_upto(&self, begin: uint, end: uint) -> &'self str;

            /// Given a potentially invalid UTF-8 string, fixes an invalid UTF-8 string with given
            /// error handler.
            fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

            /// Counts the number of bytes in the complete UTF-8 sequences up to `limit` bytes
            /// in `s` starting from `start`.
            fn count_bytes_upto(&self, start: uint, limit: uint) -> uint;

            /// Returns a length of the longest prefix of given string, which `uint::from_str`
            /// accepts without a failure, if any.
            //
            // Rust: actually, it is better to have `{uint,int,float}::from_str` returning a tuple.
            fn scan_uint(&self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which `int::from_str`
            /// accepts without a failure, if any.
            fn scan_int(&self) -> Option<uint>;

            /// Returns a length of the longest prefix of given string, which `float::from_str`
            /// accepts without a failure, if any.
            fn scan_float(&self) -> Option<uint>;

            /// Converts all ASCII letters (A-Z/a-z, no accent) to uppercase.
            fn to_ascii_upper(&self) -> ~str;

            /// Converts all ASCII letters (A-Z/a-z, no accent) to lowercase.
            fn to_ascii_lower(&self) -> ~str;

            /// Work with a null-terminated UTF-16 buffer of the string. Useful for calling
            /// Win32 API.
            fn as_utf16_c_str<T>(&self, f: &fn(*u16) -> T) -> T;
        }

        impl<'self> StrUtil<'self> for &'self str {
            fn slice_upto(&self, begin: uint, end: uint) -> &'self str {
                self.slice(begin, begin + self.count_bytes_upto(begin, end))
            }

            fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
                from_fixed_utf8_bytes(self.as_bytes(), handler)
            }

            fn count_bytes_upto(&self, start: uint, limit: uint) -> uint {
                assert!(self.is_char_boundary(start));
                let limit = start + limit;
                let l = self.len();
                assert!(limit < l);
                let mut end = start;
                loop {
                    assert!(end < l);
                    let next = self.char_range_at(end).next;
                    if next > limit { break; }
                    end = next;
                }
                end - start
            }

            fn scan_uint(&self) -> Option<uint> {
                match self.find(|c| !('0' <= c && c <= '9')) {
                    Some(first) if first > 0u => Some(first),
                    None if self.len() > 0u => Some(self.len()),
                    _ => None
                }
            }

            fn scan_int(&self) -> Option<uint> {
                if self.starts_with("-") || self.starts_with("+") {
                    self.slice_from(1u).scan_uint().map(|&pos| pos + 1u)
                } else {
                    self.scan_uint()
                }
            }

            fn scan_float(&self) -> Option<uint> {
                do self.scan_int().and_then |pos| {
                    if self.len() > pos && self.char_at(pos) == '.' {
                        let pos2 = self.slice_from(pos + 1u).scan_uint();
                        pos2.map(|&pos2| pos + pos2 + 1u)
                    } else {
                        Some(pos)
                    }
                }
            }

            fn to_ascii_upper(&self) -> ~str {
                unsafe { self.to_ascii_nocheck() }.to_upper().to_str_ascii()
            }

            fn to_ascii_lower(&self) -> ~str {
                unsafe { self.to_ascii_nocheck() }.to_lower().to_str_ascii()
            }

            fn as_utf16_c_str<T>(&self, f: &fn(*u16) -> T) -> T {
                let mut s16 = self.to_utf16();
                s16.push(0u16);
                do s16.as_imm_buf |buf, _| { f(buf) }
            }
        }

        /// A trait which provides `prefix_shifted` method. Similar to `str::starts_with`, but with
        /// swapped `self` and argument.
        pub trait ShiftablePrefix {
            /// Returns a slice of given string with `self` at the start of the string stripped only
            /// once, if any.
            fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str>;
        }

        impl ShiftablePrefix for char {
            fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
                if !s.is_empty() {
                    let CharRange {ch, next} = s.char_range_at(0u);
                    if ch == *self {
                        return Some(s.slice_from(next));
                    }
                }
                None
            }
        }

        impl<'self> ShiftablePrefix for &'self str {
            fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
                if s.starts_with(*self) {
                    Some(s.slice_from(self.len()))
                } else {
                    None
                }
            }
        }

    }

    /**
     * Option utilities for Rust. Parallels to `std::option`.
     *
     * NOTE: Some of these additions will be eventually sent to `libstd/option.rs` and are not
     * subject to the above copyright notice.
     */
    pub mod option {

        /// Filters the value inside the option using the function. Returns `None` if the original
        /// option didn't contain a value.
        #[inline(always)]
        pub fn filter<T:Clone>(opt: Option<T>, f: &fn(t: T) -> bool) -> Option<T> {
            match opt {
                Some(t) => if f(t.clone()) {Some(t)} else {None},
                None => None
            }
        }

        /// Merges two options. When one of options is `None` returns the other option. When both
        /// options contain a value the function is called to get the merged value.
        #[inline(always)]
        pub fn merge<T:Clone>(lhs: Option<T>, rhs: Option<T>, f: &fn(T, T) -> T) -> Option<T> {
            match (lhs, rhs) {
                (None, None) => None,
                (lhs,  None) => lhs,
                (None, rhs ) => rhs,
                (Some(ref lhs), Some(ref rhs)) => Some(f(lhs.clone(), rhs.clone()))
            }
        }

        pub trait CopyableOptionUtil<T:Clone> {
            /// Filters the value inside the option using the function. Returns `None` if
            /// the original option didn't contain a value.
            fn filter(self, f: &fn(x: T) -> bool) -> Option<T>;

            /// Merges two options. When one of options is `None` returns the other option. When
            /// both options contain a value the function is called to get the merged value.
            fn merge(self, other: Option<T>, f: &fn(T, T) -> T) -> Option<T>;
        }

        impl<T:Clone> CopyableOptionUtil<T> for Option<T> {
            #[inline(always)]
            fn filter(self, f: &fn(x: T) -> bool) -> Option<T> {
                filter(self, f)
            }

            #[inline(always)]
            fn merge(self, other: Option<T>, f: &fn(T, T) -> T) -> Option<T> {
                merge(self, other, f)
            }
        }
    }

    /**
     * I/O utilities for Rust. Parallels to `std::io`.
     *
     * NOTE: Some of these additions will be eventually sent to `libstd/io.rs` and are not subject
     * to the above copyright notice.
     */
    pub mod io {

        /// Extensions to `ReaderUtil`.
        pub trait ReaderUtilEx {
            /// Reads up until the first '\n' char (which is not returned), or EOF. Any invalid
            /// UTF-8 sequences are fixed with given error handler.
            fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

            /// Iterates over every line until the iterator breaks or EOF. Any invalid UTF-8
            /// sequences are fixed with given error handler.
            fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str, it: &fn(&str) -> bool);
        }

        impl<T: Reader> ReaderUtilEx for T {
            fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
                let mut bytes = ~[];
                loop {
                    let ch = self.read_byte();
                    if ch == -1 || ch == 10 { break; }
                    bytes.push(ch as u8);
                }
                ::util::str::from_fixed_utf8_bytes(bytes, handler)
            }

            fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str, it: &fn(&str) -> bool) {
                while !self.eof() {
                    if !it(self.read_and_fix_utf8_line(|buf| handler(buf))) { break; } // XXX #7363
                }
            }
        }

    }

    /**
     * Extensions to rust-sdl. This comprises of additional bindings for SDL_mixer and a minimal
     * but functional binding for SMPEG.
     *
     * NOTE: Some of these additions will be eventually sent to rust-sdl and are not subject to
     * the above copyright notice.
     */
    pub mod sdl {
        pub mod mixer {
            use std::libc::c_int;

            pub mod ll {
                use std::libc::c_int;
                extern {
                    pub fn Mix_Volume(channel: c_int, volume: c_int) -> c_int;
                    pub fn Mix_ReserveChannels(num: c_int) -> c_int;
                    pub fn Mix_GroupChannel(which: c_int, tag: c_int) -> c_int;
                    pub fn Mix_GroupNewer(tag: c_int) -> c_int;
                }
            }

            pub fn num_playing(channel: Option<c_int>) -> c_int {
                #[fixed_stack_segment]; #[inline(never)];
                use sdl::mixer;
                unsafe {
                    match channel {
                        Some(channel) => mixer::ll::Mix_Playing(channel),
                        None => mixer::ll::Mix_Playing(-1)
                    }
                }
            }

            pub fn get_channel_volume(channel: Option<c_int>) -> c_int {
                #[fixed_stack_segment]; #[inline(never)];
                unsafe {
                    let ll_channel = channel.unwrap_or(-1);
                    ll::Mix_Volume(ll_channel, -1)
                }
            }

            pub fn set_channel_volume(channel: Option<c_int>, volume: c_int) {
                #[fixed_stack_segment]; #[inline(never)];
                unsafe {
                    let ll_channel = channel.unwrap_or(-1);
                    ll::Mix_Volume(ll_channel, volume);
                }
            }

            pub fn reserve_channels(num: c_int) -> c_int {
                #[fixed_stack_segment]; #[inline(never)];
                unsafe { ll::Mix_ReserveChannels(num) }
            }

            pub fn group_channel(which: Option<c_int>, tag: Option<c_int>) -> bool {
                #[fixed_stack_segment]; #[inline(never)];
                unsafe {
                    let ll_which = which.unwrap_or(-1);
                    let ll_tag = tag.unwrap_or(-1);
                    ll::Mix_GroupChannel(ll_which, ll_tag) != 0
                }
            }

            pub fn newest_in_group(tag: Option<c_int>) -> Option<c_int> {
                #[fixed_stack_segment]; #[inline(never)];
                unsafe {
                    let ll_tag = tag.unwrap_or(-1);
                    let channel = ll::Mix_GroupNewer(ll_tag);
                    if channel == -1 {None} else {Some(channel)}
                }
            }
        }

        pub mod mpeg {
            use std::libc::{c_int, c_float};
            use std::ptr::null;
            use sdl::video::Surface;
            use self::ll::SMPEGstatus;

            pub mod ll {
                use std::libc::{c_void, c_int, c_char, c_float, c_double};
                use sdl::video::ll::{SDL_RWops, SDL_Surface};
                use sdl::audio::ll::SDL_AudioSpec;
                pub struct SMPEG { priv opaque: () }
                pub struct SMPEG_Info {
                    has_audio: c_int,
                    has_video: c_int,
                    width: c_int,
                    height: c_int,
                    current_frame: c_int,
                    current_fps: c_double,
                    audio_string: [c_char, ..80],
                    audio_current_frame: c_int,
                    current_offset: u32,
                    total_size: u32,
                    current_time: c_double,
                    total_time: c_double
                }
                pub enum SMPEGstatus {
                    SMPEG_ERROR = -1,
                    SMPEG_STOPPED = 0,
                    SMPEG_PLAYING =1
                }
                #[link_args = "-lsmpeg"]
                extern {
                    pub fn SMPEG_new(file: *c_char, info: *SMPEG_Info,
                                     sdl_audio: c_int) -> *SMPEG;
                    pub fn SMPEG_new_descr(file: c_int, info: *SMPEG_Info,
                                           sdl_audio: c_int) -> *SMPEG;
                    pub fn SMPEG_new_data(data: *c_void, size: c_int, info: *SMPEG_Info,
                                          sdl_audio: c_int) -> *SMPEG;
                    pub fn SMPEG_new_rwops(src: *SDL_RWops, info: *SMPEG_Info,
                                           sdl_audio: c_int) -> *SMPEG;
                    pub fn SMPEG_getinfo(mpeg: *SMPEG, info: *SMPEG_Info);
                    pub fn SMPEG_enableaudio(mpeg: *SMPEG, enable: c_int);
                    pub fn SMPEG_enablevideo(mpeg: *SMPEG, enable: c_int);
                    pub fn SMPEG_delete(mpeg: *SMPEG);
                    pub fn SMPEG_status(mpeg: *SMPEG) -> SMPEGstatus;
                    pub fn SMPEG_setvolume(mpeg: *SMPEG, volume: c_int);
                    // XXX SDL_Mutex and SMPEG_DisplayCallback unimplemented
                    pub fn SMPEG_setdisplay(mpeg: *SMPEG, dst: *SDL_Surface,
                                            surfLock: *c_void, callback: *c_void);
                    pub fn SMPEG_loop(mpeg: *SMPEG, repeat: c_int);
                    pub fn SMPEG_scaleXY(mpeg: *SMPEG, width: c_int, height: c_int);
                    pub fn SMPEG_scale(mpeg: *SMPEG, scale: c_int);
                    pub fn SMPEG_move(mpeg: *SMPEG, x: c_int, y: c_int);
                    pub fn SMPEG_setdisplayregion(mpeg: *SMPEG, x: c_int, y: c_int,
                                                  w: c_int, h: c_int);
                    pub fn SMPEG_play(mpeg: *SMPEG);
                    pub fn SMPEG_pause(mpeg: *SMPEG);
                    pub fn SMPEG_stop(mpeg: *SMPEG);
                    pub fn SMPEG_rewind(mpeg: *SMPEG);
                    pub fn SMPEG_seek(mpeg: *SMPEG, bytes: c_int);
                    pub fn SMPEG_skip(mpeg: *SMPEG, seconds: c_float);
                    pub fn SMPEG_renderFrame(mpeg: *SMPEG, framenum: c_int);
                    pub fn SMPEG_renderFinal(mpeg: *SMPEG, dst: *SDL_Surface, x: c_int, y: c_int);
                    // XXX SMPEG_Filter unimplemented
                    pub fn SMPEG_filter(mpeg: *SMPEG, filter: *c_void) -> *c_void;
                    pub fn SMPEG_error(mpeg: *SMPEG) -> *c_char;
                    pub fn SMPEG_playAudio(mpeg: *SMPEG, stream: *u8, len: c_int) -> c_int;
                    pub fn SMPEG_playAudioSDL(mpeg: *c_void, stream: *u8, len: c_int) -> c_int;
                    pub fn SMPEG_wantedSpec(mpeg: *SMPEG, wanted: *SDL_AudioSpec) -> c_int;
                    pub fn SMPEG_actualSpec(mpeg: *SMPEG, spec: *SDL_AudioSpec);
                }
            }

            pub struct MPEG {
                raw: *ll::SMPEG
            }

            fn wrap_mpeg(raw: *ll::SMPEG) -> ~MPEG {
                ~MPEG { raw: raw }
            }

            impl Drop for MPEG {
                fn drop(&mut self) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_delete(self.raw); }
                }
            }

            impl MPEG {
                pub fn from_path(path: &Path) -> Result<~MPEG, ~str> {
                    #[fixed_stack_segment]; #[inline(never)];
                    let raw = unsafe {
                        do path.to_c_str().with_ref |buf| {
                            ll::SMPEG_new(buf, null(), 0)
                        }
                    };

                    if raw.is_null() { Err(::sdl::get_error()) }
                    else { Ok(wrap_mpeg(raw)) }
                }

                pub fn status(&self) -> SMPEGstatus {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_status(self.raw) }
                }

                pub fn set_volume(&self, volume: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_setvolume(self.raw, volume as c_int); }
                }

                pub fn set_display(&self, surface: &Surface) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe {
                        ll::SMPEG_setdisplay(self.raw, surface.raw, null(), null());
                    }
                }

                pub fn enable_video(&self, enable: bool) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_enablevideo(self.raw, enable as c_int); }
                }

                pub fn enable_audio(&self, enable: bool) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_enableaudio(self.raw, enable as c_int); }
                }

                pub fn set_loop(&self, repeat: bool) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_loop(self.raw, repeat as c_int); }
                }

                pub fn resize(&self, width: int, height: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_scaleXY(self.raw, width as c_int, height as c_int); }
                }

                pub fn scale_by(&self, scale: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_scale(self.raw, scale as c_int); }
                }

                pub fn move(&self, x: int, y: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_move(self.raw, x as c_int, y as c_int); }
                }

                pub fn set_display_region(&self, x: int, y: int, w: int, h: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe {
                        ll::SMPEG_setdisplayregion(self.raw, x as c_int, y as c_int,
                                                   w as c_int, h as c_int);
                    }
                }

                pub fn play(&self) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_play(self.raw); }
                }

                pub fn pause(&self) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_pause(self.raw); }
                }

                pub fn stop(&self) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_stop(self.raw); }
                }

                pub fn rewind(&self) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_rewind(self.raw); }
                }

                pub fn seek(&self, bytes: int) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_seek(self.raw, bytes as c_int); }
                }

                pub fn skip(&self, seconds: float) {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe { ll::SMPEG_skip(self.raw, seconds as c_float); }
                }

                pub fn get_error(&self) -> ~str {
                    #[fixed_stack_segment]; #[inline(never)];
                    unsafe {
                        let cstr = ll::SMPEG_error(self.raw);
                        ::str::raw::from_c_str(::std::cast::transmute(&cstr))
                    }
                }
            }
        }
    }

    /// Win32 API wrappers.
    #[cfg(target_os = "win32")]
    pub mod win32 {
        pub mod ll {
            use std::libc::{c_int, c_uint, c_void};
            use std::libc::{BOOL, CHAR, WORD, DWORD, HANDLE, LPCSTR, LPWSTR, LPCWSTR};

            pub type HWND = HANDLE;
            pub type HINSTANCE = HANDLE;

            pub static OFN_HIDEREADONLY: DWORD = 4;

            pub struct OPENFILENAMEW {
                lStructSize: DWORD,
                hwndOwner: HWND,
                hInstance: HINSTANCE,
                lpstrFilter: LPCWSTR,
                lpstrCustomFilter: LPWSTR,
                nMaxCustFilter: DWORD,
                nFilterIndex: DWORD,
                lpstrFile: LPWSTR,
                nMaxFile: DWORD,
                lpstrFileTitle: LPWSTR,
                nMaxFileTitle: DWORD,
                lpstrInitialDir: LPCWSTR,
                lpstrTitle: LPCWSTR,
                Flags: DWORD,
                nFileOffset: WORD,
                nFileExtension: WORD,
                lpstrDefExt: LPCWSTR,
                lCustData: DWORD,
                lpfnHook: *(), // XXX LPOFNHOOKPROC = fn(HWND,c_uint,WPARAM,LPARAM)->c_uint
                lpTemplateName: LPCWSTR,
                pvReserved: *c_void,
                dwReserved: DWORD,
                FlagsEx: DWORD,
            }

            pub struct FILETIME {
                dwLowDateTime: DWORD,
                dwHighDateTime: DWORD,
            }

            pub struct WIN32_FIND_DATAA {
                dwFileAttributes: DWORD,
                ftCreationTime: FILETIME,
                ftLastAccessTime: FILETIME,
                ftLastWriteTime: FILETIME,
                nFileSizeHigh: DWORD,
                nFileSizeLow: DWORD,
                dwReserved0: DWORD,
                dwReserved1: DWORD,
                cFileName: [CHAR, ..260],
            }

            #[link_args = "-lkernel32"]
            #[abi = "stdcall"]
            extern "stdcall" {
                pub fn FindFirstFileA(lpFileName: LPCSTR,
                                      lpFindFileData: *WIN32_FIND_DATAA) -> HANDLE;
                pub fn FindNextFileA(hFindFile: HANDLE, lpFindFileData: *WIN32_FIND_DATAA) -> BOOL;
                pub fn FindClose(hFindFile: HANDLE) -> BOOL;
            }

            #[link_args = "-luser32"]
            #[abi = "stdcall"]
            extern "stdcall" {
                pub fn MessageBoxW(hWnd: HWND, lpText: LPCWSTR, lpCaption: LPCWSTR,
                                   uType: c_uint) -> c_int;
            }

            #[link_args = "-lcomdlg32"]
            #[abi = "stdcall"]
            extern "stdcall" {
                pub fn GetOpenFileNameW(lpofn: *OPENFILENAMEW) -> BOOL;
            }
        }
    }

    /// Immediately terminates the program with given exit code.
    pub fn exit(exitcode: int) -> ! {
        #[fixed_stack_segment];
        // Rust: `std::os::set_exit_status` doesn't immediately terminate the program.
        unsafe { ::std::libc::exit(exitcode as ::std::libc::c_int); }
    }

    /// Exits with an error message. Internally used in the `die!` macro below.
    #[cfg(target_os = "win32")]
    pub fn die(s: ~str) -> ! {
        #[fixed_stack_segment];
        use util::str::StrUtil;
        do ::exename().as_utf16_c_str() |caption| {
            do s.as_utf16_c_str() |text| {
                unsafe { win32::ll::MessageBoxW(::std::ptr::mut_null(), text, caption, 0); }
            }
        }
        exit(1)
    }

    /// Exits with an error message. Internally used in the `die!` macro below.
    #[cfg(not(target_os = "win32"))]
    pub fn die(s: ~str) -> ! {
        ::std::io::stderr().write_line(fmt!("%s: %s", ::exename(), s));
        exit(1)
    }

    /// Prints an warning message. Internally used in the `warn!` macro below.
    pub fn warn(s: ~str) {
        ::std::io::stderr().write_line(fmt!("*** Warning: %s", s));
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

    /// Reads a path string from the user in the platform-dependent way. Returns `None` if the user
    /// refused to do so or the platform is unsupported. (C: `filedialog`)
    #[cfg(target_os = "win32")]
    pub fn get_path_from_dialog() -> Option<~str> {
        #[fixed_stack_segment];
        use std::ptr::{null, mut_null};
        use util::str::StrUtil;

        let filter =
            "All Be-Music Source File (*.bms;*.bme;*.bml;*.pms)\x00*.bms;*.bme;*.bml;*.pms\x00\
             Be-Music Source File (*.bms)\x00*.bms\x00\
             Extended Be-Music Source File (*.bme)\x00*.bme\x00\
             Longnote Be-Music Source File (*.bml)\x00*.bml\x00\
             Po-Mu Source File (*.pms)\x00*.pms\x00\
             All Files (*.*)\x00*.*\x00";
        do filter.as_utf16_c_str() |filter| {
            do "Choose a file to play".as_utf16_c_str() |title| {
                let mut buf = [0u16, ..512];
                let ret = do buf.as_mut_buf |buf, bufsize| {
                    let ofnsz = ::std::sys::size_of::<win32::ll::OPENFILENAMEW>();
                    let ofn = win32::ll::OPENFILENAMEW {
                        lStructSize: ofnsz as ::std::libc::DWORD,
                        lpstrFilter: filter,
                        lpstrFile: buf,
                        nMaxFile: bufsize as ::std::libc::DWORD,
                        lpstrTitle: title,
                        Flags: win32::ll::OFN_HIDEREADONLY,

                        // zero-initialized fields
                        hwndOwner: mut_null(), hInstance: mut_null(),
                        lpstrCustomFilter: mut_null(), nMaxCustFilter: 0, nFilterIndex: 0,
                        lpstrFileTitle: mut_null(), nMaxFileTitle: 0,
                        lpstrInitialDir: null(), nFileOffset: 0, nFileExtension: 0,
                        lpstrDefExt: null(), lCustData: 0, lpfnHook: null(),
                        lpTemplateName: null(), pvReserved: null(),
                        dwReserved: 0, FlagsEx: 0,
                    };
                    unsafe {win32::ll::GetOpenFileNameW(::std::cast::transmute(&ofn))}
                };
                if ret != 0 {
                    let path: &[u16] = match buf.position_elem(&0) {
                        Some(idx) => buf.slice(0, idx),
                        // Rust: why can't we cast `&[u16, ..512]` to `&[u16]`?!
                        None => buf.slice(0, buf.len())
                    };
                    Some(::std::str::from_utf16(path))
                } else {
                    None
                }
            }
        }
    }

    /// Reads a path string from the user in the platform-dependent way. Returns `None` if the user
    /// refused to do so or the platform is unsupported. (C: `filedialog`)
    #[cfg(not(target_os = "win32"))]
    pub fn get_path_from_dialog() -> Option<~str> {
        None
    }

    /*
     * A lexer barely powerful enough to parse BMS format. Comparable to C's `sscanf`.
     *
     * `lex!(e; fmt1, fmt2, ..., fmtN)` returns an expression that evaluates to true if and only if
     * all format specification is consumed. The format specification (analogous to `sscanf`'s
     * `%`-string) is as follows:
     *
     * - `ws`: Consumes one or more whitespace. (C: `%*[ \t\r\n]` or similar)
     * - `ws*`: Consumes zero or more whitespace. (C: ` `)
     * - `int [-> e2]`: Consumes an integer and optionally saves it to `e2`. (C: `%d` and `%*d`, but
     *   does not consume preceding whitespace) The integer syntax is slightly limited compared to
     *   `sscanf`.
     * - `float [-> e2]`: Consumes a real number and optionally saves it to `e2`. (C: `%f` etc.)
     *   Again, the real number syntax is slightly limited; especially an exponent support is
     *   missing.
     * - `str [-> e2]`: Consumes a remaining input as a string and optionally saves it to `e2`.
     *   The string is at least one character long. (C: not really maps to `sscanf`, similar to
     *   `fgets`) Implies `!`. It can be followed by `ws*` which makes the string right-trimmed.
     * - `str* [-> e2]`: Same as above but the string can be empty.
     * - `char [-> e2]`: Consumes exactly one character and optionally saves it to `e2`. Resulting
     *   character can be whitespace. (C: `%1c`)
     * - `!`: Ensures that the entire string has been consumed. Should be the last format
     *   specification.
     * - `"foo"` etc.: An ordinary expression is treated as a literal string or literal character.
     *
     * For the use in Angolmois, the following specifications have been added:
     *
     * - `Key [-> e2]`: Consumes a two-letter alphanumeric key and optionally saves it to `e2`.
     *   (C: `%2[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ]` etc. followed by a call to `key2index`)
     * - `Measure [-> e2]`: Consumes exactly three digits and optionally saves it to `e2`.
     *   (C: `%1[0123456789]%1[0123456789]%1[0123456789]` followed by a call to `atoi`)
     */
    // Rust: - there is no `std::libc::sscanf` due to the varargs. maybe regex support will make
    //         this obsolete in the future, but not now.
    //       - multiple statements do not expand correctly. (#4375)
    //       - it is desirable to have a matcher only accepts an integer literal or string literal,
    //         not a generic expression.
    //       - no hygienic macro yet. possibly observable names from `$e` should be escaped for now.
    //       - it would be more useful to generate bindings for parsed result. this is related to
    //         many issues in general.
    macro_rules! lex(
        ($e:expr; ) => (true);
        ($e:expr; !) => ($e.is_empty());

        ($e:expr; int -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            // Rust: `std::num::from_str_bytes_common` does not recognize a number followed
            //        by garbage, so we need to parse it ourselves.
            do _line.scan_int().map_default(false) |&_endpos| {
                let _prefix = _line.slice_to(_endpos);
                do from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_from(_endpos); $($tail)*)
                }
            }
        });
        ($e:expr; uint -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do _line.scan_uint().map_default(false) |&_endpos| {
                let _prefix = _line.slice_to(_endpos);
                do from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_from(_endpos); $($tail)*)
                }
            }
        });
        ($e:expr; float -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do _line.scan_float().map_default(false) |&_endpos| {
                let _prefix = _line.slice_to(_endpos);
                do from_str(_prefix).map_default(false) |&_value| {
                    $dst = _value;
                    lex!(_line.slice_from(_endpos); $($tail)*)
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
                $dst = _line.slice_from(0); // Rust: why we need to reborrow `_line` here?!
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
            $dst = _line.slice_from(0); // Rust: why we need to reborrow `_line` here?!
            lex!(""; $($tail)*) // optimization!
        });
        ($e:expr; char -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() {
                let _range = _line.char_range_at(0);
                $dst = _range.ch;
                lex!(_line.slice_from(_range.next); $($tail)*)
            } else {
                false
            }
        });
        // start Angolmois-specific
        ($e:expr; Key -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            do key2index_str(_line).map_default(false) |&_value| {
                $dst = Key(_value);
                lex!(_line.slice_from(2u); $($tail)*)
            }
        });
        ($e:expr; Measure -> $dst:expr, $($tail:tt)*) => ({
            let _line: &str = $e;
            let _isdigit = |c| { '0' <= c && c <= '9' };
            // Rust: this is plain annoying.
            if _line.len() >= 3 && _isdigit(_line.char_at(0)) && _isdigit(_line.char_at(1)) &&
                    _isdigit(_line.char_at(2)) {
                $dst = from_str(_line.slice_to(3u)).unwrap();
                lex!(_line.slice_from(3u); $($tail)*)
            } else {
                false
            }
        });
        // end Angolmois-specific

        ($e:expr; ws, $($tail:tt)*) => ({
            let _line: &str = $e;
            if !_line.is_empty() && _line.char_at(0).is_whitespace() {
                lex!(_line.trim_left(); $($tail)*)
            } else {
                false
            }
        });
        ($e:expr; ws*, $($tail:tt)*) => ({
            let _line: &str = $e;
            lex!(_line.trim_left(); $($tail)*)
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
            do $lit.prefix_shifted($e).map_default(false) |_line| {
                lex!(*_line; $($tail)*)
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

//==================================================================================================
// bms parser

/**
 * BMS parser module.
 *
 * # Structure
 *
 * The BMS format is a plain text format with most directives start with optional whitespace
 * followed by `#`. Besides the metadata (title, artist etc.), a BMS file is a map from the time
 * position to various game play elements (henceforth "objects") and other object-like effects
 * including BGM and BGA changes. It also contains preprocessor directives used to randomize some or
 * all parts of the BMS file, which would only make sense in the loading time.
 *
 * The time position is a virtual time divided by an unit of (musical) measure. It is related to
 * the actual time by the current Beats Per Minute (BPM) value which can, well, also change during
 * the game play. Consequently it is convenient to refer the position in terms of measures, which
 * the BMS format does: the lines `#xxxyy:AABBCC...` indicates that the measure number `xxx`
 * contains objects or object-like effects (of the type specified by `yy`, henceforth "channels"),
 * evenly spaced throughout the measure and which data values are `AA`, `BB`, `CC` respectively.
 *
 * An alphanumeric identifier (henceforth "alphanumeric key") like `AA` or `BB` may mean that
 * the actual numeric value interpreted as base 16 or 36 (depending on the channel), or a reference
 * to other assets (e.g. `#BMPAA foo.png`) or complex values specified by other commands (e.g.
 * `#BPMBB 192.0`). In most cases, an identifier `00` indicates an absence of objects or object-like
 * effects at that position.
 *
 * More detailed information about BMS format, including surveys about how different implementations
 * (so called BMS players) react to underspecified features or edge cases, can be found at [BMS
 * command memo](http://hitkey.nekokan.dyndns.info/cmds.htm).
 */
pub mod parser {
    use std::{float, str, vec, cmp, iter};
    use std::rand::*;
    use util::str::StrUtil;

    //----------------------------------------------------------------------------------------------
    // alphanumeric key

    /// Two-letter alphanumeric identifier used for virtually everything, including resource
    /// management, variable BPM and chart specification.
    #[deriving(Eq,Clone)]
    pub struct Key(int);

    /// The number of all possible alphanumeric keys. (C: `MAXKEY`)
    pub static MAXKEY: int = 36*36;

    impl Key {
        /// Returns if the alphanumeric key is in the proper range. Angolmois supports the full
        /// range of 00-ZZ (0-1295) for every case.
        pub fn is_valid(self) -> bool {
            0 <= *self && *self < MAXKEY
        }

        /// Re-reads the alphanumeric key as a hexadecimal number if possible. This is required
        /// due to handling of channel #03 (BPM is expected to be in hexadecimal).
        pub fn to_hex(self) -> Option<int> {
            let sixteens = *self / 36;
            let ones = *self % 36;
            if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
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
        /// Returns a two-letter representation of alphanumeric key. (C: `TO_KEY`)
        fn to_str(&self) -> ~str {
            assert!(self.is_valid());
            let map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            fmt!("%c%c", map[**self / 36] as char, map[**self % 36] as char)
        }
    }

    //----------------------------------------------------------------------------------------------
    // lane and key kinds

    /// A game play element mapped to the single input element (for example, button) and the screen
    /// area (henceforth "lane").
    #[deriving(Eq,Clone)]
    pub struct Lane(uint);

    /// The maximum number of lanes. (C: `NNOTECHANS`)
    pub static NLANES: uint = 72;

    impl Lane {
        /// Converts the channel number to the lane number.
        pub fn from_channel(chan: Key) -> Lane {
            let player = match *chan / 36 {
                1 | 3 | 5 | 0xD => 0,
                2 | 4 | 6 | 0xE => 1,
                _ => fail!(~"non-object channel")
            };
            Lane(player * 36 + *chan as uint % 36)
        }
    }

    /**
     * Key kinds. They define an appearance of particular lane, but otherwise ignored for the game
     * play. Angolmois supports several key kinds in order to cover many potential uses.
     * (C: `KEYKIND_MNEMONICS`)
     *
     * # Defaults
     *
     * For BMS/BME, channels #11/13/15/19 and #21/23/25/29 use `WhiteKey`, #12/14/18 and #22/24/28
     * use `BlackKey`, #16 and #26 use `Scratch`, #17 and #27 use `FootPedal`.
     *
     * For PMS, channels #11/17/25 use `Button1`, #12/16/24 use `Button2`, #13/19/23 use `Button3`,
     * #14/18/22 use `Button4`, #15 uses `Button5`.
     */
    #[deriving(Eq)]
    pub enum KeyKind {
        /// White key, which mimics a real white key in the musical keyboard.
        WhiteKey,
        /// White key, but rendered yellow. This is used for simulating the O2Jam interface which
        /// has one yellow lane (mapped to spacebar) in middle of six other lanes (mapped to normal
        /// keys).
        WhiteKeyAlt,
        /// Black key, which mimics a real black key in the keyboard but rendered light blue as in
        /// Beatmania and other games.
        BlackKey,
        /// Scratch, rendered red. Scratch lane is wider than other "keys" and normally doesn't
        /// count as a key.
        Scratch,
        /// Foot pedal, rendered green. Otherwise has the same properties as scratch. The choice of
        /// color follows that of EZ2DJ, one of the first games that used this game element.
        FootPedal,
        /// White button. This and following "buttons" come from Pop'n Music, which has nine colored
        /// buttons. (White buttons constitute 1st and 9th of Pop'n Music buttons.) The "buttons"
        /// are wider than aforementioned "keys" but narrower than scratch and foot pedal.
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

    impl KeyKind {
        /// Returns a list of all supported key kinds.
        //
        // Rust: can this method be generated on the fly?
        pub fn all() -> &'static [KeyKind] {
            &[WhiteKey, WhiteKeyAlt, BlackKey, Scratch, FootPedal,
              Button1, Button2, Button3, Button4, Button5]
        }

        /// Converts a mnemonic character to an appropriate key kind. Used for parsing a key
        /// specification (see also `KeySpec`).
        pub fn from_char(c: char) -> Option<KeyKind> {
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

        /// Converts an appropriate key kind to a mnemonic character. Used for environment variables
        /// (see also `read_keymap`).
        pub fn to_char(self) -> char {
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
         * This affects the number of keys displayed in the loading screen, and reflects a common
         * practice of counting "keys" in many games (e.g. Beatmania IIDX has 8 lanes including one
         * scratch but commonly said to have 7 "keys").
         */
        pub fn counts_as_key(self) -> bool {
            self != Scratch && self != FootPedal
        }
    }

    //----------------------------------------------------------------------------------------------
    // object parameters

    /// Sound reference.
    #[deriving(Eq,Clone)]
    pub struct SoundRef(Key);

    /// Image reference.
    #[deriving(Eq,Clone)]
    pub struct ImageRef(Key);

    /// BGA layers. (C: `enum BGA_type`)
    #[deriving(Eq,Clone)]
    pub enum BGALayer {
        /// The lowest layer. BMS channel #04. (C: `BGA_LAYER`)
        Layer1 = 0,
        /// The middle layer. BMS channel #07. (C: `BGA2_LAYER`)
        Layer2 = 1,
        /// The highest layer. BMS channel #0A. (C: `BGA3_LAYER`)
        Layer3 = 2,
        /// The layer only displayed shortly after the MISS grade. It is technically not over
        /// `Layer3`, but several extensions to BMS assumes it. BMS channel #06.
        /// (C: `POORBGA_LAYER`)
        PoorBGA = 3
    }

    /// The number of BGA layers.
    pub static NLAYERS: uint = 4;

    /// Beats per minute. Used as a conversion factor between the time position and actual time
    /// in BMS.
    #[deriving(Eq,Clone)]
    pub struct BPM(float);

    impl BPM {
        /// Converts a measure to a millisecond. (C: `MEASURE_TO_MSEC`)
        pub fn measure_to_msec(self, measure: float) -> float { measure * 240000.0 / *self }

        /// Converts a millisecond to a measure. (C: `MSEC_TO_MEASURE`)
        pub fn msec_to_measure(self, msec: float) -> float { msec * *self / 240000.0 }
    }

    /// A duration from the particular point. It may be specified in measures or seconds. Used in
    /// the `Stop` object.
    #[deriving(Eq,Clone)]
    pub enum Duration { Seconds(float), Measures(float) }

    impl Duration {
        /// Calculates the actual milliseconds from the current BPM.
        pub fn to_msec(&self, bpm: BPM) -> float {
            match *self {
                Seconds(secs) => secs * 1000.0,
                Measures(measures) => bpm.measure_to_msec(measures)
            }
        }
    }

    /// A damage value upon the MISS grade. Normally it is specified in percents of the full gauge
    /// (as in `MAXGAUGE`), but sometimes it may cause an instant death. Used in the `Bomb` object
    /// (normal note objects have a fixed value).
    #[deriving(Eq,Clone)]
    pub enum Damage { GaugeDamage(float), InstantDeath }

    //----------------------------------------------------------------------------------------------
    // object

    /// A data for objects (or object-like effects). Does not include the time information.
    #[deriving(Eq,Clone)]
    pub enum ObjData {
        /// Deleted object. Only used during various processing.
        Deleted,
        /// Visible object. Sound is played when the key is input inside the associated grading
        /// area. (C: `NOTE`)
        Visible(Lane, Option<SoundRef>),
        /// Invisible object. Sound is played when the key is input inside the associated grading
        /// area. No render nor grading performed. (C: `INVNOTE`)
        Invisible(Lane, Option<SoundRef>),
        /// Start of long note (LN). Sound is played when the key is down inside the associated
        /// grading area. (C: `LNSTART`)
        LNStart(Lane, Option<SoundRef>),
        /// End of LN. Sound is played when the start of LN is graded, the key was down and now up
        /// inside the associated grading area. (C: `LNDONE`)
        LNDone(Lane, Option<SoundRef>),
        /// Bomb. Pressing the key down at the moment that the object is on time causes
        /// the specified damage; sound is played in this case. No associated grading area.
        /// (C: `BOMB`)
        Bomb(Lane, Option<SoundRef>, Damage),
        /// Plays associated sound. (C: `BGM_CHANNEL`)
        BGM(SoundRef),
        /**
         * Sets the virtual BGA layer to given image. The layer itself may not be displayed
         * depending on the current game status. (C: `BGA_CHANNEL`)
         *
         * If the reference points to a movie, the movie starts playing; if the other layer had
         * the same movie started, it rewinds to the beginning. The resulting image from the movie
         * can be shared among multiple layers.
         */
        SetBGA(BGALayer, Option<ImageRef>),
        /// Sets the BPM. Negative BPM causes the chart scrolls backwards (and implicitly signals
        /// the end of the chart). (C: `BPM_CHANNEL`)
        SetBPM(BPM),
        /// Stops the scroll of the chart for given duration ("scroll stopper" hereafter).
        /// (C: `STOP_CHANNEL`)
        Stop(Duration)
    }

    /// Query operations for objects.
    pub trait ObjQueryOps {
        /// Returns true if the object is a visible object (`Visible`). (C: `obj->type == NOTE`)
        fn is_visible(self) -> bool;
        /// Returns true if the object is an invisible object (`Invisible`).
        /// (C: `obj->type == INVNOTE`)
        fn is_invisible(self) -> bool;
        /// Returns true if the object is a start of LN object (`LNStart`).
        /// (C: `obj->type == LNSTART`)
        fn is_lnstart(self) -> bool;
        /// Returns true if the object is an end of LN object (`LNEnd`). (C: `obj->type == LNDONE`)
        fn is_lndone(self) -> bool;
        /// Returns true if the object is either a start or an end of LN object.
        /// (C: `obj->type < NOTE`)
        fn is_ln(self) -> bool;
        /// Returns true if the object is a bomb (`Bomb`). (C: `obj->type == BOMB`)
        fn is_bomb(self) -> bool;
        /// Returns true if the object is soundable when it is the closest soundable object from
        /// the current position and the player pressed the key. Named "soundable" since it may
        /// choose not to play the associated sound. Note that not every object with sound is
        /// soundable. (C: `obj->type <= INVNOTE`)
        fn is_soundable(self) -> bool;
        /// Returns true if the object is subject to grading. (C: `obj->type < INVNOTE`)
        fn is_gradable(self) -> bool;
        /// Returns true if the object has a visible representation. (C: `obj->type != INVNOTE`)
        fn is_renderable(self) -> bool;
        /// Returns true if the data is an object. (C: `IS_NOTE_CHANNEL(obj->chan)`)
        fn is_object(self) -> bool;
        /// Returns true if the data is a BGM. (C: `obj->chan == BGM_CHANNEL`)
        fn is_bgm(self) -> bool;
        /// Returns true if the data is a BGA. (C: `obj->chan == BGA_CHANNEL`)
        fn is_setbga(self) -> bool;
        /// Returns true if the data is a BPM change. (C: `obj->chan == BPM_CHANNEL`)
        fn is_setbpm(self) -> bool;
        /// Returns true if the data is a scroll stopper. (C: `obj->chan == STOP_CHANNEL`)
        fn is_stop(self) -> bool;

        /// Returns an associated lane if the data is an object.
        fn object_lane(self) -> Option<Lane>;
        /// Returns all sounds associated to the data.
        fn sounds(self) -> ~[SoundRef];
        /// Returns all sounds played when key is pressed.
        fn keydown_sound(self) -> Option<SoundRef>;
        /// Returns all sounds played when key is unpressed.
        fn keyup_sound(self) -> Option<SoundRef>;
        /// Returns all sounds played when the object is activated while the corresponding key is
        /// currently pressed. Bombs are the only instance of this kind of sounds.
        fn through_sound(self) -> Option<SoundRef>;
        /// Returns all images associated to the data.
        fn images(self) -> ~[ImageRef];
        /// Returns an associated damage value when the object is activated.
        fn through_damage(self) -> Option<Damage>;
    }

    /// Conversion operations for objects.
    pub trait ObjConvOps: ObjQueryOps {
        /// Returns a visible object with the same time, lane and sound as given object.
        fn to_visible(self) -> Self;
        /// Returns an invisible object with the same time, lane and sound as given object.
        fn to_invisible(self) -> Self;
        /// Returns a start of LN object with the same time, lane and sound as given object.
        fn to_lnstart(self) -> Self;
        /// Returns an end of LN object with the same time, lane and sound as given object.
        fn to_lndone(self) -> Self;
    }

    impl ObjQueryOps for ObjData {
        fn is_visible(self) -> bool {
            match self { Visible(*) => true, _ => false }
        }

        fn is_invisible(self) -> bool {
            match self { Invisible(*) => true, _ => false }
        }

        fn is_lnstart(self) -> bool {
            match self { LNStart(*) => true, _ => false }
        }

        fn is_lndone(self) -> bool {
            match self { LNDone(*) => true, _ => false }
        }

        fn is_ln(self) -> bool {
            match self { LNStart(*) | LNDone(*) => true, _ => false }
        }

        fn is_bomb(self) -> bool {
            match self { Bomb(*) => true, _ => false }
        }

        fn is_soundable(self) -> bool {
            match self { Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) => true, _ => false }
        }

        fn is_gradable(self) -> bool {
            match self { Visible(*) | LNStart(*) | LNDone(*) => true, _ => false }
        }

        fn is_renderable(self) -> bool {
            match self { Visible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true, _ => false }
        }

        fn is_object(self) -> bool {
            match self { Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true,
                         _ => false }
        }

        fn is_bgm(self) -> bool {
            match self { BGM(*) => true, _ => false }
        }

        fn is_setbga(self) -> bool {
            match self { SetBGA(*) => true, _ => false }
        }

        fn is_setbpm(self) -> bool {
            match self { SetBPM(*) => true, _ => false }
        }

        fn is_stop(self) -> bool {
            match self { Stop(*) => true, _ => false }
        }

        fn object_lane(self) -> Option<Lane> {
            match self {
                Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
                LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
                _ => None
            }
        }

        fn sounds(self) -> ~[SoundRef] {
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

        fn keydown_sound(self) -> Option<SoundRef> {
            match self { Visible(_,sref) | Invisible(_,sref) | LNStart(_,sref) => sref, _ => None }
        }

        fn keyup_sound(self) -> Option<SoundRef> {
            match self { LNDone(_,sref) => sref, _ => None }
        }

        fn through_sound(self) -> Option<SoundRef> {
            match self { Bomb(_,sref,_) => sref, _ => None }
        }

        fn images(self) -> ~[ImageRef] {
            match self { SetBGA(_,Some(iref)) => ~[iref], _ => ~[] }
        }

        fn through_damage(self) -> Option<Damage> {
            match self { Bomb(_,_,damage) => Some(damage), _ => None }
        }
    }

    impl ObjConvOps for ObjData {
        fn to_visible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Visible(lane,snd),
                _ => fail!(~"to_visible for non-object")
            }
        }

        fn to_invisible(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => Invisible(lane,snd),
                _ => fail!(~"to_invisible for non-object")
            }
        }

        fn to_lnstart(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNStart(lane,snd),
                _ => fail!(~"to_lnstart for non-object")
            }
        }

        fn to_lndone(self) -> ObjData {
            match self {
                Visible(lane,snd) | Invisible(lane,snd) |
                LNStart(lane,snd) | LNDone(lane,snd) => LNDone(lane,snd),
                _ => fail!(~"to_lndone for non-object")
            }
        }
    }

    /// Game play data associated to the time axis. It contains both objects (which are also
    /// associated to lanes) and object-like effects.
    #[deriving(Eq,Clone)]
    pub struct Obj {
        /// Time position in measures.
        time: float,
        /// Actual data.
        data: ObjData
    }

    impl Obj {
        /// Creates a `Visible` object.
        pub fn Visible(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            Obj { time: time, data: Visible(lane, sref.map_move(SoundRef)) }
        }

        /// Creates an `Invisible` object.
        pub fn Invisible(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            Obj { time: time, data: Invisible(lane, sref.map_move(SoundRef)) }
        }

        /// Creates an `LNStart` object.
        pub fn LNStart(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            Obj { time: time, data: LNStart(lane, sref.map_move(SoundRef)) }
        }

        /// Creates an `LNDone` object.
        pub fn LNDone(time: float, lane: Lane, sref: Option<Key>) -> Obj {
            Obj { time: time, data: LNDone(lane, sref.map_move(SoundRef)) }
        }

        /// Creates a `Bomb` object.
        pub fn Bomb(time: float, lane: Lane, sref: Option<Key>, damage: Damage) -> Obj {
            Obj { time: time, data: Bomb(lane, sref.map_move(SoundRef), damage) }
        }

        /// Creates a `BGM` object.
        pub fn BGM(time: float, sref: Key) -> Obj {
            Obj { time: time, data: BGM(SoundRef(sref)) }
        }

        /// Creates a `SetBGA` object.
        pub fn SetBGA(time: float, layer: BGALayer, iref: Option<Key>) -> Obj {
            Obj { time: time, data: SetBGA(layer, iref.map_move(ImageRef)) }
        }

        /// Creates a `SetBPM` object.
        pub fn SetBPM(time: float, bpm: BPM) -> Obj {
            Obj { time: time, data: SetBPM(bpm) }
        }

        /// Creates a `Stop` object.
        pub fn Stop(time: float, duration: Duration) -> Obj {
            Obj { time: time, data: Stop(duration) }
        }

        /// Returns the number of a measure containing this object.
        pub fn measure(&self) -> int { self.time.floor() as int }
    }

    impl Ord for Obj {
        fn lt(&self, other: &Obj) -> bool { self.time < other.time }
        fn le(&self, other: &Obj) -> bool { self.time <= other.time }
        fn ge(&self, other: &Obj) -> bool { self.time >= other.time }
        fn gt(&self, other: &Obj) -> bool { self.time > other.time }
    }

    impl ObjQueryOps for Obj {
        fn is_visible(self) -> bool { self.data.is_visible() }
        fn is_invisible(self) -> bool { self.data.is_invisible() }
        fn is_lnstart(self) -> bool { self.data.is_lnstart() }
        fn is_lndone(self) -> bool { self.data.is_lndone() }
        fn is_ln(self) -> bool { self.data.is_ln() }
        fn is_bomb(self) -> bool { self.data.is_bomb() }
        fn is_soundable(self) -> bool { self.data.is_soundable() }
        fn is_gradable(self) -> bool { self.data.is_gradable() }
        fn is_renderable(self) -> bool { self.data.is_renderable() }
        fn is_object(self) -> bool { self.data.is_object() }
        fn is_bgm(self) -> bool { self.data.is_bgm() }
        fn is_setbga(self) -> bool { self.data.is_setbga() }
        fn is_setbpm(self) -> bool { self.data.is_setbpm() }
        fn is_stop(self) -> bool { self.data.is_stop() }

        fn object_lane(self) -> Option<Lane> { self.data.object_lane() }
        fn sounds(self) -> ~[SoundRef] { self.data.sounds() }
        fn keydown_sound(self) -> Option<SoundRef> { self.data.keydown_sound() }
        fn keyup_sound(self) -> Option<SoundRef> { self.data.keyup_sound() }
        fn through_sound(self) -> Option<SoundRef> { self.data.through_sound() }
        fn images(self) -> ~[ImageRef] { self.data.images() }
        fn through_damage(self) -> Option<Damage> { self.data.through_damage() }
    }

    impl ObjConvOps for Obj {
        fn to_visible(self) -> Obj { Obj { time: self.time, data: self.data.to_visible() } }
        fn to_invisible(self) -> Obj { Obj { time: self.time, data: self.data.to_invisible() } }
        fn to_lnstart(self) -> Obj { Obj { time: self.time, data: self.data.to_lnstart() } }
        fn to_lndone(self) -> Obj { Obj { time: self.time, data: self.data.to_lndone() } }
    }

    //----------------------------------------------------------------------------------------------
    // BMS data

    /// Default BPM. This value comes from the original BMS specification.
    pub static DefaultBPM: BPM = BPM(130.0);

    /**
     * Blit commands, which manipulate the image after the image had been loaded. This maps to BMS
     * #BGA command. (C: `struct blitcmd`)
     *
     * Blitting occurs from the region `(x1,y1)-(x2,y2)` in the source surface to the region
     * `(dx,dy)-(dx+(x2-x1),dy+(y2-y1))` in the destination surface. The rectangular region contains
     * the upper-left corner but not the lower-right corner. The region is clipped to make
     * the upper-left corner has non-negative coordinates and the size of the region doesn't exceed
     * 256 by 256 pixels.
     */
    pub struct BlitCmd {
        dst: ImageRef, src: ImageRef,
        x1: int, y1: int, x2: int, y2: int, dx: int, dy: int
    }

    /// A value of BMS #PLAYER command signifying Single Play (SP), where only channels #1x are used
    /// for the game play.
    pub static SinglePlay: int = 1;
    /// A value of BMS #PLAYER command signifying Couple Play, where channels #1x and #2x renders to
    /// the different panels. They are originally meant to be played by different players with
    /// separate gauges and scores, but this mode of game play is increasingly unsupported by modern
    /// implementations. Angolmois has only a limited support for Couple Play.
    pub static CouplePlay: int = 2;
    /// A value of BMS #PLAYER command signifying Double Play (DP), where both channels #1x and #2x
    /// renders to a single wide panel. The chart is still meant to be played by one person.
    pub static DoublePlay: int = 3;

    /// Loaded BMS data. It is not a global state unlike C.
    pub struct Bms {
        /// Title. Maps to BMS #TITLE command. (C: `string[S_TITLE]`)
        title: Option<~str>,
        /// Genre. Maps to BMS #GENRE command. (C: `string[S_GENRE]`)
        genre: Option<~str>,
        /// Artist. Maps to BMS #ARTIST command. (C: `string[S_ARTIST]`)
        artist: Option<~str>,
        /// Path to an image for loading screen. Maps to BMS #STAGEFILE command.
        /// (C: `string[S_STAGEFILE]`)
        stagefile: Option<~str>,
        /// A base path used for loading all other resources. Maps to BMS #PATH_WAV command.
        /// (C: `string[S_BASEPATH]`)
        basepath: Option<~str>,

        /// Game mode. One of `SinglePlay`(1), `CouplePlay`(2) or `DoublePlay`(3). Maps to BMS
        /// #PLAYER command. (C: `value[V_PLAYER]`)
        player: int,
        /// Game level. Does not affect the actual game play. Maps to BMS #PLAYLEVEL command.
        /// (C: `value[V_PLAYLEVEL]`)
        playlevel: int,
        /// Gauge difficulty. Higher is easier. Maps to BMS #RANK command. (C: `value[V_RANK]`)
        rank: int,

        /// Initial BPM. (C: `initbpm`)
        initbpm: BPM,
        /// Paths to sound file relative to `basepath` or BMS file. (C: `sndpath`)
        sndpath: ~[Option<~str>],
        /// Paths to image/movie file relative to `basepath` or BMS file. (C: `imgpath`)
        imgpath: ~[Option<~str>],
        /// List of blit commands to be executed after `imgpath` is loaded. (C: `blitcmd`)
        blitcmd: ~[BlitCmd],

        /// List of objects sorted by the position. (C: `objs`)
        objs: ~[Obj],
        /// The scaling factor of measures. Defaults to 1.0. (C: `shortens`)
        shortens: ~[float],
        /// The number of measures after the origin, i.e. the length of the BMS file. The play stops
        /// after the last measure. (C: `length`)
        nmeasures: uint
    }

    /// Creates a default value of BMS data.
    pub fn Bms() -> Bms {
        // Rust: `None` is not clonable when it has a type of `Option<~str>`, so `[None, ..N]`
        //       syntax does not work. this makes a fixed size vector unconstructible.
        Bms { title: None, genre: None, artist: None, stagefile: None, basepath: None,
              player: SinglePlay, playlevel: 0, rank: 2, initbpm: DefaultBPM,
              sndpath: vec::from_elem(MAXKEY as uint, None),
              imgpath: vec::from_elem(MAXKEY as uint, None), blitcmd: ~[],
              objs: ~[], shortens: ~[], nmeasures: 0 }
    }

    impl Bms {
        /// Returns a scaling factor of given measure number. The default scaling factor is 1.0, and
        /// that value applies to any out-of-bound measures. (C: `shorten`)
        pub fn shorten(&self, measure: int) -> float {
            if measure < 0 || measure as uint >= self.shortens.len() {
                1.0
            } else {
                self.shortens[measure as uint]
            }
        }

        /// Calculates the virtual time that is `offset` measures away from the virtual time `base`.
        /// This takes account of the scaling factor, so if first four measures are scaled by 1/4,
        /// then `adjust_object_time(0.0, 2.0)` results in `5.0`. (C: `adjust_object_time`)
        pub fn adjust_object_time(&self, base: float, offset: float) -> float {
            let basemeasure = base.floor() as int;
            let baseshorten = self.shorten(basemeasure);
            let basefrac = base - basemeasure as float;
            let tonextmeasure = (1.0 - basefrac) * baseshorten;
            if offset < tonextmeasure {
                base + offset / baseshorten
            } else {
                let mut offset = offset - tonextmeasure;
                let mut i = basemeasure + 1;
                let mut curshorten = self.shorten(i);
                while offset >= curshorten {
                    offset -= curshorten;
                    i += 1;
                    curshorten = self.shorten(i);
                }
                i as float + offset / curshorten
            }
        }

        /// Calculates an adjusted offset between the virtual time `base` and `base + offset`.
        /// This takes account of the measure scaling factor, so for example, the adjusted offset
        /// between the virtual time 0.0 and 2.0 is, if the measure #000 is scaled by 1.2x,
        /// 2.2 measures instead of 2.0 measures. (C: `adjust_object_position`)
        pub fn adjust_object_position(&self, base: float, time: float) -> float {
            let basemeasure = base.floor() as int;
            let timemeasure = time.floor() as int;
            let basefrac = base - basemeasure as float;
            let timefrac = time - timemeasure as float;
            let mut pos = timefrac * self.shorten(timemeasure) -
                          basefrac * self.shorten(basemeasure);
            for i in range(basemeasure, timemeasure) {
                pos += self.shorten(i);
            }
            pos
        }
    }

    //----------------------------------------------------------------------------------------------
    // parsing

    /// Converts a single alphanumeric (base-36) letter to an integer. (C: `getdigit`)
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
        do getdigit(s[0]).and_then |a| {
            do getdigit(s[1]).map |&b| { a * 36 + b }
        }
    }

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    pub fn key2index_str(s: &str) -> Option<int> {
        if s.len() < 2 { return None; }
        let str::CharRange {ch:c1, next:p1} = s.char_range_at(0);
        do getdigit(c1).and_then |a| {
            let str::CharRange {ch:c2, next:p2} = s.char_range_at(p1);
            do getdigit(c2).map |&b| {
                assert!(p2 == 2); // both characters should be in ASCII
                a * 36 + b
            }
        }
    }

    /// Reads and parses the BMS file with given RNG from given reader.
    pub fn parse_bms_from_reader<R:Rng>(f: @::std::io::Reader, r: &mut R) -> Result<Bms,~str> {
        /// The list of recognized prefixes of directives. The longest prefix should come first.
        /// Also note that not all recognized prefixes are processed (counterexample being `ENDSW`).
        /// (C: `bmsheader`)
        static bmsheader: &'static [&'static str] = &[
            "TITLE", "GENRE", "ARTIST", "STAGEFILE", "PATH_WAV", "BPM",
            "PLAYER", "PLAYLEVEL", "RANK", "LNTYPE", "LNOBJ", "WAV", "BMP",
            "BGA", "STOP", "STP", "RANDOM", "SETRANDOM", "ENDRANDOM", "IF",
            "ELSEIF", "ELSE", "ENDSW", "END"];

        let mut bms = Bms();

        /// The state of the block, for determining which lines should be processed.
        enum BlockState {
            /// Not contained in the #IF block. (C: `state == -1`)
            Outside,
            /// Active. (C: `state == 0`)
            Process,
            /// Inactive, but (for the purpose of #IF/#ELSEIF/#ELSE/#ENDIF structure) can move to
            /// `Process` state when matching clause appears. (C: `state == 1`)
            Ignore,
            /// Inactive and won't be processed until the end of block. (C: `state == 2`)
            NoFurther
        }

        impl BlockState {
            /// Returns true if lines should be ignored in the current block given that the parent
            /// block was active. (C: `state > 0`)
            fn inactive(self) -> bool {
                match self { Outside | Process => false, Ignore | NoFurther => true }
            }
        }

        /**
         * Block information. The parser keeps a list of nested blocks and determines if
         * a particular line should be processed or not. (C: `struct rnd`)
         *
         * Angomlois actually recognizes only one kind of blocks, starting with #RANDOM or
         * #SETRANDOM and ending with #ENDRANDOM or #END(IF) outside an #IF block. An #IF block is
         * a state within #RANDOM, so it follows that #RANDOM/#SETRANDOM blocks can nest but #IF
         * can't nest unless its direct parent is #RANDOM/#SETRANDOM.
         */
        struct Block {
            /// A generated value if any. It can be `None` if this block is the topmost one (which
            /// is actually not a block but rather a sentinel) or the last `#RANDOM` or `#SETRANDOM`
            /// command was invalid, and #IF in that case will always evaluates to false. (C: `val`
            /// field)
            val: Option<int>,
            /// The state of the block. (C: `state` field)
            state: BlockState,
            /// True if the parent block is already ignored so that this block should be ignored
            /// no matter what `state` is. (C: `skip` field)
            skip: bool
        }

        impl Block {
            /// Returns true if lines should be ignored in the current block.
            fn inactive(&self) -> bool { self.skip || self.state.inactive() }
        }

        // Rust: #[deriving(Eq)] does not work inside the function. (#4913)
        impl Eq for BlockState {
            fn eq(&self, other: &BlockState) -> bool {
                match (*self, *other) {
                    (Outside, Outside) | (Process, Process) |
                    (Ignore, Ignore) | (NoFurther, NoFurther) => true,
                    (_, _) => false
                }
            }
            fn ne(&self, other: &BlockState) -> bool { !self.eq(other) }
        }
        impl Eq for Block {
            fn eq(&self, other: &Block) -> bool {
                // Rust: this is for using `ImmutableEqVector<T>::rposition`, which should have been
                //       in `ImmutableVector<T>`.
                self.val == other.val && self.state == other.state && self.skip == other.skip
            }
            fn ne(&self, other: &Block) -> bool { !self.eq(other) }
        }

        // A list of nested blocks. (C: `rnd`)
        let mut blk = ~[Block { val: None, state: Outside, skip: false }];

        /// An unprocessed data line of BMS file.
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

        impl Clone for BmsLine {
            fn clone(&self) -> BmsLine {
                BmsLine { measure: self.measure, chan: self.chan, data: self.data.clone() }
            }
        }

        // A list of unprocessed data lines. They have to be sorted with a stable algorithm and
        // processed in the order of measure number. (C: `bmsline`)
        let mut bmsline = ~[];
        // A table of BPMs. Maps to BMS #BPMxx command. (C: `bpmtab`)
        let mut bpmtab = ~[DefaultBPM, ..MAXKEY];
        // A table of the length of scroll stoppers. Maps to BMS #STOP/#STP commands. (C: `stoptab`)
        let mut stoptab = ~[Seconds(0.0), ..MAXKEY];

        // Allows LNs to be specified as a consecutive row of same or non-00 alphanumeric keys (MGQ
        // type, #LNTYPE 2). The default is to specify LNs as two endpoints (RDM type, #LNTYPE 1).
        // (C: `value[V_LNTYPE]`)
        let mut consecutiveln = false;

        // An end-of-LN marker used in LN specification for channels #1x/2x. Maps to BMS #LNOBJ
        // command. (C: `value[V_LNOBJ]`)
        let mut lnobj = None;

        let file = f.read_whole_stream();
        for line0 in file.split_iter(|&ch| ch == 10u8) {
            let line0 = ::util::str::from_fixed_utf8_bytes(line0, |_| ~"\ufffd");
            let line: &str = line0;

            // skip non-command lines
            let line = line.trim_left();
            if !line.starts_with("#") { loop; }
            let line = line.slice_from(1);

            // search for header prefix. the header list (`bmsheader`) is in the decreasing order
            // of prefix length.
            let mut prefix = "";
            for &header in bmsheader.iter() {
                if line.len() >= header.len() &&
                   line.slice_to(header.len()).to_ascii_upper() == header.to_owned() {
                    prefix = header;
                    break;
                }
            }
            let line = line.slice_from(prefix.len());

            // Common readers.
            macro_rules! read(
                (string $string:ident) => ({
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        bms.$string = Some(text.to_owned());
                    }
                });
                (value $value:ident) => ({
                    lex!(line; ws, int -> bms.$value);
                });
                (path $paths:ident) => ({
                    let mut key = Key(-1);
                    let mut path = "";
                    if lex!(line; Key -> key, ws, str -> path, ws*, !) {
                        let Key(key) = key;
                        bms.$paths[key] = Some(path.to_owned());
                    }
                })
            )

            assert!(!blk.is_empty());
            match (prefix, blk.last().inactive()) {
                // #TITLE|#GENRE|#ARTIST|#STAGEFILE|#PATH_WAV <string>
                ("TITLE", false) => read!(string title),
                ("GENRE", false) => read!(string genre),
                ("ARTIST", false) => read!(string artist),
                ("STAGEFILE", false) => read!(string stagefile),
                ("PATH_WAV", false) => read!(string basepath),

                // #BPM <float> or #BPMxx <float>
                ("BPM", false) => {
                    let mut key = Key(-1);
                    let mut bpm = 0.0;
                    if lex!(line; Key -> key, ws, float -> bpm) {
                        let Key(key) = key;
                        bpmtab[key] = BPM(bpm);
                    } else if lex!(line; ws, float -> bpm) {
                        bms.initbpm = BPM(bpm);
                    }
                }

                // #PLAYER|#PLAYLEVEL|#RANK <int>
                ("PLAYER", false) => read!(value player),
                ("PLAYLEVEL", false) => read!(value playlevel),
                ("RANK", false) => read!(value rank),

                // #LNTYPE <int>
                ("LNTYPE", false) => {
                    let mut lntype = 1;
                    if lex!(line; ws, int -> lntype) {
                        consecutiveln = (lntype == 2);
                    }
                }
                // #LNOBJ <key>
                ("LNOBJ", false) => {
                    let mut key = Key(-1);
                    if lex!(line; ws, Key -> key) { lnobj = Some(key); }
                }

                // #WAVxx|#BMPxx <path>
                ("WAV", false) => read!(path sndpath),
                ("BMP", false) => read!(path imgpath),

                // #BGAxx yy <int> <int> <int> <int> <int> <int>
                ("BGA", false) => {
                    let mut dst = Key(0);
                    let mut src = Key(0);
                    let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                           x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
                    if lex!(line; Key -> dst, ws, Key -> src, ws,
                            int -> bc.x1, ws, int -> bc.y1, ws, int -> bc.x2, ws, int -> bc.y2, ws,
                            int -> bc.dx, ws, int -> bc.dy) {
                        bc.src = ImageRef(src);
                        bc.dst = ImageRef(dst);
                        bms.blitcmd.push(bc);
                    }
                }

                // #STOPxx <int>
                ("STOP", false) => {
                    let mut key = Key(-1);
                    let mut duration = 0;
                    if lex!(line; Key -> key, ws, int -> duration) {
                        let Key(key) = key;
                        stoptab[key] = Measures(duration as float / 192.0);
                    }
                }

                // #STP<int>.<int> <int>
                ("STP", false) => {
                    let mut measure = 0;
                    let mut frac = 0;
                    let mut duration = 0;
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

                        // do not generate a random value if the entire block is skipped (but it
                        // still marks the start of block)
                        let inactive = blk.last().inactive();
                        let generated = do val.and_then |val| {
                            if prefix == "SETRANDOM" {
                                Some(val)
                            } else if !inactive {
                                Some(r.gen_integer_range(1, val + 1))
                            } else {
                                None
                            }
                        };
                        blk.push(Block { val: generated, state: Outside, skip: inactive });
                    }
                }

                // #ENDRANDOM
                ("ENDRANDOM", _) => {
                    if blk.len() > 1 { blk.pop(); }
                }

                // #IF|#ELSEIF <int>
                ("IF", _) |
                ("ELSEIF", _) => {
                    let mut val = 0;
                    if lex!(line; ws, int -> val) {
                        let val = if val <= 0 {None} else {Some(val)};

                        // Rust: `blk.last_ref()` may be useful?
                        let last = &mut blk[blk.len() - 1];
                        last.state =
                            if (prefix == "IF" && !last.state.inactive()) || last.state == Ignore {
                                if val.is_none() || val != last.val {Ignore} else {Process}
                            } else {
                                NoFurther
                            };
                    }
                }

                // #ELSE
                ("ELSE", _) => {
                    let last = &mut blk[blk.len() - 1];
                    last.state = if last.state == Ignore {Process} else {NoFurther};
                }

                // #END(IF)
                ("END", _) => {
                    let lastinside = blk.iter().rposition(|&i| i.state != Outside); // XXX #3511
                    for &idx in lastinside.iter() {
                        if idx > 0 { blk.truncate(idx + 1); }
                    }

                    let last = &mut blk[blk.len() - 1];
                    last.state = Outside;
                }

                // #nnnmm:...
                ("", false) => {
                    let mut measure = 0;
                    let mut chan = Key(0);
                    let mut data = "";
                    if lex!(line; Measure -> measure, Key -> chan, ':', ws*, str -> data, ws*, !) {
                        bmsline.push(BmsLine { measure: measure, chan: chan,
                                               data: data.to_owned() })
                    }
                }

                (_, _) => {}
            }
        }

        // Poor BGA defined by #BMP00 wouldn't be played if it is a movie. We can't just let it
        // played at the beginning of the chart as the "beginning" is not always 0.0 (actually,
        // `originoffset`). Thus we add an artificial BGA object at time 0.0 only when the other
        // poor BGA does not exist at this position. (C: `poorbgafix`)
        let mut poorbgafix = true;

        // Indices to last visible object per channels. A marker specified by #LNOBJ will turn
        // this last object to the start of LN. (C: `prev12`)
        let mut lastvis: [Option<uint>, ..NLANES] = [None, ..NLANES];

        // Indices to last LN start or end inserted (and not finalized yet) per channels.
        // If `consecutiveln` is on (#LNTYPE 2), the position of referenced object gets updated
        // during parsing; if off (#LNTYPE 1), it is solely used for checking if we are inside
        // the LN or not. (C: `prev56`)
        let mut lastln: [Option<uint>, ..NLANES] = [None, ..NLANES];

        // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
        // particular position `t`. The position `t2` next to `t` is used for some cases that
        // an alphanumeric key designates an area rather than a point.
        let handle_key = |chan: Key, t: float, t2: float, v: Key| {
            // Adds an object. Objects are sorted by its position later.
            let add = |obj: Obj| { bms.objs.push(obj); };
            // Adds an object and returns its position. LN parsing generally mutates the existing
            // object for simplicity.
            let mark = |obj: Obj| -> Option<uint> {
                let marked = bms.objs.len();
                bms.objs.push(obj);
                Some(marked)
            };

            match *chan {
                // channel #01: BGM
                1 => { add(Obj::BGM(t, v)); }

                // channel #03: BPM as an hexadecimal key
                3 => {
                    let v = v.to_hex(); // XXX #3511
                    for &v in v.iter() {
                        add(Obj::SetBPM(t, BPM(v as float)))
                    }
                }

                // channel #04: BGA layer 1
                4 => { add(Obj::SetBGA(t, Layer1, Some(v))); }

                // channel #06: POOR BGA
                6 => {
                    add(Obj::SetBGA(t, PoorBGA, Some(v)));
                    poorbgafix = false; // we don't add artificial BGA
                }

                // channel #07: BGA layer 2
                7 => { add(Obj::SetBGA(t, Layer2, Some(v))); }

                // channel #08: BPM defined by #BPMxx
                8 => { add(Obj::SetBPM(t, bpmtab[*v])); } // TODO bpmtab validity check

                // channel #09: scroll stopper defined by #STOPxx
                9 => { add(Obj::Stop(t, stoptab[*v])); } // TODO stoptab validity check

                // channel #0A: BGA layer 3
                10 => { add(Obj::SetBGA(t, Layer3, Some(v))); }

                // channels #1x/2x: visible object, possibly LNs when #LNOBJ is in active
                36/*1*36*/..107/*3*36-1*/ => {
                    let lane = Lane::from_channel(chan);
                    if lnobj.is_some() && lnobj == Some(v) {
                        // change the last inserted visible object to the start of LN if any.
                        let lastvispos = lastvis[*lane];
                        for &pos in lastvispos.iter() {
                            assert!(bms.objs[pos].is_visible());
                            bms.objs[pos] = bms.objs[pos].to_lnstart();
                            add(Obj::LNDone(t, lane, Some(v)));
                            lastvis[*lane] = None;
                        }
                    } else {
                        lastvis[*lane] = mark(Obj::Visible(t, lane, Some(v)));
                    }
                }

                // channels #3x/4x: invisible object
                108/*3*36*/..179/*5*36-1*/ => {
                    let lane = Lane::from_channel(chan);
                    add(Obj::Invisible(t, lane, Some(v)));
                }

                // channels #5x/6x, #LNTYPE 1: LN endpoints
                180/*5*36*/..251/*7*36-1*/ if !consecutiveln => {
                    let lane = Lane::from_channel(chan);

                    // a pair of non-00 alphanumeric keys designate one LN. if there are an odd
                    // number of them, the last LN is implicitly closed later.
                    if lastln[*lane].is_some() {
                        lastln[*lane] = None;
                        add(Obj::LNDone(t, lane, Some(v)));
                    } else {
                        lastln[*lane] = mark(Obj::LNStart(t, lane, Some(v)));
                    }
                }

                // channels #5x/6x, #LNTYPE 2: LN areas
                180/*5*36*/..251/*7*36-1*/ if consecutiveln => {
                    let lane = Lane::from_channel(chan);

                    // one non-00 alphanumeric key, in the absence of other information, inserts one
                    // complete LN starting at `t` and ending at `t2`.
                    //
                    // the next non-00 alphanumeric key also inserts one complete LN from `t` to
                    // `t2`, unless there is already an end of LN at `t` in which case the end of LN
                    // is simply moved from `t` to `t2` (effectively increasing the length of
                    // previous LN).
                    match lastln[*lane] {
                        Some(pos) if bms.objs[pos].time == t => {
                            assert!(bms.objs[pos].is_lndone());
                            bms.objs[pos].time = t2;
                        }
                        _ => {
                            add(Obj::LNStart(t, lane, Some(v)));
                            lastln[*lane] = mark(Obj::LNDone(t2, lane, Some(v)));
                        }
                    }
                }

                // channels #Dx/Ex: bombs, base-36 damage value (unit of 0.5% of the full gauge) or
                // instant death (ZZ)
                468/*0xD*36*/..539/*0xF*36-1*/ => {
                    let lane = Lane::from_channel(chan);
                    let damage = match *v {
                        1..200 => Some(GaugeDamage(*v as float / 200.0)),
                        1295 => Some(InstantDeath), // XXX 1295=MAXKEY-1
                        _ => None
                    };
                    for &damage in damage.iter() {
                        add(Obj::Bomb(t, lane, Some(Key(0)), damage));
                    }
                }

                // unsupported: channels #0B/0C/0D/0E (BGA opacity), #97/98 (sound volume),
                // #99 (text), #A0 (dynamic #RANK), #A1/A2/A3/A4 (BGA color key update),
                // #A5 (BGA on keypress), #A6 (player-specific option)
                _ => {}
            }
        };

        // loops over the sorted bmslines
        ::extra::sort::tim_sort(bmsline);
        for line in bmsline.iter() {
            if *line.chan == 2 {
                let mut shorten = 0.0;
                if lex!(line.data; ws*, float -> shorten) {
                    if shorten > 0.001 {
                        bms.shortens.grow_set(line.measure, &1.0, shorten);
                    }
                }
            } else {
                let measure = line.measure as float;
                let data: ~[char] = line.data.iter().collect();
                let max = data.len() / 2 * 2;
                let count = max as float;
                for i in iter::range_step(0, max, 2) {
                    let v = key2index(data.slice(i, i+2));
                    for &v in v.iter() {
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
        for i in range(0, NLANES) {
            if lastvis[i].is_some() || (!consecutiveln && lastln[i].is_some()) {
                bms.objs.push(Obj::LNDone(endt, Lane(i), None));
            }
        }

        Ok(bms)
    }

    /// Reads and parses the BMS file with given RNG. (C: `parse_bms`)
    pub fn parse_bms<R:Rng>(bmspath: &str, r: &mut R) -> Result<Bms,~str> {
        do ::std::io::file_reader(&Path(bmspath)).and_then |f| {
            parse_bms_from_reader(f, r)
        }
    }

    //----------------------------------------------------------------------------------------------
    // key specification

    /// The key specification. Specifies the order and apperance of lanes. Once determined from
    /// the options and BMS file, the key specification is fixed and independent of other data
    /// (e.g. `#PLAYER` value).
    pub struct KeySpec {
        /// The number of lanes on the left side. This number is significant only when Couple Play
        /// is used. (C: `nleftkeys`)
        split: uint,
        /// The order of significant lanes. The first `nleftkeys` lanes go to the left side and
        /// the remaining lanes (C: `nrightkeys`) go to the right side. (C: `keyorder`)
        order: ~[Lane],
        /// The type of lanes. (C: `keykind`)
        kinds: ~[Option<KeyKind>]
    }

    impl KeySpec {
        /// Returns a number of lanes that count towards "keys". Notably scratches and pedals do not
        /// count as keys. (C: `nkeys`)
        pub fn nkeys(&self) -> uint {
            let mut nkeys = 0;
            for kind in self.kinds.iter().filter_map(|kind| *kind) {
                if kind.counts_as_key() { nkeys += 1; }
            }
            nkeys
        }

        /// Returns a list of lanes on the left side, from left to right.
        pub fn left_lanes<'r>(&'r self) -> &'r [Lane] {
            assert!(self.split <= self.order.len());
            self.order.slice(0, self.split)
        }

        /// Returns a list of lanes on the right side if any, from left to right.
        pub fn right_lanes<'r>(&'r self) -> &'r [Lane] {
            assert!(self.split <= self.order.len());
            self.order.slice(self.split, self.order.len())
        }
    }

    /// Parses the key specification from the string. (C: `parse_key_spec`)
    pub fn parse_key_spec(s: &str) -> Option<~[(Lane, KeyKind)]> {
        let mut specs = ~[];
        let mut s = s.trim_left();
        while !s.is_empty() {
            let mut chan = Key(0);
            let mut kind = '\x00';
            if !lex!(s; Key -> chan, char -> kind, ws*, str* -> s, !) {
                return None;
            }
            match (chan, KeyKind::from_char(kind)) {
                (Key(36/*1*36*/..107/*3*36-1*/), Some(kind)) => {
                    specs.push((Lane(*chan as uint - 1*36), kind));
                }
                (_, _) => { return None; }
            }
        }
        Some(specs)
    }

    /// A list of well-known key specifications. (C: `presets`)
    static PRESETS: &'static [(&'static str, &'static str, &'static str)] = &[
        // 5-key BMS, SP/DP
        ("5",     "16s 11a 12b 13a 14b 15a", ""),
        ("10",    "16s 11a 12b 13a 14b 15a", "21a 22b 23a 24b 25a 26s"),
        // 5-key BMS with a foot pedal, SP/DP
        ("5/fp",  "16s 11a 12b 13a 14b 15a 17p", ""),
        ("10/fp", "16s 11a 12b 13a 14b 15a 17p", "27p 21a 22b 23a 24b 25a 26s"),
        // 7-key BME, SP/DP
        ("7",     "16s 11a 12b 13a 14b 15a 18b 19a", ""),
        ("14",    "16s 11a 12b 13a 14b 15a 18b 19a", "21a 22b 23a 24b 25a 28b 29a 26s"),
        // 7-key BME with a foot pedal, SP/DP
        ("7/fp",  "16s 11a 12b 13a 14b 15a 18b 19a 17p", ""),
        ("14/fp", "16s 11a 12b 13a 14b 15a 18b 19a 17p", "27p 21a 22b 23a 24b 25a 28b 29a 26s"),
        // 9-key PMS (#PLAYER 3)
        ("9",     "11q 12w 13e 14r 15t 22r 23e 24w 25q", ""),
        // 9-key PMS (BME-compatible)
        ("9-bme", "11q 12w 13e 14r 15t 18r 19e 16w 17q", ""),
    ];

    /**
     * Determines the key specification from the preset name, in the absence of explicit key
     * specification with `-K` option. (C: `detect_preset`)
     *
     * Besides from presets specified in `PRESETS`, this function also allows the following
     * pseudo-presets inferred from the BMS file:
     *
     * - `bms`, `bme`, `bml` or no preset: Selects one of eight presets `{5,7,10,14}[/fp]`.
     * - `pms`: Selects one of two presets `9` and `9-bme`.
     */
    pub fn preset_to_key_spec(bms: &Bms, preset: Option<~str>) -> Option<(~str, ~str)> {
        let mut present = [false, ..NLANES];
        for &obj in bms.objs.iter() {
            let lane = obj.object_lane(); // XXX #3511
            for &Lane(lane) in lane.iter() {
                present[lane] = true;
            }
        }

        let preset = match preset.map(|s| s.to_ascii_lower()) {
            None | Some(~"bms") | Some(~"bme") | Some(~"bml") => {
                let isbme = (present[8] || present[9] || present[36+8] || present[36+9]);
                let haspedal = (present[7] || present[36+7]);
                let nkeys = match bms.player {
                    CouplePlay | DoublePlay => if isbme {~"14"} else {~"10"},
                    _                       => if isbme {~"7" } else {~"5" }
                };
                if haspedal {nkeys + "/fp"} else {nkeys}
            },
            Some(~"pms") => {
                let isbme = (present[6] || present[7] || present[8] || present[9]);
                if isbme {~"9-bme"} else {~"9"}
            },
            Some(preset) => preset
        };

        for &(name, leftkeys, rightkeys) in PRESETS.iter() {
            if name == preset {
                return Some((leftkeys.to_owned(), rightkeys.to_owned()));
            }
        }
        None
    }

    //----------------------------------------------------------------------------------------------
    // post-processing

    /// Updates the object in place to BGM or placeholder. (C: `remove_or_replace_note`)
    fn remove_or_replace_note(obj: &mut Obj) {
        obj.data = match obj.data {
            Visible(_,Some(sref)) | Invisible(_,Some(sref)) |
            LNStart(_,Some(sref)) | LNDone(_,Some(sref)) => BGM(sref),
            _ => Deleted
        };
    }

    /// Fixes a problematic data. (C: `sanitize_bms`)
    pub fn sanitize_bms(bms: &mut Bms) {
        ::extra::sort::tim_sort(bms.objs);

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
                    let ty = to_type(obj); // XXX #3511
                    for &t in ty.iter() {
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
                    let ty = to_type(obj); // XXX #3511
                    for &t in ty.iter() {
                        if (types & (1 << t)) == 0 {
                            remove_or_replace_note(obj);
                        }
                    }
                    i += 1;
                }
            }
        }

        for lane in range(0, NLANES) {
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
            do sanitize(bms.objs, |obj| to_type(obj)) |mut types| { // XXX #7363
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
                match bms.objs.iter().rposition(|obj| to_type(obj).is_some()) {
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

    /// Removes insignificant objects (i.e. not in visible lanes) and ensures that there is no
    /// `Deleted` object. (C: `analyze_and_compact_bms`)
    pub fn compact_bms(bms: &mut Bms, keyspec: &KeySpec) {
        for obj in bms.objs.mut_iter() {
            let lane = obj.object_lane(); // XXX #3511
            for &Lane(lane) in lane.iter() {
                if keyspec.kinds[lane].is_none() {
                    remove_or_replace_note(obj)
                }
            }
        }

        do bms.objs.retain |&obj| { obj.data != Deleted }
    }

    //----------------------------------------------------------------------------------------------
    // analysis

    /// Derived BMS information. Again, this is not a global state.
    pub struct BmsInfo {
        /// The start position of the BMS file. This is either -1.0 or 0.0 depending on the first
        /// measure has any visible objects or not. (C: `originoffset`)
        originoffset: float,
        /// Set to true if the BMS file has a BPM change. (C: `hasbpmchange`)
        hasbpmchange: bool,
        /// Set to true if the BMS file has long note objects. (C: `haslongnote`)
        haslongnote: bool,
        /// The number of visible objects in the BMS file. A long note object counts as one object.
        /// (C: `nnotes`)
        nnotes: int,
        /// The maximum possible score. (C: `maxscore`)
        maxscore: int
    }

    /// Analyzes the loaded BMS file. (C: `analyze_and_compact_bms`)
    pub fn analyze_bms(bms: &Bms) -> BmsInfo {
        let mut infos = BmsInfo { originoffset: 0.0, hasbpmchange: false, haslongnote: false,
                                  nnotes: 0, maxscore: 0 };

        for &obj in bms.objs.iter() {
            infos.haslongnote |= obj.is_lnstart();
            infos.hasbpmchange |= obj.is_setbpm();

            if obj.is_lnstart() || obj.is_visible() {
                infos.nnotes += 1;
                if obj.time < 1.0 { infos.originoffset = -1.0; }
            }
        }

        for i in range(0, infos.nnotes) {
            let ratio = (i as float) / (infos.nnotes as float);
            infos.maxscore += (300.0 * (1.0 + ratio)) as int;
        }

        infos
    }

    /// Calculates the duration of the loaded BMS file in seconds. `sound_length` should return
    /// the length of sound resources in seconds or 0.0. (C: `get_bms_duration`)
    pub fn bms_duration(bms: &Bms, originoffset: float,
                        sound_length: &fn(SoundRef) -> float) -> float {
        let mut pos = originoffset;
        let mut bpm = bms.initbpm;
        let mut time = 0.0;
        let mut sndtime = 0.0;

        for &obj in bms.objs.iter() {
            let delta = bms.adjust_object_position(pos, obj.time);
            time += bpm.measure_to_msec(delta);
            match obj.data {
                Visible(_,Some(sref)) | LNStart(_,Some(sref)) | BGM(sref) => {
                    sndtime = cmp::max(sndtime, time + sound_length(sref) * 1000.0);
                }
                SetBPM(BPM(newbpm)) => {
                    if newbpm > 0.0 {
                        bpm = BPM(newbpm);
                    } else if newbpm < 0.0 {
                        bpm = BPM(newbpm);
                        let delta = bms.adjust_object_position(originoffset, pos);
                        time += BPM(-newbpm).measure_to_msec(delta);
                        break;
                    }
                }
                Stop(duration) => {
                    time += duration.to_msec(bpm);
                }
                _ => {}
            }
            pos = obj.time;
        }

        if *bpm > 0.0 { // the chart scrolls backwards to `originoffset` for negative BPM
            let delta = bms.adjust_object_position(pos, (bms.nmeasures + 1) as float);
            time += bpm.measure_to_msec(delta);
        }
        cmp::max(time, sndtime) / 1000.0
     }

    //----------------------------------------------------------------------------------------------
    // modifiers

    /// Applies a function to the object lane if any. This is used to shuffle the lanes without
    /// modifying the relative time position.
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

    /// Swaps given lanes in the reverse order. (C: `shuffle_bms` with `MIRROR_MODF`)
    pub fn apply_mirror_modf(bms: &mut Bms, lanes: &[Lane]) {
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        let mut assocs = lanes.iter().zip(lanes.rev_iter()); // XXX #3511
        for (&Lane(from), &to) in assocs {
            map[from] = to;
        }

        for obj in bms.objs.mut_iter() {
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    /// Swaps given lanes in the random order. (C: `shuffle_bms` with
    /// `SHUFFLE_MODF`/`SHUFFLEEX_MODF`)
    pub fn apply_shuffle_modf<R:Rng>(bms: &mut Bms, r: &mut R, lanes: &[Lane]) {
        let shuffled = r.shuffle(lanes.to_owned());
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        let mut assocs = lanes.iter().zip(shuffled.iter()); // XXX #3511
        for (&Lane(from), &to) in assocs {
            map[from] = to;
        }

        for obj in bms.objs.mut_iter() {
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    /// Swaps given lanes in the random order, where the order is determined per object.
    /// `bms` should be first sanitized by `sanitize_bms`. It does not cause objects to move within
    /// another LN object, or place two objects in the same or very close time position to the same
    /// lane. (C: `shuffle_bms` with `RANDOM_MODF`/`RANDOMEX_MODF`)
    pub fn apply_random_modf<R:Rng>(bms: &mut Bms, r: &mut R, lanes: &[Lane]) {
        let mut movable = lanes.to_owned();
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));

        let mut lasttime = float::neg_infinity;
        for obj in bms.objs.mut_iter() {
            if obj.is_lnstart() {
                let lane = obj.object_lane().unwrap();
                match movable.position_elem(&lane) {
                    Some(i) => { movable.swap_remove(i); }
                    None => fail!(~"non-sanitized BMS detected")
                }
            }
            if lasttime < obj.time { // reshuffle required
                lasttime = obj.time + 1e-4;
                let shuffled = r.shuffle(movable.clone());
                let mut assocs = movable.iter().zip(shuffled.iter()); // XXX #3511
                for (&Lane(from), &to) in assocs {
                    map[from] = to;
                }
            }
            if obj.is_lnstart() {
                let lane = obj.object_lane().unwrap();
                movable.push(lane);
            }
            update_object_lane(obj, |Lane(lane)| map[lane]);
        }
    }

    //----------------------------------------------------------------------------------------------

}

//==================================================================================================
// graphics

/// Graphic utilities.
pub mod gfx {
    use std::{vec, num};
    use sdl::Rect;
    pub use sdl::video::*;

    //----------------------------------------------------------------------------------------------
    // `Rect` additions

    /// A trait that can be translated to point coordinates (`x` and `y` fields in `sdl::Rect`,
    /// hence the name). Also contains `()`.
    pub trait XyOpt {
        /// Returns point coordinates if any.
        fn xy_opt(&self) -> Option<(i16,i16)>;
    }

    /// Same as `XyOpt` but does not contain `()`.
    pub trait Xy: XyOpt {
        /// Returns point coordinates.
        fn xy(&self) -> (i16,i16);
    }

    /// A trait that can be translated to a rectangular area (`w` and `h` fields in `sdl::Rect`,
    /// hence the name). Also contains `()`.
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

    // Rust: we can't define these with `impl<T:Xy> XyOpt for T` due to the ambiguity.
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

    /// A helper trait for defining every implementations for types `(T1,T2)` where `T1` and `T2` is
    /// convertible to an integer.
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

    impl<X:ToInt16+Clone,Y:ToInt16+Clone> XyOpt for (X,Y) {
        #[inline(always)]
        fn xy_opt(&self) -> Option<(i16,i16)> {
            let (x, y) = self.clone();
            Some((x.to_i16(), y.to_i16()))
        }
    }

    impl<X:ToInt16+Clone,Y:ToInt16+Clone> Xy for (X,Y) {
        #[inline(always)]
        fn xy(&self) -> (i16,i16) {
            let (x, y) = self.clone();
            (x.to_i16(), y.to_i16())
        }
    }

    impl<W:ToInt16+Clone,H:ToInt16+Clone> WhOpt for (W,H) {
        #[inline(always)]
        fn wh_opt(&self) -> Option<(u16,u16)> {
            let (w, h) = self.clone();
            Some((w.to_u16(), h.to_u16()))
        }
    }

    impl<W:ToInt16+Clone,H:ToInt16+Clone> Wh for (W,H) {
        #[inline(always)]
        fn wh(&self) -> (u16,u16) {
            let (w, h) = self.clone();
            (w.to_u16(), h.to_u16())
        }
    }

    /// Constructs an `sdl::Rect` from given point coordinates. Fills `w` and `h` fields to 0
    /// as expected by the second `sdl::Rect` argument from `SDL_BlitSurface`.
    #[inline(always)]
    fn rect_from_xy<XY:Xy>(xy: XY) -> Rect {
        let (x, y) = xy.xy();
        Rect { x: x, y: y, w: 0, h: 0 }
    }

    /// Constructs an `sdl::Rect` from given point coordinates and optional rectangular area.
    /// `rect_from_xywh(xy, ())` equals to `rect_from_xy(xy)`.
    #[inline(always)]
    fn rect_from_xywh<XY:Xy,WH:WhOpt>(xy: XY, wh: WH) -> Rect {
        let (x, y) = xy.xy();
        let (w, h) = wh.wh_opt().unwrap_or((0, 0));
        Rect { x: x, y: y, w: w, h: h }
    }

    /// Additions to `sdl::video::Surface`. They replace their `_rect` suffixed counterparts,
    /// which are generally annoying to work with.
    pub trait SurfaceAreaUtil {
        /// An alternative interface to `set_clip_rect`.
        fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH);
        /// An alternative interface to `blit_rect`.
        fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                                                    srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool;
        /// An alternative interface to `fill_rect`.
        fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH, color: Color) -> bool;
    }

    impl SurfaceAreaUtil for Surface {
        #[inline(always)]
        fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH) {
            let rect = rect_from_xywh(xy, wh);
            self.set_clip_rect(&rect)
        }

        #[inline(always)]
        fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                                                    srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool {
            let srcrect = rect_from_xywh(srcxy, wh);
            let dstrect = dstxy.xy_opt().map(|&xy| rect_from_xywh(xy, &srcrect));
            self.blit_rect(src, Some(srcrect), dstrect)
        }

        #[inline(always)]
        fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH, color: Color) -> bool {
            let rect = rect_from_xywh(xy, wh);
            self.fill_rect(Some(rect), color)
        }
    }

    //----------------------------------------------------------------------------------------------
    // color

    /// Extracts red, green, blue components from given color.
    fn to_rgb(c: Color) -> (u8, u8, u8) {
        match c { RGB(r, g, b) | RGBA(r, g, b, _) => (r, g, b) }
    }

    /// Linear color gradient.
    #[deriving(Eq)]
    pub struct Gradient {
        /// A color at the position 0.0. Normally used as a topmost value.
        zero: Color,
        /// A color at the position 1.0. Normally used as a bottommost value.
        one: Color
    }

    /// Creates a new color gradient (for text printing).
    pub fn Gradient(top: Color, bottom: Color) -> Gradient {
        Gradient { zero: top, one: bottom }
    }

    /// A trait for color or color gradient. The color at the particular position can be calculated
    /// with `blend` method.
    pub trait Blend {
        /// Returns itself. This is same as `Clone::clone` but redefined here due to the inability
        /// of implementing `Clone` for `Color`.
        fn clone(&self) -> Self;
        /// Calculates the color at the position `num/denom`. (C: `blend`)
        fn blend(&self, num: int, denom: int) -> Color;
    }

    impl Blend for Color {
        fn clone(&self) -> Color { *self }
        fn blend(&self, _num: int, _denom: int) -> Color { *self }
    }

    impl Blend for Gradient {
        fn clone(&self) -> Gradient { *self }
        fn blend(&self, num: int, denom: int) -> Color {
            fn mix(x: u8, y: u8, num: int, denom: int) -> u8 {
                let x = x as int;
                let y = y as int;
                (y + ((x - y) * num / denom)) as u8
            }

            let (r0, g0, b0) = to_rgb(self.zero);
            let (r1, g1, b1) = to_rgb(self.one);
            RGB(mix(r1, r0, num, denom), mix(g1, g0, num, denom), mix(b1, b0, num, denom))
        }
    }

    //----------------------------------------------------------------------------------------------
    // surface utilities

    /// Creates a new RAM-backed surface. By design, Angolmois does not use a VRAM-backed surface
    /// except for the screen. (C: `newsurface`)
    pub fn new_surface(w: uint, h: uint) -> ~Surface {
        match Surface::new([SWSurface], w as int, h as int, 32, 0xff0000, 0xff00, 0xff, 0) {
            Ok(surface) => surface,
            Err(err) => die!("new_surface failed: %s", err)
        }
    }

    /// A proxy to `sdl::video::Surface` for the direct access to pixels. For now, it is for 32 bits
    /// per pixel only.
    pub struct SurfacePixels<'self> {
        fmt: *ll::SDL_PixelFormat,
        width: uint,
        height: uint,
        pitch: uint,
        pixels: &'self mut [u32]
    }

    /// A trait for the direct access to pixels.
    pub trait SurfacePixelsUtil {
        /// Grants the direct access to pixels. Also locks the surface as needed, so you can't blit
        /// during working with pixels.
        fn with_pixels<R>(&self, f: &fn(pixels: &mut SurfacePixels) -> R) -> R;
    }

    impl SurfacePixelsUtil for Surface {
        fn with_pixels<R>(&self, f: &fn(pixels: &mut SurfacePixels) -> R) -> R {
            do self.with_lock |pixels| {
                let fmt = unsafe {(*self.raw).format};
                let pitch = unsafe {((*self.raw).pitch / 4) as uint};
                let pixels = unsafe {::std::cast::transmute(pixels)};
                let mut proxy = SurfacePixels { fmt: fmt, width: self.get_width() as uint,
                                                height: self.get_height() as uint,
                                                pitch: pitch, pixels: pixels };
                f(&mut proxy)
            }
        }
    }

    impl<'self> SurfacePixels<'self> {
        /// Returns a pixel at given position. (C: `getpixel`)
        pub fn get_pixel(&self, x: uint, y: uint) -> Color {
            Color::from_mapped(self.pixels[x + y * self.pitch], self.fmt)
        }

        /// Sets a pixel to given position. (C: `putpixel`)
        pub fn put_pixel(&mut self, x: uint, y: uint, c: Color) {
            self.pixels[x + y * self.pitch] = c.to_mapped(self.fmt);
        }

        /// Sets or blends (if `c` is `RGBA`) a pixel to given position. (C: `putblendedpixel`)
        pub fn put_blended_pixel(&mut self, x: uint, y: uint, c: Color) {
            match c {
                RGB(*) => self.put_pixel(x, y, c),
                RGBA(r,g,b,a) => match self.get_pixel(x, y) {
                    RGB(r2,g2,b2) | RGBA(r2,g2,b2,_) => {
                        let grad = Gradient { zero: RGB(r,g,b), one: RGB(r2,g2,b2) };
                        self.put_pixel(x, y, grad.blend(a as int, 255));
                    }
                }
            }
        }
    }

    /// A scaling factor for the calculation of convolution kernel.
    static FP_SHIFT1: int = 11;
    /// A scaling factor for the summation of weighted pixels.
    static FP_SHIFT2: int = 16;

    /// Returns `2^FP_SHIFT * W(x/y)` where `W(x)` is a bicubic kernel function. `y` should be
    /// positive. (C: `bicubic_kernel`)
    fn bicubic_kernel(x: int, y: int) -> int {
        let x = num::abs(x);
        if x < y {
            // W(x/y) = 1/2 (2 - 5(x/y)^2 + 3(x/y)^3)
            ((2*y*y - 5*x*x + 3*x*x/y*x) << (FP_SHIFT1-1)) / (y*y)
        } else if x < y * 2 {
            // W(x/y) = 1/2 (4 - 8(x/y) + 5(x/y)^2 - (x/y)^3)
            ((4*y*y - 8*x*y + 5*x*x - x*x/y*x) << (FP_SHIFT1-1)) / (y*y)
        } else {
            0
        }
    }

    /**
     * Performs the bicubic interpolation. `dest` should be initialized to the target dimension
     * before calling this function. This function should be used only for the upscaling; it can do
     * the downscaling somehow but technically its result is incorrect. (C: `bicubic_interpolation`)
     *
     * Well, this function is one of the ugliest functions in Angolmois, especially since it is
     * a complicated (in terms of code complexity) and still poor (we normally use the matrix form
     * instead) implementation of the algorithm. In fact, the original version of `bicubic_kernel`
     * had even a slightly incorrect curve (`1/2 - x^2 + 1/2 x^3` instead of `1 - 5/2 x^2 +
     * 3/2 x^3`). This function still remains here only because we don't use OpenGL...
     */
    pub fn bicubic_interpolation(src: &SurfacePixels, dest: &mut SurfacePixels) {
        let w = dest.width as int - 1;
        let h = dest.height as int - 1;
        let ww = src.width as int - 1;
        let hh = src.height as int - 1;

        let mut dx = 0;
        let mut x = 0;
        for i in range(0, w + 1) {
            let mut dy = 0;
            let mut y = 0;
            for j in range(0, h + 1) {
                let mut r = 0;
                let mut g = 0;
                let mut b = 0;
                let a0 = [bicubic_kernel((x-1) * w - i * ww, w),
                          bicubic_kernel( x    * w - i * ww, w),
                          bicubic_kernel((x+1) * w - i * ww, w),
                          bicubic_kernel((x+2) * w - i * ww, w)];
                let a1 = [bicubic_kernel((y-1) * h - j * hh, h),
                          bicubic_kernel( y    * h - j * hh, h),
                          bicubic_kernel((y+1) * h - j * hh, h),
                          bicubic_kernel((y+2) * h - j * hh, h)];
                for k0 in range(0, 4) {
                    for k1 in range(0, 4) {
                        let xx = x + k0 - 1;
                        let yy = y + k1 - 1;
                        if 0 <= xx && xx <= ww && 0 <= yy && yy <= hh {
                            let (r2,g2,b2) = to_rgb(src.get_pixel(xx as uint, yy as uint));
                            let d = (a0[k0] * a1[k1]) >> (FP_SHIFT1*2 - FP_SHIFT2);
                            r += r2 as int * d;
                            g += g2 as int * d;
                            b += b2 as int * d;
                        }
                    }
                }

                let r = num::clamp(r >> FP_SHIFT2, 0, 255) as u8;
                let g = num::clamp(g >> FP_SHIFT2, 0, 255) as u8;
                let b = num::clamp(b >> FP_SHIFT2, 0, 255) as u8;
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

    //----------------------------------------------------------------------------------------------
    // bitmap font

    /// Bit vector which represents one row of zoomed font.
    type ZoomedFontRow = u32;

    /// 8x16 resizable bitmap font.
    pub struct Font {
        /**
         * Font data used for zoomed font reconstruction. This is actually an array of `u32`
         * elements, where the first `u16` element forms upper 16 bits and the second forms lower
         * 16 bits. It is reinterpreted for better compression. (C: `fontdata`)
         *
         * One glyph has 16 `u32` elements for each row from the top to the bottom. One `u32`
         * element contains eight four-bit groups for each column from the left (lowermost group)
         * to the right (uppermost group). Each group is a bitwise OR of following bits:
         *
         * - 1: the lower right triangle of the zoomed pixel should be drawn.
         * - 2: the lower left triangle of the zoomed pixel should be drawn.
         * - 4: the upper left triangle of the zoomed pixel should be drawn.
         * - 8: the upper right triangle of the zoomed pixel should be drawn.
         *
         * So for example, if the group bits read 3 (1+2), the zoomed pixel would be drawn
         * as follows (in the zoom factor 5):
         *
         *     .....
         *     #...#
         *     ##.##
         *     #####
         *     #####
         *
         * The group bits 15 (1+2+4+8) always draw the whole square, so in the zoom factor 1 only
         * pixels with group bits 15 will be drawn.
         */
        glyphs: ~[u16],

        /// Precalculated zoomed font per zoom factor. It is three-dimensional array which indices
        /// are zoom factor, glyph number and row respectively. Assumes that each element has
        /// at least zoom factor times 8 (columns per row) bits. (C: `zoomfont`)
        pixels: ~[~[~[ZoomedFontRow]]]
    }

    /// An alignment mode of `Font::print_string`.
    pub enum Alignment {
        /// Coordinates specify the top-left corner of the bounding box.
        LeftAligned,
        /// Coordinates specify the top-center point of the bounding box.
        Centered,
        /// Coordinates specify the top-right corner of the bounding box.
        RightAligned
    }

    /// Decompresses a bitmap font data. `Font::create_zoomed_font` is required for the actual use.
    pub fn Font() -> Font {
        // Delta-coded code words. (C: `words`)
        let dwords = [0, 2, 6, 2, 5, 32, 96, 97, 15, 497, 15, 1521, 15, 1537,
            16, 48, 176, 1, 3, 1, 3, 7, 1, 4080, 4096, 3, 1, 8, 3, 4097, 4080,
            16, 16128, 240, 1, 2, 9, 3, 8177, 15, 16385, 240, 15, 1, 47, 721,
            143, 2673, 2, 6, 7, 1, 31, 17, 16, 63, 64, 33, 0, 1, 2, 1, 8, 3];

        // LZ77-compressed indices to code words:
        // - Byte 33..97 encodes a literal code word 0..64;
        // - Byte 98..126 encodes an LZ77 length distance pair with length 3..31;
        //   the following byte 33..126 encodes a distance 1..94.
        // (C: `indices`)
        let indices =
            ~"!!7a/&/&s$7a!f!'M*Q*Qc$(O&J!!&J&Jc(e!2Q2Qc$-Bg2m!2bB[Q7Q2[e&2Q!Qi>&!&!>UT2T2&2>WT!c*\
              T2GWc8icM2U2D!.8(M$UQCQ-jab!'U*2*2*2TXbZ252>9ZWk@*!*!*8(J$JlWi@cxQ!Q!d$#Q'O*?k@e2dfe\
              jcNl!&JTLTLG_&J>]c*&Jm@cB&J&J7[e(o>pJM$Qs<7[{Zj`Jm40!3!.8(M$U!C!-oR>UQ2U2]2a9Y[S[QCQ\
              2GWk@*M*Q*B*!*!g$aQs`G8.M(U$[!Ca[o@Q2Q!IJQ!Q!c,GWk@787M6U2C2d!a[2!2k?!bnc32>[u`>Uc4d\
              @b(q@abXU!D!.8(J&J&d$q`Q2IXu`g@Q2aWQ!q@!!ktk,x@M$Qk@3!.8(M$U!H#W'O,?4m_f!7[i&n!:eX5g\
              hCk=>UQ2Q2U2Dc>J!!&J&b&k@J)LKg!GK!)7Wk@'8,M=UWCcfa[c&Q2l`f4If(Q2G[l@MSUQC!2!2c$Q:RWG\
              Ok@,[<2WfZQ2U2D2.l`a[eZ7f(!2b2|@b$j!>MSUQCc6[2W2Q:RWGOk@Q2Q2c$a[g*Ql`7[&J&Jk$7[l`!Qi\
              $d^GWk@U2D2.9([$[#['[,@<2W2k@!2!2m$a[l`:^[a[a[T2Td~c$k@d2:R[V[a@_b|o@,M=UWCgZU:EW.Ok\
              @>[g<G[!2!2d$k@Ug@Q2V2a2IW_!Wt`Ih*q`!2>WQ!Q!c,Gk_!7[&J&Jm$k@gti$m`k:U:EW.O(?s@T2Tb$a\
              [CW2Qk@M+U:^[GbX,M>U`[WCO-l@'U,D<.W(O&J&Je$k@a[Q!U!]!G8.M(U$[!Ca[k@*Q!Q!l$b2m!+!:#W'\
              O,?4!1n;c`*!*!l$h`'8,M=UWCO-pWz!a[i,#Q'O,?4~R>QQ!Q!aUQ2Q2Q2aWl=2!2!2>[e<c$G[p`dZcHd@\
              l`czi|c$al@i`b:[!2Un`>8TJTJ&J7[&b&e$o`i~aWQ!c(hd2!2!2>[g@e$k]epi|e0i!bph(d$dbGWhA2!2\
              U2D2.9(['[,@<2W2k`*J*?*!*!k$o!;[a[T2T2c$c~o@>[c6i$p@Uk>GW}`G[!2!2b$h!al`aWQ!Q!Qp`fVl\
              Zf@UWb6>eX:GWk<&J&J7[c&&JTJTb$G?o`c~i$m`k@U:EW.O(v`T2Tb$a[Fp`M+eZ,M=UWCO-u`Q:RWGO.A(\
              M$U!Ck@a[]!G8.M(U$[!Ca[i:78&J&Jc$%[g*7?e<g0w$cD#iVAg*$[g~dB]NaaPGft~!f!7[.W(O";

        /// Decompresses a font data from `dwords` and `indices`. (C: `fontdecompress`)
        fn decompress(dwords: &[u16], indices: &str) -> ~[u16] {
            let mut words = ~[0];
            for &delta in dwords.iter() {
                let last = *words.last();
                words.push(last + delta);
            }

            let nindices = indices.len();
            let mut i = 0;
            let mut glyphs = ~[];
            while i < nindices {
                let code = indices[i] as uint;
                i += 1;
                match code {
                    33..97 => { glyphs.push(words[code - 33]); }
                    98..126 => {
                        let length = code - 95; // code=98 -> length=3
                        let distance = indices[i] as uint - 32;
                        i += 1;
                        let start = glyphs.len() - distance;
                        for i in range(start, start + length) {
                            glyphs.push(glyphs[i]);
                        }
                    }
                    _ => fail!(~"unexpected codeword")
                }
            }
            glyphs
        }

        let glyphs = decompress(dwords, indices);
        assert!(glyphs.len() == 3072);
        Font { glyphs: glyphs, pixels: ~[] }
    }

    impl Font {
        /// Creates a zoomed font of scale `zoom`. (C: `fontprocess`)
        pub fn create_zoomed_font(&mut self, zoom: uint) {
            assert!(zoom > 0);
            assert!(zoom <= (8 * ::std::sys::size_of::<ZoomedFontRow>()) / 8);
            if zoom < self.pixels.len() && !self.pixels[zoom].is_empty() { return; }

            let nrows = 16;
            let nglyphs = self.glyphs.len() / nrows / 2;
            let mut pixels = vec::from_elem(nglyphs, vec::from_elem(zoom*nrows, 0u32));

            let put_zoomed_pixel = |glyph: uint, row: uint, col: uint, v: u32| {
                let zoomrow = row * zoom;
                let zoomcol = col * zoom;
                for r in range(0, zoom) {
                    for c in range(0, zoom) {
                        let mut mask = 0;
                        if r + c >= zoom    { mask |= 1; } // lower right
                        if r > c            { mask |= 2; } // lower left
                        if r < c            { mask |= 4; } // upper right
                        if r + c < zoom - 1 { mask |= 8; } // upper left

                        // if `zoom` is odd, drawing four corner triangles leaves one center pixel
                        // intact since we don't draw diagonals for aesthetic reason. such case
                        // must be specially handled.
                        if (v & mask) != 0 || v == 15 {
                            pixels[glyph][zoomrow+r] |= 1 << (zoomcol+c);
                        }
                    }
                }
            };

            let mut i = 0;
            for glyph in range(0, nglyphs) {
                for row in range(0, nrows) {
                    let data = (self.glyphs[i] as u32 << 16) | (self.glyphs[i+1] as u32);
                    i += 2;
                    for col in range(0, 8u) {
                        let v = (data >> (4 * col)) & 15;
                        put_zoomed_pixel(glyph, row, col, v);
                    }
                }
            }
            self.pixels.grow_set(zoom, &~[], pixels);
        }

        /// Prints a glyph with given position and color (possibly gradient). This method is
        /// distinct from `print_glyph` since the glyph #95 is used for the tick marker
        /// (character code -1 in C). (C: `printchar`)
        pub fn print_glyph<ColorT:Blend>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                         zoom: uint, glyph: uint, color: ColorT) {
            assert!(!self.pixels[zoom].is_empty());
            for iy in range(0, 16 * zoom) {
                let row = self.pixels[zoom][glyph][iy];
                let rowcolor = color.blend(iy as int, 16 * zoom as int);
                for ix in range(0, 8 * zoom) {
                    if ((row >> ix) & 1) != 0 {
                        pixels.put_pixel(x + ix, y + iy, rowcolor);
                    }
                }
            }
        }

        /// Prints a character with given position and color.
        pub fn print_char<ColorT:Blend>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                        zoom: uint, c: char, color: ColorT) {
            if !c.is_whitespace() {
                let c = c as uint;
                let glyph = if 32 <= c && c < 126 {c-32} else {0};
                self.print_glyph(pixels, x, y, zoom, glyph, color);
            }
        }

        /// Prints a string with given position, alignment and color. (C: `printstr`)
        pub fn print_string<ColorT:Blend>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                          zoom: uint, align: Alignment, s: &str, color: ColorT) {
            let mut x = match align {
                LeftAligned  => x,
                Centered     => x - s.char_len() * (8 * zoom) / 2,
                RightAligned => x - s.char_len() * (8 * zoom),
            };
            for c in s.iter() {
                let nextx = x + 8 * zoom;
                if nextx >= pixels.width { break; }
                self.print_char(pixels, x, y, zoom, c, color.clone());
                x = nextx;
            }
        }
    }

    //----------------------------------------------------------------------------------------------

}

//==================================================================================================
// game play

/**
 * Game play logics. This module contains whooping 2000+ lines of code, reflecting the fact that
 * Angolmois is not well refactored. (In fact, the game logic is usually hard to refactor, right?)
 */
pub mod player {
    use std::{vec, cmp, num, iter};
    use sdl::*;
    use sdl::video::*;
    use sdl::event::*;
    use sdl::mixer::*;
    use util::sdl::mixer::*;
    use util::sdl::mpeg::*;
    use parser::*;
    use gfx::*;

    /// The width of screen, unless the exclusive mode.
    pub static SCREENW: uint = 800;
    /// The height of screen, unless the exclusive mode.
    pub static SCREENH: uint = 600;
    /// The width of BGA, or the width of screen for the exclusive mode.
    pub static BGAW: uint = 256;
    /// The height of BGA, or the height of screen for the exclusive mode.
    pub static BGAH: uint = 256;

    //----------------------------------------------------------------------------------------------
    // options

    /// Game play modes. (C: `enum mode`)
    #[deriving(Eq)]
    pub enum Mode {
        /// Normal game play. The graphical display and input is enabled. (C: `PLAY_MODE`)
        PlayMode,
        /// Automatic game play. The graphical display is enabled but the input is mostly ignored
        /// except for the play speed change. (C: `AUTOPLAY_MODE`)
        AutoPlayMode,
        /// Exclusive (headless) mode. The graphical display is reduced to the BGA or absent at all
        /// (when `NoBga` is also set). (C: `EXCLUSIVE_MODE`)
        ExclusiveMode
    }

    /// Modifiers that affect the game data. (C: `enum modf`)
    #[deriving(Eq)]
    pub enum Modf {
        /// Swaps all "key" (i.e. `KeyKind::counts_as_key` returns true) lanes in the reverse order.
        /// See `player::apply_mirror_modf` for the detailed algorithm. (C: `MIRROR_MODF`)
        MirrorModf,
        /// Swaps all "key" lanes in the random order. See `player::apply_shuffle_modf` for
        /// the detailed algorithm. (C: `SHUFFLE_MODF`)
        ShuffleModf,
        /// Swaps all lanes in the random order. (C: `SHUFFLEEX_MODF`)
        ShuffleExModf,
        /// Swaps all "key" lanes in the random order, where the order is determined per object.
        /// See `player::apply_random_modf` for the detailed algorithm. (C: `RANDOM_MODF`)
        RandomModf,
        /// Swaps all lanes in the random order, where the order is determined per object.
        /// (C: `RANDOMEX_MODF`)
        RandomExModf
    }

    /// Specifies how the BGA is displayed. (C: `enum bga`)
    #[deriving(Eq)]
    pub enum Bga {
        /// Both the BGA image and movie is displayed. (C: `BGA_AND_MOVIE`)
        BgaAndMovie,
        /// The BGA is displayed but the movie is not loaded. (C: `BGA_BUT_NO_MOVIE`)
        BgaButNoMovie,
        /// The BGA is not displayed. When used with `ExclusiveMode` it also disables the graphical
        /// display entirely. (C: `NO_BGA`)
        NoBga
    }

    /// Global options set from the command line and environment variables.
    pub struct Options {
        /// A path to the BMS file. Used for finding the resource when `BMS::basepath` is not set.
        /// (C: `bmspath`)
        bmspath: ~str,
        /// Game play mode. (C: `opt_mode`)
        mode: Mode,
        /// Modifiers that affect the game data. (C: `opt_modf`)
        modf: Option<Modf>,
        /// Specifies how the BGA is displayed. (C: `opt_bga`)
        bga: Bga,
        /// True if the metadata (either overlaid in the loading screen or printed separately
        /// in the console) is displayed. (C: `opt_showinfo`)
        showinfo: bool,
        /// True if the full screen is enabled. (C: `opt_fullscreen`)
        fullscreen: bool,
        /// An index to the joystick device if any. (C: `opt_joystick`)
        joystick: Option<uint>,
        /// A key specification preset name if any. (C: `preset`)
        preset: Option<~str>,
        /// A left-hand-side key specification if any. (C: `leftkeys`)
        leftkeys: Option<~str>,
        /// A right-hand-side key specification if any. Can be an empty string. (C: `rightkeys`)
        rightkeys: Option<~str>,
        /// An initial play speed. (C: `playspeed`)
        playspeed: float,
    }

    impl Options {
        /// Returns true if the exclusive mode is enabled. This enables a text-based interface.
        /// (C: `opt_mode >= EXCLUSIVE_MODE`)
        pub fn is_exclusive(&self) -> bool { self.mode == ExclusiveMode }

        /// Returns true if the input is ignored. Escape key or speed-changing keys are still
        /// available as long as the graphical screen is enabled. (C: `!!opt_mode`)
        pub fn is_autoplay(&self) -> bool { self.mode != PlayMode }

        /// Returns true if the BGA is displayed. (C: `opt_bga < NO_BGA`)
        pub fn has_bga(&self) -> bool { self.bga != NoBga }

        /// Returns true if the BGA movie is enabled. (C: `opt_bga < BGA_BUT_NO_MOVIE`)
        pub fn has_movie(&self) -> bool { self.bga == BgaAndMovie }

        /// Returns true if the graphical screen is enabled.
        /// (C: `opt_mode < EXCLUSIVE_MODE || opt_bga < NO_BGA`)
        pub fn has_screen(&self) -> bool { !self.is_exclusive() || self.has_bga() }
    }

    //----------------------------------------------------------------------------------------------
    // bms utilities

    /// Parses a key specification from the options.
    pub fn key_spec(bms: &Bms, opts: &Options) -> Result<~KeySpec,~str> {
        use util::str::StrUtil;

        let (leftkeys, rightkeys) =
            if opts.leftkeys.is_none() && opts.rightkeys.is_none() {
                let preset =
                    if opts.preset.is_none() && opts.bmspath.to_ascii_lower().ends_with(".pms") {
                        Some(~"pms")
                    } else {
                        opts.preset.clone()
                    };
                match preset_to_key_spec(bms, preset) {
                    Some(leftright) => leftright,
                    None => {
                        return Err(fmt!("Invalid preset name: %s",
                                        opts.preset.map_default(~"", |v| v.clone())));
                    }
                }
            } else {
                // Rust: `Option` of managed pointer is not easy to use due to
                //       implicit move. `Option<T>::clone_default` maybe?
                (opts.leftkeys.map_default(~"", |v| v.clone()),
                 opts.rightkeys.map_default(~"", |v| v.clone()))
            };

        let mut keyspec = ~KeySpec { split: 0, order: ~[], kinds: ~[None, ..NLANES] };
        let parse_and_add = |keys: &str| -> Option<uint> {
            match parse_key_spec(keys) {
                None | Some([]) => None,
                Some(left) => {
                    let mut err = false;
                    for &(lane,kind) in left.iter() {
                        if keyspec.kinds[*lane].is_some() { err = true; break; }
                        keyspec.order.push(lane);
                        keyspec.kinds[*lane] = Some(kind);
                    }
                    if err {None} else {Some(left.len())}
                }
            }
        };

        if !leftkeys.is_empty() {
            match parse_and_add(leftkeys) {
                None => { return Err(fmt!("Invalid key spec for left hand side: %s", leftkeys)); }
                Some(nkeys) => { keyspec.split += nkeys; }
            }
        } else {
            return Err(fmt!("No key model is specified using -k or -K"));
        }
        if !rightkeys.is_empty() {
            match parse_and_add(rightkeys) {
                None => { return Err(fmt!("Invalid key spec for right hand side: %s", rightkeys)); }
                Some(nkeys) => { // no split panes except for #PLAYER 2
                    if bms.player != CouplePlay { keyspec.split += nkeys; }
                }
            }
        }
        Ok(keyspec)
    }

    /// Applies given modifier to the game data. The target lanes of the modifier is determined
    /// from given key specification. This function should be called twice for the Couple Play,
    /// since 1P and 2P should be treated separately. (C: `shuffle_bms`)
    pub fn apply_modf<R: ::std::rand::Rng>(bms: &mut Bms, modf: Modf, r: &mut R,
                                           keyspec: &KeySpec, begin: uint, end: uint) {
        let mut lanes = ~[];
        for i in range(begin, end) {
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

    //----------------------------------------------------------------------------------------------
    // utilities

    /// Checks if the user pressed the escape key or the quit button. `atexit` is called before
    /// the program is terminated. (C: `check_exit`)
    pub fn check_exit(atexit: &fn()) {
        loop {
            match poll_event() {
                KeyEvent(EscapeKey,_,_,_) | QuitEvent => {
                    atexit();
                    ::util::exit(0);
                },
                NoEvent => { break; },
                _ => {}
            }
        }
    }

    /// Writes a line to the console without advancing to the next line. `s` should be short enough
    /// to be replaced (currently up to 72 bytes).
    pub fn update_line(s: &str) {
        ::std::io::stderr().write_str(fmt!("\r%s\r%s", " ".repeat(72), s));
    }

    /// A periodic timer for thresholding the rate of information display.
    pub struct Ticker {
        /// Minimal required milliseconds after the last display.
        interval: uint,
        /// The timestamp at the last display. It is a return value from `sdl::get_ticks` and
        /// measured in milliseconds. May be a `None` if the ticker is at the initial state or
        /// has been reset by `reset` method. (C: `lastinfo`)
        lastinfo: Option<uint>
    }

    /// Returns a new ticker with a default display interval.
    pub fn Ticker() -> Ticker {
        /// A reasonable interval for the console and graphic display. Currently set to about 21fps.
        /// (C: `INFO_INTERVAL`)
        static INFO_INTERVAL: uint = 47;
        Ticker { interval: INFO_INTERVAL, lastinfo: None }
    }

    impl Ticker {
        /// Calls `f` only when required milliseconds have passed after the last display.
        /// `now` should be a return value from `sdl::get_ticks`.
        pub fn on_tick(&mut self, now: uint, f: &fn()) {
            if self.lastinfo.map_default(true, |&t| now - t >= self.interval) {
                self.lastinfo = Some(now);
                f();
            }
        }

        /// Lets the next call to `on_tick` always call the callback.
        pub fn reset(&mut self) {
            self.lastinfo = None;
        }
    }

    //----------------------------------------------------------------------------------------------
    // initialization

    /// An internal sampling rate for SDL_mixer. Every chunk loaded is first converted to
    /// this sampling rate for the purpose of mixing.
    static SAMPLERATE: i32 = 44100;

    /// The number of bytes in the chunk converted to an internal sampling rate.
    static BYTESPERSEC: i32 = SAMPLERATE * 2 * 2; // stereo, 16 bits/sample

    /// Creates a small screen for BGAs (`BGAW` by `BGAH` pixels) if `exclusive` is set,
    /// or a full-sized screen (`SCREENW` by `SCREENH` pixels) otherwise. `fullscreen` is ignored
    /// when `exclusive` is set. (C: `init_video`)
    pub fn init_video(exclusive: bool, fullscreen: bool) -> ~Surface {
        let result =
            if exclusive {
                set_video_mode(BGAW as int, BGAH as int, 32, [SWSurface], [DoubleBuf])
            } else if !fullscreen {
                set_video_mode(SCREENW as int, SCREENH as int, 32, [SWSurface], [DoubleBuf])
            } else {
                set_video_mode(SCREENW as int, SCREENH as int, 32, [], [Fullscreen])
            };
        let screen =
            match result {
                Ok(screen) => screen,
                Err(err) => die!("SDL Video Initialization Failure: %s", err)
            };
        if !exclusive {
            mouse::set_cursor_visible(false);
        }
        wm::set_caption(::version(), "");
        screen
    }

    /// Initializes an SDL, SDL_image and SDL_mixer. (C: `init_ui`)
    pub fn init_sdl() {
        if !init([InitVideo, InitAudio, InitJoystick]) {
            die!("SDL Initialization Failure: %s", get_error());
        }
        img::init([img::InitJPG, img::InitPNG]);
        //mixer::init([mixer::InitOGG, mixer::InitMP3]); // TODO
        if mixer::open(SAMPLERATE, audio::S16_AUDIO_FORMAT, audio::Stereo, 2048).is_err() {
            die!("SDL Mixer Initialization Failure");
        }
    }

    /// Initializes a joystick with given index.
    pub fn init_joystick(joyidx: uint) -> ~joy::Joystick {
        unsafe {
            joy::ll::SDL_JoystickEventState(1); // TODO rust-sdl patch
        }
        match joy::Joystick::open(joyidx as int) {
            Ok(joy) => joy,
            Err(err) => die!("SDL Joystick Initialization Failure: %s", err)
        }
    }

    //----------------------------------------------------------------------------------------------
    // virtual input

    /// Actual input. Mapped to zero or more virtual inputs by input mapping.
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
        fn iter_bytes(&self, lsb0: bool, f: ::std::to_bytes::Cb) -> bool {
            match *self {
                KeyInput(key) => // XXX #7363
                    0u8.iter_bytes(lsb0, |b| f(b)) && (key as uint).iter_bytes(lsb0, |b| f(b)),
                JoyAxisInput(axis) => // XXX #7363
                    1u8.iter_bytes(lsb0, |b| f(b)) && axis.iter_bytes(lsb0, |b| f(b)),
                JoyButtonInput(button) => // XXX #7363
                    2u8.iter_bytes(lsb0, |b| f(b)) && button.iter_bytes(lsb0, |b| f(b)),
            }
        }
    }

    /// Virtual input.
    #[deriving(Eq)]
    enum VirtualInput {
        /// Virtual input mapped to the lane.
        LaneInput(Lane),
        /// Speed down input (normally F3).
        SpeedDownInput,
        /// Speed up input (normally F4).
        SpeedUpInput
    }

    /**
     * State of virtual input elements. There are three states: neutral, and positive or negative.
     * There is no difference between positive and negative states (the naming is arbitrary)
     * except for that they are distinct.
     *
     * The states should really be one of pressed (non-neutral) or unpressed (neutral) states,
     * but we need two non-neutral states since the actual input device with continuous values
     * (e.g. joystick axes) can trigger the state transition *twice* without hitting the neutral
     * state. We solve this problem by making the transition from negative to positive (and vice
     * versa) temporarily hit the neutral state.
     */
    #[deriving(Eq)]
    enum InputState {
        /// Positive input state. Occurs when the button is pressed or the joystick axis is moved
        /// in the positive direction.
        Positive = 1,
        /// Neutral input state. Occurs when the button is not pressed or the joystick axis is moved
        /// back to the origin.
        Neutral = 0,
        /// Negative input state. Occurs when the joystick axis is moved in the negative direction.
        Negative = -1
    }

    impl VirtualInput {
        /// Returns true if the virtual input has a specified key kind in the key specification.
        pub fn active_in_key_spec(&self, kind: KeyKind, keyspec: &KeySpec) -> bool {
            match *self {
                LaneInput(Lane(lane)) => keyspec.kinds[lane] == Some(kind),
                SpeedDownInput | SpeedUpInput => true
            }
        }
    }

    /// An information about an environment variable for multiple keys.
    //
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

    /// A list of environment variables that set the mapping for multiple keys, and corresponding
    /// default values and the order of keys. (C: `envvars`)
    static KEYSETS: &'static [KeySet] = &[
        (/*KeySet { envvar:*/ &"ANGOLMOIS_1P_KEYS",
                 /*default:*/ &"left shift%axis 3|z%button 3|s%button 6|x%button 2|d%button 7|\
                            c%button 1|f%button 4|v%axis 2|left alt",
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
                            (Some(Button4), &[LaneInput(Lane(8)), LaneInput(Lane(36+2))]),
                            (Some(Button3), &[LaneInput(Lane(9)), LaneInput(Lane(36+3))]),
                            (Some(Button2), &[LaneInput(Lane(6)), LaneInput(Lane(36+4))]),
                            (Some(Button1), &[LaneInput(Lane(7)), LaneInput(Lane(36+5))])] ),
        (/*KeySet { envvar:*/ &"ANGOLMOIS_SPEED_KEYS",
                 /*default:*/ &"f3|f4",
                 /*mapping:*/ &[(None, &[SpeedDownInput]),
                            (None, &[SpeedUpInput])] ),
    ];

    /// An input mapping, i.e. a mapping from the actual input to the virtual input.
    pub type KeyMap = ::std::hashmap::HashMap<Input,VirtualInput>;

    /// Reads an input mapping from the environment variables. (C: `read_keymap`)
    pub fn read_keymap(keyspec: &KeySpec, getenv: &fn(&str) -> Option<~str>) -> KeyMap {
        use util::str::StrUtil;

        /// Finds an SDL virtual key with the given name. Matching is done case-insensitively.
        fn sdl_key_from_name(name: &str) -> Option<event::Key> {
            let name = name.to_ascii_lower();
            unsafe {
                let firstkey = 0;
                let lastkey = ::std::cast::transmute(event::LastKey);
                for keyidx in range(firstkey, lastkey) {
                    let key = ::std::cast::transmute(keyidx);
                    let keyname = event::get_key_name(key).to_ascii_lower();
                    if keyname == name { return Some(key); }
                }
            }
            None
        }

        /// Parses an `Input` value from the string. E.g. `"backspace"`, `"button 2"` or `"axis 0"`.
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

        let mut map = ::std::hashmap::HashMap::new();
        let add_mapping = |kind: Option<KeyKind>, input: Input, vinput: VirtualInput| {
            if kind.map_default(true, |&kind| vinput.active_in_key_spec(kind, keyspec)) {
                map.insert(input, vinput);
            }
        };

        for &keyset in KEYSETS.iter() {
            let (envvar, default, mapping) = keyset; // XXX
            let spec = getenv(/*keyset.*/envvar);
            let spec = spec.unwrap_or(/*keyset.*/default.to_owned());

            let mut i = 0;
            for part in spec.split_iter('|') {
                let (kind, vinputs) = /*keyset.*/mapping[i];
                for s in part.split_iter('%') {
                    match parse_input(s) {
                        Some(input) => {
                            for &vinput in vinputs.iter() {
                                add_mapping(kind, input, vinput);
                            }
                        }
                        None => die!("Unknown key name in the environment \
                                      variable %s: %s", /*keyset.*/envvar, s)
                    }
                }

                i += 1;
                if i >= /*keyset.*/mapping.len() { break; }
            }
        }

        for &lane in keyspec.order.iter() {
            let key = Key(36 + *lane as int);
            let kind = keyspec.kinds[*lane].unwrap();
            let envvar = fmt!("ANGOLMOIS_%s%c_KEY", key.to_str(), kind.to_char());
            let val = getenv(envvar); // XXX #3511
            for s in val.iter() {
                match parse_input(*s) {
                    Some(input) => { add_mapping(Some(kind), input, LaneInput(lane)); }
                    None => die!("Unknown key name in the environment variable %s: %s", envvar, *s)
                }
            }
        }

        map
    }

    //----------------------------------------------------------------------------------------------
    // resource management

    /// Alternative file extensions for sound resources. (C: `SOUND_EXTS`)
    static SOUND_EXTS: &'static [&'static str] = &[".WAV", ".OGG", ".MP3"];
    /// Alternative file extensions for image resources. (C: `IMAGE_EXTS`)
    static IMAGE_EXTS: &'static [&'static str] = &[".BMP", ".PNG", ".JPG", ".JPEG", ".GIF"];

    /// Returns a specified or implied resource directory from the BMS file.
    fn get_basedir(bms: &Bms, opts: &Options) -> Path {
        // TODO this logic assumes that #PATH_WAV is always interpreted as a native path, which
        // the C version doesn't assume. this difference barely makes the practical issue though.
        match bms.basepath {
            Some(ref basepath) => { let basepath: &str = *basepath; Path(basepath) }
            None => Path(opts.bmspath).dir_path()
        }
    }

    /**
     * Resolves the specified resource path to the actual path if possible. May fail, but its
     * success doesn't guarantee that the resource should be read without a failure either.
     * (C: `resolve_relative_path`)
     *
     * The actual resolution is complicated by the fact that many BMSes assume the case-insensitive
     * matching on file names and the coexistence between WAV resources and MP3 resources while
     * keeping the same BMS file. Therefore Angolmois adopted the following resolution rules:
     *
     * 1. Both `/` and `\` are accepted as a directory separator.
     * 2. Path components including file names are matched case-insensitively. If there are multiple
     *    matches then any one can be used, even when a better match exists.
     * 3. If the initial match on the file name fails, and the file name does contain an extension,
     *    then a list of alternative extensions is applied with the same matching procedure.
     */
    fn resolve_relative_path(basedir: &Path, path: &str, exts: &[&str]) -> Option<Path> {
        use std::os::{path_is_dir, list_dir};
        use util::str::StrUtil;

        let mut parts = ~[];
        for part in path.split_iter(|c: char| c == '/' || c == '\\') {
            if part.is_empty() { loop; }
            parts.push(part);
        }
        if parts.is_empty() { return None; }

        let mut cur = basedir.clone();
        let lastpart = parts.pop();
        for part in parts.iter() {
            // early exit if the intermediate path does not exist or is not a directory
            if !path_is_dir(&cur) { return None; }

            let part = part.to_ascii_upper();
            let mut found = false;
            let entries = list_dir(&cur); // XXX #3511
            for next in entries.iter() {
                if ".".equiv(next) || "..".equiv(next) { loop; }
                if next.to_ascii_upper() == part {
                    cur = cur.push(*next);
                    found = true;
                    break;
                }
            }
            if !found { return None; }
        }

        if !path_is_dir(&cur) { return None; }

        let lastpart = lastpart.to_ascii_upper();
        let entries = list_dir(&cur); // XXX #3511
        for next in entries.iter() {
            if ".".equiv(next) || "..".equiv(next) { loop; }
            let next_ = next.to_ascii_upper();
            let mut found = (next_ == lastpart);
            if !found {
                match next_.rfind('.') {
                    Some(idx) => {
                        let nextnoext = next_.slice_to(idx).to_owned();
                        for ext in exts.iter() {
                            if nextnoext + ext.to_owned() == lastpart {
                                found = true;
                                break;
                            }
                        }
                    }
                    None => {} // does not try alternative extensions if there was no extension
                }
            }
            if found {
                return Some(cur.push(*next));
            }
        }

        None
    }

    /// Sound resource associated to `SoundRef`. It contains the actual SDL_mixer chunk that can be
    /// readily played. (C: the type of `sndres`)
    enum SoundResource {
        /// No sound resource is associated, or error occurred while loading.
        NoSound,
        /// Sound resource is associated.
        //
        // Rust: ideally this should be just a ~-ptr, but the current borrowck is very constrained
        //       in this aspect. after several attempts I finally sticked to delegate the ownership
        //       to a managed box.
        Sound(@~Chunk) // XXX borrowck
    }

    impl SoundResource {
        /// Returns the associated chunk if any.
        pub fn chunk(&self) -> Option<@~Chunk> {
            match *self {
                NoSound => None,
                Sound(chunk) => Some(chunk)
            }
        }

        /// Returns the length of associated sound chunk in seconds. This is used for determining
        /// the actual duration of the song in presence of key and background sounds, so it may
        /// return 0.0 if no sound is present.
        pub fn duration(&self) -> float {
            match *self {
                NoSound => 0.0,
                Sound(chunk) => {
                    let chunk = chunk.to_ll_chunk();
                    (unsafe {(*chunk).alen} as float) / (BYTESPERSEC as float)
                }
            }
        }
    }

    /// Loads a sound resource.
    fn load_sound(key: Key, path: &str, basedir: &Path) -> SoundResource {
        let res = match resolve_relative_path(basedir, path, SOUND_EXTS) {
            Some(fullpath) => Chunk::from_wav(&fullpath),
            None => Err(~"not found")
        };
        match res {
            Ok(res) => Sound(@res),
            Err(_) => {
                warn!("failed to load sound #WAV%s (%s)", key.to_str(), path);
                NoSound
            }
        }
    }

    /// Image resource associated to `ImageRef`. It can be either a static image or a movie, and
    /// both contains an SDL surface that can be blitted to the screen. (C: the type of `imgres`)
    enum ImageResource {
        /// No image resource is associated, or error occurred while loading.
        NoImage,
        /// A static image is associated. The surface may have a transparency which is already
        /// handled by `load_image`.
        Image(@~Surface), // XXX borrowck
        /// A movie is associated. A playback starts when `start_movie` method is called, and stops
        /// when `stop_movie` is called. An associated surface is updated from the separate thread
        /// during the playback.
        Movie(@~Surface, @~MPEG) // XXX borrowck
    }

    impl ImageResource {
        /// Returns an associated surface if any.
        pub fn surface(&self) -> Option<@~Surface> {
            match *self {
                NoImage => None,
                Image(surface) | Movie(surface,_) => Some(surface)
            }
        }

        /// Stops the movie playback if possible.
        pub fn stop_movie(&self) {
            match *self {
                NoImage | Image(_) => {}
                Movie(_,mpeg) => { mpeg.stop(); }
            }
        }

        /// Starts (or restarts, if the movie was already being played) the movie playback
        /// if possible.
        pub fn start_movie(&self) {
            match *self {
                NoImage | Image(_) => {}
                Movie(_,mpeg) => { mpeg.rewind(); mpeg.play(); }
            }
        }
    }

    /// Loads an image resource.
    fn load_image(key: Key, path: &str, opts: &Options, basedir: &Path) -> ImageResource {
        use util::str::StrUtil;

        /// Converts a surface to the native display format, while preserving a transparency or
        /// setting a color key if required.
        fn to_display_format(surface: ~Surface) -> Result<~Surface,~str> {
            if unsafe {(*(*surface.raw).format).Amask} != 0 {
                let res = surface.display_format_alpha();
                for surface in res.iter() {
                    surface.set_alpha([SrcAlpha, RLEAccel], 255);
                }
                res
            } else {
                let res = surface.display_format();
                for surface in res.iter() {
                    surface.set_color_key([SrcColorKey, RLEAccel], RGB(0,0,0));
                }
                res
            }
        }

        if path.to_ascii_lower().ends_with(".mpg") {
            if opts.has_movie() {
                let res = match resolve_relative_path(basedir, path, []) {
                    Some(fullpath) => MPEG::from_path(&fullpath),
                    None => Err(~"not found")
                };
                match res {
                    Ok(movie) => {
                        let surface = @new_surface(BGAW, BGAH);
                        movie.enable_video(true);
                        movie.set_loop(true);
                        movie.set_display(*surface);
                        return Movie(surface, @movie);
                    }
                    Err(_) => { warn!("failed to load image #BMP%s (%s)", key.to_str(), path); }
                }
            }
        } else if opts.has_bga() {
            let res = match resolve_relative_path(basedir, path, IMAGE_EXTS) {
                Some(fullpath) =>
                    do img::load(&fullpath).and_then |surface| {
                        do to_display_format(surface).and_then |surface| {
                            Ok(Image(@surface))
                        }
                    },
                None => Err(~"not found")
            };
            match res {
                Ok(res) => { return res; },
                Err(_) => { warn!("failed to load image #BMP%s (%s)", key.to_str(), path); }
            }
        }
        NoImage
    }

    /// Applies the blit command to given list of image resources. (C: a part of `load_resource`)
    fn apply_blitcmd(imgres: &mut [ImageResource], bc: &BlitCmd) {
        let origin: @~Surface = match imgres[**bc.src] {
            Image(src) => src,
            _ => { return; }
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
            _ => { return; }
        };

        let x1 = cmp::max(bc.x1, 0);
        let y1 = cmp::max(bc.y1, 0);
        let x2 = cmp::min(bc.x2, bc.x1 + BGAW as int);
        let y2 = cmp::min(bc.y2, bc.y1 + BGAH as int);
        target.blit_area(*origin, (x1,y1), (bc.dx,bc.dy), (x2-x1,y2-y1));
    }

    /// A list of image references displayed in BGA layers (henceforth the BGA state). Not all image
    /// referenced here is directly rendered, but the references themselves are kept.
    type BGAState = [Option<ImageRef>, ..NLAYERS];

    /// Returns the initial BGA state. Note that merely setting a particular layer doesn't start
    /// the movie playback; `poorbgafix` in `parser::parse` function handles it.
    fn initial_bga_state() -> BGAState {
        [None, None, None, Some(ImageRef(Key(0)))]
    }

    /// A trait for BGA state.
    trait BGAStateOps {
        /// Updates the BGA state. This method prepares given image resources for the next
        /// rendering, notably by starting and stopping the movie playback.
        fn update(&mut self, current: &BGAState, imgres: &[ImageResource]);
        /// Renders the image resources for the specified layers to the specified region of
        /// `screen`.
        fn render(&self, screen: &Surface, layers: &[BGALayer], imgres: &[ImageResource],
                  x: uint, y: uint);
    }

    impl BGAStateOps for BGAState {
        fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
            for layer in range(0, NLAYERS) {
                // TODO this design can't handle the case that a BGA layer is updated to the same
                // image reference, which should rewind the movie playback. the original Angolmois
                // does handle it.
                if self[layer] != current[layer] {
                    for &iref in self[layer].iter() {
                        imgres[**iref].stop_movie();
                    }
                    for &iref in current[layer].iter() {
                        imgres[**iref].start_movie();
                    }
                }
            }
            *self = *current;
        }

        fn render(&self, screen: &Surface, layers: &[BGALayer], imgres: &[ImageResource],
                  x: uint, y: uint) {
            screen.fill_area((x,y), (256,256), RGB(0,0,0));
            for &layer in layers.iter() {
                for &iref in self[layer as uint].iter() {
                    let surface = imgres[**iref].surface(); // XXX #3511
                    for &surface in surface.iter() {
                        screen.blit_area(&**surface, (0,0), (x,y), (256,256));
                    }
                }
            }
        }
    }

    //----------------------------------------------------------------------------------------------
    // loading

    /// Returns the interface string common to the graphical and textual loading screen.
    fn displayed_info(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec) -> (~str, ~str, ~str, ~str) {
        let meta = fmt!("Level %d | BPM %.2f%s | %d note%s [%uKEY%s]",
                        bms.playlevel, *bms.initbpm, if infos.hasbpmchange {~"?"} else {~""},
                        infos.nnotes, if infos.nnotes == 1 {~""} else {~"s"}, keyspec.nkeys(),
                        if infos.haslongnote {~"-LN"} else {~""});
        let title = bms.title.clone().unwrap_or(~"");
        let genre = bms.genre.clone().unwrap_or(~"");
        let artist = bms.artist.clone().unwrap_or(~"");
        (meta, title, genre, artist)
    }

    /// Renders the graphical loading screen by blitting BMS #STAGEFILE image (if any) and showing
    /// the metadata. (C: `play_show_stagefile` when `opt_mode < EXCLUSIVE_MODE`)
    pub fn show_stagefile_screen(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec, opts: &Options,
                                 screen: &Surface, font: &Font) {
        let (meta, title, genre, artist) = displayed_info(bms, infos, keyspec);

        do screen.with_pixels |pixels| {
            font.print_string(pixels, SCREENW/2, SCREENH/2-16, 2, Centered, "loading bms file...",
                              Gradient(RGB(0x80,0x80,0x80), RGB(0x20,0x20,0x20)));
        }
        screen.flip();

        do screen.with_pixels |pixels| {
            for path in bms.stagefile.iter() {
                let basedir = get_basedir(bms, opts);
                let resolved = resolve_relative_path(&basedir, *path, IMAGE_EXTS); // XXX #3511
                for path in resolved.iter() {
                    match img::load(path).and_then(|s| s.display_format()) {
                        Ok(surface) => {
                            do surface.with_pixels |srcpixels| {
                                bicubic_interpolation(srcpixels, pixels);
                            }
                        }
                        Err(_) => {}
                    }
                }
            }

            if opts.showinfo {
                let bg = RGBA(0x10,0x10,0x10,0x40);
                let fg = Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80));
                for i in range(0, SCREENW) {
                    for j in range(0, 42u) {
                        pixels.put_blended_pixel(i, j, bg);
                    }
                    for j in range(SCREENH-20, SCREENH) {
                        pixels.put_blended_pixel(i, j, bg);
                    }
                }
                font.print_string(pixels, 6, 4, 2, LeftAligned, title, fg);
                font.print_string(pixels, SCREENW-8, 4, 1, RightAligned, genre, fg);
                font.print_string(pixels, SCREENW-8, 20, 1, RightAligned, artist, fg);
                font.print_string(pixels, 3, SCREENH-18, 1, LeftAligned, meta, fg);
            }
        }

        screen.flip();
    }

    /// Renders the textual loading screen by printing the metadata.
    /// (C: `play_show_stagefile` when `opt_mode >= EXCLUSIVE_MODE`)
    pub fn show_stagefile_noscreen(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec, opts: &Options) {
        if opts.showinfo {
            let (meta, title, genre, artist) = displayed_info(bms, infos, keyspec);
            ::std::io::stderr().write_line(fmt!("\
----------------------------------------------------------------------------------------------
Title:    %s\nGenre:    %s\nArtist:   %s\n%s
----------------------------------------------------------------------------------------------",
                title, genre, artist, meta));
        }
    }

    /// Loads the image and sound resources and calls a callback whenever a new resource has been
    /// loaded. (C: `load_resource`)
    pub fn load_resource(bms: &Bms, opts: &Options,
                         callback: &fn(Option<~str>)) -> (~[SoundResource], ~[ImageResource]) {
        let basedir = get_basedir(bms, opts);

        let sndres: ~[SoundResource] =
            do bms.sndpath.iter().enumerate().map |(i, path)| {
                match *path {
                    Some(ref path) => {
                        callback(Some(path.clone()));
                        load_sound(Key(i as int), path.clone(), &basedir)
                    },
                    None => NoSound
                }
            }.collect();
        let mut imgres: ~[ImageResource] =
            do bms.imgpath.iter().enumerate().map |(i, path)| {
                match *path {
                    Some(ref path) => {
                        callback(Some(path.clone()));
                        load_image(Key(i as int), path.clone(), opts, &basedir)
                    },
                    None => NoImage
                }
            }.collect();

        for bc in bms.blitcmd.iter() {
            apply_blitcmd(imgres, bc);
        }
        (sndres, imgres)
    }

    /// Saves a portion of the screen for the use in `graphic_update_status`.
    pub fn save_screen_for_loading(screen: &Surface) -> ~Surface {
        let saved_screen = new_surface(SCREENW, 20);
        saved_screen.blit_area(screen, (0,SCREENH-20), (0,0), (SCREENW,20));
        saved_screen
    }

    /// A callback template for `load_resource` with the graphical loading screen.
    /// (C: `resource_loaded`)
    pub fn graphic_update_status(path: Option<~str>, screen: &Surface, saved_screen: &Surface,
                                 font: &Font, ticker: &mut Ticker, atexit: &fn()) {
        // Rust: `on_tick` calls the closure at most once so `path` won't be referenced twice,
        //       but the analysis can't reason that. (#4654) an "option dance" via
        //       `Option<T>::swap_unwrap` is not helpful here since `path` can be `None`.
        let mut path = path; // XXX #4654
        do ticker.on_tick(get_ticks()) {
            let path = ::std::util::replace(&mut path, None); // XXX #4654
            let msg = path.unwrap_or(~"loading...");
            screen.blit_at(saved_screen, 0, (SCREENH-20) as i16);
            do screen.with_pixels |pixels| {
                font.print_string(pixels, SCREENW-3, SCREENH-18, 1, RightAligned, msg,
                                  Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x80,0x80,0x80)));
            }
            screen.flip();
        }
        check_exit(atexit);
    }

    /// A callback template for `load_resource` with the textual loading screen.
    /// (C: `resource_loaded`)
    pub fn text_update_status(path: Option<~str>, ticker: &mut Ticker, atexit: &fn()) {
        let mut path = path; // XXX #4654
        do ticker.on_tick(get_ticks()) {
            match ::std::util::replace(&mut path, None) { // XXX #4654
                Some(path) => {
                    use util::str::StrUtil;
                    let path = if path.len() < 63 {path} else {path.slice_upto(0, 63).to_owned()};
                    update_line(~"Loading: " + path);
                }
                None => { update_line("Loading done."); }
            }
        }
        check_exit(atexit);
    }

    //----------------------------------------------------------------------------------------------
    // pointers

    /// A pointer to the object. A pointer is used to implement common operations, e.g. iterating
    /// until given position, or finding the closest object with given condition. A pointer can also
    /// be used like an object when it points to the valid object.
    struct Pointer {
        /// A BMS data holding objects.
        bms: @mut ~Bms,
        /// The current position. Can be the past-the-end value.
        pos: uint
    }

    /// Returns true if two pointers share the common BMS data.
    fn has_same_bms(lhs: &Pointer, rhs: &Pointer) -> bool {
        ::std::managed::mut_ptr_eq(lhs.bms, rhs.bms)
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


    impl Clone for Pointer {
        fn clone(&self) -> Pointer {
            Pointer { bms: self.bms, pos: self.pos }
        }
    }

    impl ObjQueryOps for Pointer {
        fn is_visible(self) -> bool { self.bms.objs[self.pos].is_visible() }
        fn is_invisible(self) -> bool { self.bms.objs[self.pos].is_invisible() }
        fn is_lnstart(self) -> bool { self.bms.objs[self.pos].is_lnstart() }
        fn is_lndone(self) -> bool { self.bms.objs[self.pos].is_lndone() }
        fn is_ln(self) -> bool { self.bms.objs[self.pos].is_ln() }
        fn is_bomb(self) -> bool { self.bms.objs[self.pos].is_bomb() }
        fn is_soundable(self) -> bool { self.bms.objs[self.pos].is_soundable() }
        fn is_gradable(self) -> bool { self.bms.objs[self.pos].is_gradable() }
        fn is_renderable(self) -> bool { self.bms.objs[self.pos].is_renderable() }
        fn is_object(self) -> bool { self.bms.objs[self.pos].is_object() }
        fn is_bgm(self) -> bool { self.bms.objs[self.pos].is_bgm() }
        fn is_setbga(self) -> bool { self.bms.objs[self.pos].is_setbga() }
        fn is_setbpm(self) -> bool { self.bms.objs[self.pos].is_setbpm() }
        fn is_stop(self) -> bool { self.bms.objs[self.pos].is_stop() }

        fn object_lane(self) -> Option<Lane> { self.bms.objs[self.pos].object_lane() }
        fn sounds(self) -> ~[SoundRef] { self.bms.objs[self.pos].sounds() }
        fn keydown_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].keydown_sound() }
        fn keyup_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].keyup_sound() }
        fn through_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].through_sound() }
        fn images(self) -> ~[ImageRef] { self.bms.objs[self.pos].images() }
        fn through_damage(self) -> Option<Damage> { self.bms.objs[self.pos].through_damage() }
    }

    impl Pointer {
        /// Returns the time of pointed object.
        pub fn time(&self) -> float { self.bms.objs[self.pos].time }

        /// Returns the associated game data of pointed object.
        pub fn data(&self) -> ObjData { self.bms.objs[self.pos].data }

        /// Seeks to the first object which time is past the limit, if any.
        pub fn seek_until(&mut self, limit: float) {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            while self.pos < nobjs {
                if bms.objs[self.pos].time >= limit { break; }
                self.pos += 1;
            }
        }

        /// Iterates over objects starting from the current object, until the first object which
        /// time is past the limit is reached.
        pub fn iter_until(&mut self, limit: float, f: &fn(&Obj) -> bool) -> bool {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            while self.pos < nobjs {
                let current = &bms.objs[self.pos];
                if current.time >= limit { return false; }
                if !f(current) { return false; }
                self.pos += 1;
            }
            true
        }

        /// Seeks to the object pointed by the other pointer.
        pub fn seek_to(&mut self, limit: Pointer) {
            assert!(has_same_bms(self, &limit));
            let bms = &*self.bms;
            assert!(limit.pos <= bms.objs.len());
            self.pos = limit.pos;
        }

        /// Iterates over objects starting from the current object, until the object pointed by
        /// the other pointer is reached.
        pub fn iter_to(&mut self, limit: Pointer, f: &fn(&Obj) -> bool) -> bool {
            assert!(has_same_bms(self, &limit));
            let bms = &*self.bms;
            assert!(limit.pos <= bms.objs.len());
            while self.pos < limit.pos {
                let current = &bms.objs[self.pos];
                if !f(current) { return false; }
                self.pos += 1;
            }
            true
        }

        /// Seeks to the end of objects.
        pub fn seek_to_end(&mut self) {
            let bms = &*self.bms;
            self.pos = bms.objs.len();
        }

        /// Iterates over objects starting from the current object.
        pub fn iter_to_end(&mut self, f: &fn(&Obj) -> bool) -> bool {
            let bms = &*self.bms;
            let nobjs = bms.objs.len();
            while self.pos < nobjs {
                let current = &bms.objs[self.pos];
                if !f(current) { return false; }
                self.pos += 1;
            }
            true
        }

        /// Finds the next object that satisfies given condition if any, without updating itself.
        pub fn find_next_of_type(&self, cond: &fn(&Obj) -> bool) -> Option<Pointer> {
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

        /// Finds the previous object that satisfies given condition if any, without updating
        /// itself.
        pub fn find_previous_of_type(&self, cond: &fn(&Obj) -> bool) -> Option<Pointer> {
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

        /// Finds the closest object from the virtual time `base` that satisfies given condition
        /// if any. `base` should lie between the pointed object and the previous object.
        /// The proximity is measured in terms of virtual time, which can differ from actual time.
        pub fn find_closest_of_type(&self, base: float,
                                    cond: &fn(&Obj) -> bool) -> Option<Pointer> {
            let previous = self.find_previous_of_type(|obj| cond(obj)); // XXX #7363
            let next = self.find_next_of_type(|obj| cond(obj)); // XXX #7363
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

    /// Returns a pointer pointing the first object in `bms`.
    fn Pointer(bms: @mut ~Bms) -> Pointer {
        Pointer { bms: bms, pos: 0 }
    }

    /// Returns a pointer pointing given object in `bms`.
    fn pointer_with_pos(bms: @mut ~Bms, pos: uint) -> Pointer {
        Pointer { bms: bms, pos: pos }
    }

    //----------------------------------------------------------------------------------------------
    // game play logics

    /// Grades. Angolmois performs the time-based grading as long as possible (it can go wrong when
    /// the object is near the discontinuity due to the current implementation strategy).
    #[deriving(Eq)]
    enum Grade {
        /**
         * Issued when the player did not input the object at all, the player was pressing the key
         * while a bomb passes through the corresponding lane, or failed to unpress the key within
         * the grading area for the end of LN. Resets the combo number, decreases the gauge
         * by severe amount (`MISS_DAMAGE` unless specified by the bomb) and displays the POOR BGA
         * for moments.
         *
         * Several games also use separate grading areas for empty lanes next to the object,
         * in order to avoid continuing the consecutive run ("combo") of acceptable grades by
         * just pressing every keys in the correct timing instead of pressing only lanes containing
         * objects. While this system is not a bad thing (and there are several BMS implementations
         * that use it), it is tricky to implement for the all situations. Angolmois currently
         * does not use this system due to the complexity.
         */
        MISS = 0,
        /// Issued when the player inputed the object and the normalized time difference (that is,
        /// the time difference multiplied by `Player::gradefactor`) between the input point and
        /// the object is between `GOOD_CUTOFF` and `BAD_CUTOFF` milliseconds. Resets the combo
        /// number, decreases the gauge by moderate amount (`BAD_DAMAGE`) and displays the POOR BGA
        /// for moments.
        BAD  = 1,
        /// Issued when the player inputed the object and the normalized time difference is between
        /// `GREAT_CUTOFF` and `GOOD_CUTOFF` milliseconds. Both the combo number and gauge is
        /// left unchanged.
        GOOD = 2,
        /// Issued when the player inputed the object and the normalized time difference is between
        /// `COOL_CUTOFF` and `GREAT_CUTOFF` milliseconds. The combo number is increased by one and
        /// the gauge is replenished by small amount.
        GREAT = 3,
        /// Issued when the player inputed the object and the normalized time difference is less
        /// than `COOL_CUTOFF` milliseconds. The combo number is increased by one and the gauge is
        /// replenished by large amount.
        COOL = 4,
    }

    /// Required time difference in milliseconds to get at least COOL grade.
    static COOL_CUTOFF: float = 14.4;
    /// Required time difference in milliseconds to get at least GREAT grade.
    static GREAT_CUTOFF: float = 48.0;
    /// Required time difference in milliseconds to get at least GOOD grade.
    static GOOD_CUTOFF: float = 84.0;
    /// Required time difference in milliseconds to get at least BAD grade.
    static BAD_CUTOFF: float = 144.0;

    /// The number of available grades.
    static NGRADES: uint = 5;

    /// The maximum (internal) value for the gauge.
    static MAXGAUGE: int = 512;
    /// A base score per exact input. Actual score can increase by the combo (up to 2x) or decrease
    /// by the larger time difference.
    static SCOREPERNOTE: float = 300.0;

    /// A damage due to the MISS grading. Only applied when the grading is not due to the bomb.
    static MISS_DAMAGE: Damage = GaugeDamage(0.059);
    /// A damage due to the BAD grading.
    static BAD_DAMAGE: Damage = GaugeDamage(0.030);

    /// Game play states independent to the display.
    pub struct Player {
        /// The game play options.
        opts: ~Options,
        /// The current BMS data.
        //
        // Rust: this should have been just `~Bms`, and `Pointer` should have received a lifetime
        //       parameter (for `&'self Bms` things). in reality, though, a lifetime parameter made
        //       borrowck much stricter and I ended up with wrapping `bms` to a mutable managed box.
        bms: @mut ~Bms,
        /// The derived BMS information.
        infos: ~BmsInfo,
        /// The length of BMS file in seconds as calculated by `bms_duration`. (C: `duration`)
        duration: float,
        /// The key specification.
        keyspec: ~KeySpec,
        /// The input mapping.
        keymap: ~KeyMap,

        /// Set to true if the corresponding object in `bms.objs` had graded and should not be
        /// graded twice. Its length equals to that of `bms.objs`. (C: `nograding` field in
        /// `struct obj`)
        nograding: ~[bool],
        /// Sound resources. (C: `res` field in `sndres`)
        sndres: ~[SoundResource],
        /// A sound chunk used for beeps. It always plays on the channel #0. (C: `beep`)
        beep: ~Chunk,
        /// Last channels in which the corresponding sound in `sndres` was played.
        /// (C: `lastch` field in `sndres`)
        sndlastch: ~[Option<uint>],
        /// Indices to last sounds which the channel has played. For every `x`, if `sndlastch[x] ==
        /// Some(y)` then `sndlastchmap[y] == Some(x)` and vice versa. (C: `sndlastchmap`)
        lastchsnd: ~[Option<uint>],
        /// Currently active BGA layers. (C: `bga`)
        bga: BGAState,

        /// The chart expansion rate, or "play speed". One measure has the length of 400 pixels
        /// times the play speed, so higher play speed means that objects will fall much more
        /// quickly (hence the name). (C: `playspeed`)
        playspeed: float,
        /// The play speed targeted for speed change if any. It is also the value displayed while
        /// the play speed is changing. (C: `targetspeed`)
        targetspeed: Option<float>,
        /// The current BPM. Can be negative, in that case the chart will scroll backwards.
        /// (C: `bpm`)
        bpm: BPM,
        /// The timestamp at the last tick. It is a return value from `sdl::get_ticks` and measured
        /// in milliseconds. (C: `now`)
        now: uint,
        /// The timestamp at the first tick. (C: `origintime`)
        origintime: uint,
        /**
         * The timestamp at the last discontinuity that breaks a linear relationship between
         * the virtual time and actual time. (C: `starttime`) Currently the following are
         * considered a discontinuity:
         *
         * - `origintime`
         * - A change in BPM
         * - A change in scaling factor of measure
         * - A scroll stopper (in this case, `stoptime` is first updated and `starttime` is updated
         *   at the end of stop)
         */
        starttime: uint,
        /// The timestamp at the end of ongoing scroll stopper, if any. (C: `stoptime`)
        stoptime: Option<uint>,
        /// The virtual time at the last discontinuity. (C: `startoffset`)
        startoffset: float,
        /// The current scaling factor of measure. (C: `startshorten`)
        startshorten: float,

        /// The virtual time at the bottom of the visible chart. (C: `bottom`)
        bottom: float,
        /// The virtual time at the grading line. Currently same as `bottom`. (C: `line`)
        line: float,
        /// The virtual time at the top of the visible chart. (C: `top`)
        top: float,
        /// A pointer to the first `Obj` after `bottom`. (C: `pfront`)
        pfront: Pointer,
        /// A pointer to the first `Obj` after `line`. (C: `pcur`)
        pcur: Pointer,
        /// A pointer to the first `Obj` that haven't escaped the grading area. It is possible that
        /// this `Obj` haven't reached the grading area either. (C: `pcheck`)
        pcheck: Pointer,
        /// Pointers to `Obj`s for the start of LN which grading is in progress. (C: `pthru`)
        //
        // Rust: this is intended to be `[Option<Pointer>, ..NLANES]` but a fixed-size vector cannot
        //       be cloned.
        pthru: ~[Option<Pointer>],

        /// The scale factor for grading area. The factor less than 1 causes the grading area
        /// shrink. (C: `gradefactor`)
        gradefactor: float,
        /// (C: `grademode` and `gradetime`)
        lastgrade: Option<(Grade,uint)>,
        /// The numbers of each grades. (C: `scocnt`)
        gradecounts: [uint, ..NGRADES],
        /// The last combo number, i.e. the number of objects graded at least GREAT. GOOD doesn't
        /// cause the combo number reset; BAD and MISS do. (C: `scombo`)
        lastcombo: uint,
        /// The best combo number so far. If the player manages to get no BADs and MISSes, then
        /// the combo number should end up with the number of note and LN objects
        /// (`BMSInfo::nnotes`). (C: `smaxcombo`)
        bestcombo: uint,
        /// The current score. (C: `score`)
        score: uint,
        /// The current health gauge. Should be no larger than `MAXGAUGE`. This can go negative
        /// (not displayed directly), which will require players much more efforts to survive.
        /// (C: `gauge`)
        gauge: int,
        /// The health gauge required to survive at the end of the song. Note that the gaugex
        /// less than this value (or even zero) doesn't cause the instant game over;
        /// only `InstantDeath` value from `Damage` does. (C: `survival`)
        survival: int,

        /// The number of keyboard or joystick keys, mapped to each lane and and currently pressed.
        /// (C: `keypressed[0]`)
        keymultiplicity: [uint, ..NLANES],
        /// The state of joystick axes. (C: `keypressed[1]`)
        joystate: [InputState, ..NLANES],
    }

    /// A list of play speed marks. `SpeedUpInput` and `SpeedDownInput` changes the play speed to
    /// the next/previous nearest mark. (C: `speeds`)
    static SPEED_MARKS: &'static [float] = &[0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0,
        3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 8.0, 10.0, 15.0, 25.0, 40.0, 60.0, 99.0];

    /// Finds the next nearest play speed mark if any.
    fn next_speed_mark(current: float) -> Option<float> {
        let mut prev = None;
        for &speed in SPEED_MARKS.iter() {
            if speed < current - 0.001 {
                prev = Some(speed);
            } else {
                return prev;
            }
        }
        None
    }

    /// Finds the previous nearest play speed mark if any.
    fn previous_speed_mark(current: float) -> Option<float> {
        let mut next = None;
        for &speed in SPEED_MARKS.rev_iter() {
            if speed > current + 0.001 {
                next = Some(speed);
            } else {
                return next;
            }
        }
        None
    }

    /// Creates a beep sound played on the play speed change. (C: `create_beep`)
    fn create_beep() -> ~Chunk {
        let samples = vec::from_fn::<i32>(12000, // approx. 0.14 seconds
            // sawtooth wave at 3150 Hz, quadratic decay after 0.02 seconds.
            |i| { let i = i as i32; (i%28-14) * cmp::min(2000, (12000-i)*(12000-i)/50000) });
        Chunk::new(unsafe { ::std::cast::transmute(samples) }, 128)
    }

    /// Creates a new player object. The player object owns other related structures, including
    /// the options, BMS file, key specification, input mapping and sound resources.
    pub fn Player(opts: ~Options, bms: ~Bms, infos: ~BmsInfo, duration: float, keyspec: ~KeySpec,
                  keymap: ~KeyMap, sndres: ~[SoundResource]) -> Player {
        let now = get_ticks();
        let initplayspeed = opts.playspeed;
        let originoffset = infos.originoffset;
        let startshorten = bms.shorten(originoffset as int);
        let gradefactor = 1.5 - cmp::min(bms.rank, 5) as float * 0.25;
        let initialgauge = MAXGAUGE * 500 / 1000;
        let survival = MAXGAUGE * 293 / 1000;
        let initbpm = bms.initbpm;
        let nobjs = bms.objs.len();
        let nsounds = sndres.len();

        let bms = @mut bms;
        let initptr = Pointer(bms);
        let mut player = Player {
            opts: opts, bms: bms, infos: infos, duration: duration,
            keyspec: keyspec, keymap: keymap,

            nograding: vec::from_elem(nobjs, false), sndres: sndres, beep: create_beep(),
            sndlastch: vec::from_elem(nsounds, None), lastchsnd: ~[], bga: initial_bga_state(),

            playspeed: initplayspeed, targetspeed: None, bpm: initbpm, now: now, origintime: now,
            starttime: now, stoptime: None, startoffset: originoffset, startshorten: startshorten,

            bottom: originoffset, line: originoffset, top: originoffset,
            pfront: initptr, pcur: initptr, pcheck: initptr, pthru: ~[None, ..NLANES],

            gradefactor: gradefactor, lastgrade: None, gradecounts: [0, ..NGRADES],
            lastcombo: 0, bestcombo: 0, score: 0, gauge: initialgauge, survival: survival,

            keymultiplicity: [0, ..NLANES], joystate: [Neutral, ..NLANES],
        };

        player.allocate_more_channels(64);
        reserve_channels(1); // so that the beep won't be affected
        player
    }

    impl Player {
        /// Returns true if the specified lane is being pressed, either by keyboard, joystick
        /// buttons or axes.
        pub fn key_pressed(&self, lane: Lane) -> bool {
            self.keymultiplicity[*lane] > 0 || self.joystate[*lane] != Neutral
        }

        /// Returns the play speed displayed. Can differ from the actual play speed
        /// (`self.playspeed`) when the play speed is changing.
        pub fn nominal_playspeed(&self) -> float {
            self.targetspeed.unwrap_or(self.playspeed)
        }

        /// Updates the score and associated statistics according to grading. `scoredelta` is
        /// an weight normalized to [0,1] that is calculated from the distance between the object
        /// and the input time, and `damage` is an optionally associated `Damage` value for bombs.
        /// May return true when `Damage` resulted in the instant death. (C: `update_grade`)
        pub fn update_grade(&mut self, grade: Grade, scoredelta: float,
                            damage: Option<Damage>) -> bool {
            self.gradecounts[grade as uint] += 1;
            self.lastgrade = Some((grade, self.now));
            self.score += (scoredelta * SCOREPERNOTE *
                           (1.0 + (self.lastcombo as float) /
                                  (self.infos.nnotes as float))) as uint;

            match grade {
                MISS | BAD => { self.lastcombo = 0; }
                GOOD => {}
                GREAT | COOL => {
                    // at most 5/512(1%) recover when the combo is topped
                    let weight = if grade == GREAT {2} else {3};
                    let cmbbonus = cmp::min(self.lastcombo as int, 100) / 50;
                    self.lastcombo += 1;
                    self.gauge = cmp::min(self.gauge + weight + cmbbonus, MAXGAUGE);
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

        /// Same as `update_grade`, but the grade is calculated from the normalized difference
        /// between the object and input time in milliseconds. The normalized distance equals to
        /// the actual time difference when `gradefactor` is 1.0. (C: `update_grade(grade,
        /// scoredelta, 0)` where `grade` and `scoredelta` are pre-calculated from `dist`)
        pub fn update_grade_from_distance(&mut self, dist: float) {
            let dist = num::abs(dist);
            let (grade, damage) = if      dist <  COOL_CUTOFF {(COOL,None)}
                                  else if dist < GREAT_CUTOFF {(GREAT,None)}
                                  else if dist <  GOOD_CUTOFF {(GOOD,None)}
                                  else if dist <   BAD_CUTOFF {(BAD,Some(BAD_DAMAGE))}
                                  else                        {(MISS,Some(MISS_DAMAGE))};
            let scoredelta = cmp::max(1.0 - dist / BAD_CUTOFF, 0.0);
            let keepgoing = self.update_grade(grade, scoredelta, damage);
            assert!(keepgoing);
        }

        /// Same as `update_grade`, but with the predetermined damage value. Always results in MISS
        /// grade. May return true when the damage resulted in the instant death.
        /// (C: `update_grade(0, 0, damage)`)
        pub fn update_grade_from_damage(&mut self, damage: Damage) -> bool {
            self.update_grade(MISS, 0.0, Some(damage))
        }

        /// Same as `update_grade`, but always results in MISS grade with the standard damage value.
        /// (C: `update_grade(0, 0, 0)`)
        pub fn update_grade_to_miss(&mut self) {
            let keepgoing = self.update_grade(MISS, 0.0, Some(MISS_DAMAGE));
            assert!(keepgoing);
        }

        /// Allocate more SDL_mixer channels without stopping already playing channels.
        /// (C: `allocate_more_channels`)
        pub fn allocate_more_channels(&mut self, howmany: uint) {
            let howmany = howmany as ::std::libc::c_int;
            let nchannels = allocate_channels(-1 as ::std::libc::c_int);
            let nchannels = allocate_channels(nchannels + howmany) as uint;
            if self.lastchsnd.len() < nchannels {
                self.lastchsnd.grow(nchannels, &None);
            }
        }

        /// Plays a given sound referenced by `sref`. `bgm` indicates that the sound is a BGM and
        /// should be played with the lower volume and should in the different channel group from
        /// key sounds. (C: `play_sound`)
        pub fn play_sound(&mut self, sref: SoundRef, bgm: bool) {
            let sref = **sref as uint;
            let chunk = match self.sndres[sref].chunk() {
                Some(chunk) => chunk,
                None => { return; }
            };
            let lastch = self.sndlastch[sref].map(|&ch| ch as ::std::libc::c_int);

            // try to play on the last channel if it is not occupied by other sounds (in this case
            // the last channel info is removed)
            let mut ch;
            loop {
                ch = chunk.play(lastch, 0);
                if ch >= 0 { break; }
                self.allocate_more_channels(32);
            }

            let group = if bgm {1} else {0};
            set_channel_volume(Some(ch), if bgm {96} else {128});
            group_channel(Some(ch), Some(group));

            let ch = ch as uint;
            for &idx in self.lastchsnd[ch].iter() { self.sndlastch[idx] = None; }
            self.sndlastch[sref] = Some(ch);
            self.lastchsnd[ch] = Some(sref as uint);
        }

        /// Plays a given sound if `sref` is not zero. This reflects the fact that an alphanumeric
        /// key `00` is normally a placeholder.
        pub fn play_sound_if_nonzero(&mut self, sref: SoundRef, bgm: bool) {
            if **sref > 0 { self.play_sound(sref, bgm); }
        }

        /// Plays a beep. The beep is always played in the channel 0, which is excluded from
        /// the uniform key sound and BGM management. (C: `Mix_PlayChannel(0, beep, 0)`)
        pub fn play_beep(&self) {
            self.beep.play(Some(0), 0);
        }

        /// Updates the player state. (C: `play_process`)
        pub fn tick(&mut self) -> bool {
            let bms = &*self.bms;
            let mut pfront = self.pfront.clone();
            let mut pcur = self.pcur.clone();
            let mut pcheck = self.pcheck.clone();
            let mut pthru = self.pthru.clone();

            // smoothly change the play speed
            if self.targetspeed.is_some() {
                let target = self.targetspeed.unwrap();
                let delta = target - self.playspeed;
                if num::abs(delta) < 0.001 {
                    self.playspeed = target;
                    self.targetspeed = None;
                } else {
                    self.playspeed += delta * 0.1;
                }
            }

            // process the ongoing scroll stopper if any
            self.now = get_ticks();
            self.bottom = match self.stoptime {
                Some(t) => {
                    if self.now >= t {
                        self.starttime = t;
                        self.stoptime = None;
                    }
                    self.startoffset
                }
                None => {
                    let msecdiff = (self.now - self.starttime) as float;
                    let measurediff = self.bpm.msec_to_measure(msecdiff);
                    self.startoffset + measurediff / self.startshorten
                }
            };

            // Breaks a continuity at given virtual time.
            let break_continuity = |at: float| {
                assert!(at >= self.startoffset);
                self.starttime += (self.bpm.measure_to_msec(at - self.startoffset) *
                                   self.startshorten) as uint;
                self.startoffset = at;
            };

            // process the measure scale factor change
            let bottommeasure = self.bottom.floor();
            let curshorten = bms.shorten(bottommeasure as int);
            if bottommeasure >= -1.0 && self.startshorten != curshorten {
                break_continuity(bottommeasure);
                self.startshorten = curshorten;
            }

            //self.line = bms.adjust_object_time(self.bottom, 0.03 / self.playspeed);
            self.line = self.bottom;
            self.top = bms.adjust_object_time(self.bottom, 1.25 / self.playspeed);
            let lineshorten = bms.shorten(self.line.floor() as int);

            // apply object-like effects while advancing to new `pcur`
            pfront.seek_until(self.bottom);
            let mut prevpcur = pointer_with_pos(self.bms, pcur.pos);
            do pcur.iter_until(self.line) |&obj| {
                match obj.data {
                    BGM(sref) => {
                        self.play_sound_if_nonzero(sref, true);
                    }
                    SetBGA(layer, iref) => {
                        self.bga[layer as uint] = iref;
                    }
                    SetBPM(newbpm) => {
                        break_continuity(obj.time);
                        self.bpm = newbpm;
                    }
                    Stop(duration) => {
                        let msecs = duration.to_msec(self.bpm);
                        let newstoptime = Some(msecs as uint + self.now);
                        self.stoptime = self.stoptime.merge(newstoptime, cmp::max);
                        self.startoffset = obj.time;
                    }
                    Visible(_,sref) | LNStart(_,sref) => {
                        if self.opts.is_autoplay() {
                            for &sref in sref.iter() {
                                self.play_sound_if_nonzero(sref, false);
                            }
                            self.update_grade_from_distance(0.0);
                        }
                    }
                    _ => {}
                }
                true
            };

            // grade objects that have escaped the grading area
            if !self.opts.is_autoplay() {
                do pcheck.iter_to(pcur) |&obj| {
                    let dist = self.bpm.measure_to_msec(self.line - obj.time) *
                               bms.shorten(obj.measure()) * self.gradefactor;
                    if dist < BAD_CUTOFF {
                        false
                    } else {
                        if !self.nograding[pcheck.pos] {
                            let lane = obj.object_lane(); // XXX #3511
                            for &Lane(lane) in lane.iter() {
                                let missable =
                                    match obj.data {
                                        Visible(*) | LNStart(*) => true,
                                        LNDone(*) => pthru[lane].is_some(),
                                        _ => false,
                                    };
                                if missable {
                                    self.update_grade_to_miss();
                                    pthru[lane] = None;
                                }
                            }
                        }
                        true
                    }
                };
            }

            // process inputs
            loop {
                // map to the virtual input. results in `vkey` (virtual key), `state` (input state)
                // and `continuous` (true if the input is not discrete and `Negative` input state
                // matters).
                let (key, state) = match poll_event() {
                    NoEvent => break,
                    QuitEvent | KeyEvent(EscapeKey,_,_,_) => { return false; }
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
                    JoyAxisEvent(_which,axis,_delta) =>
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

                // Returns true if the given lane is previously pressed and now unpressed.
                // When the virtual input is mapped to multiple actual inputs it can update
                // the internal state but still return false.
                let is_unpressed = |lane: Lane, continuous: bool, state: InputState| {
                    if state == Neutral || (continuous && self.joystate[*lane] != state) {
                        if continuous {
                            self.joystate[*lane] = state; true
                        } else {
                            if self.keymultiplicity[*lane] > 0 {
                                self.keymultiplicity[*lane] -= 1;
                            }
                            (self.keymultiplicity[*lane] == 0)
                        }
                    } else {
                        false
                    }
                };

                // Returns true if the given lane is previously unpressed and now pressed.
                // When the virtual input is mapped to multiple actual inputs it can update
                // the internal state but still return false.
                let is_pressed = |lane: Lane, continuous: bool, state: InputState| {
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
                    // if LN grading is in progress and it is not within the threshold then
                    // MISS grade is issued
                    for &thru in pthru[*lane].iter() {
                        let nextlndone = do thru.find_next_of_type |&obj| {
                            obj.object_lane() == Some(lane) &&
                            obj.is_lndone()
                        };
                        for &p in nextlndone.iter() {
                            let delta = self.bpm.measure_to_msec(p.time() - self.line) *
                                        lineshorten * self.gradefactor;
                            if num::abs(delta) < BAD_CUTOFF {
                                self.nograding[p.pos] = true;
                            } else {
                                self.update_grade_to_miss();
                            }
                        }
                    }
                    pthru[*lane] = None;
                };

                let process_press = |lane: Lane| {
                    // plays the closest key sound
                    let soundable =
                        do pcur.find_closest_of_type(self.line) |&obj| {
                            obj.object_lane() == Some(lane) && obj.is_soundable()
                        };
                    for &p in soundable.iter() {
                        let sounds = p.sounds(); // XXX #3511
                        for &sref in sounds.iter() {
                            self.play_sound(sref, false);
                        }
                    }

                    // tries to grade the closest gradable object in
                    // the grading area
                    let gradable =
                        do pcur.find_closest_of_type(self.line) |&obj| {
                            obj.object_lane() == Some(lane) && obj.is_gradable()
                        };
                    for &p in gradable.iter() {
                        if p.pos >= pcheck.pos && !self.nograding[p.pos] && !p.is_lndone() {
                            let dist = self.bpm.measure_to_msec(p.time() - self.line) *
                                       lineshorten * self.gradefactor;
                            if num::abs(dist) < BAD_CUTOFF {
                                if p.is_lnstart() {
                                    pthru[*lane] = Some(pointer_with_pos(self.bms, p.pos));
                                }
                                self.nograding[p.pos] = true;
                                self.update_grade_from_distance(dist);
                            }
                        }
                    }
                    true
                };

                match (vkey, state) {
                    (SpeedDownInput, Positive) | (SpeedDownInput, Negative) => {
                        let current = self.targetspeed.unwrap_or(self.playspeed);
                        let newspeed = next_speed_mark(current); // XXX #3511
                        for &newspeed in newspeed.iter() {
                            self.targetspeed = Some(newspeed);
                            self.play_beep();
                        }
                    }
                    (SpeedUpInput, Positive) | (SpeedUpInput, Negative) => {
                        let current = self.targetspeed.unwrap_or(self.playspeed);
                        let newspeed = previous_speed_mark(current); // XXX #3511
                        for &newspeed in newspeed.iter() {
                            self.targetspeed = Some(newspeed);
                            self.play_beep();
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

            // process bombs
            if !self.opts.is_autoplay() {
                // TODO make `Pointer::iter_*` a proper iterator?
                let nodeath = do prevpcur.iter_to(pcur) |&obj| {
                    match obj.data {
                        Bomb(lane,sref,damage) if self.key_pressed(lane) => {
                            // ongoing long note is not graded twice
                            pthru[*lane] = None;
                            for &sref in sref.iter() {
                                self.play_sound(sref, false);
                            }
                            if !self.update_grade_from_damage(damage) {
                                // instant death
                                pcur.seek_to_end();
                                false
                            } else {
                                true
                            }
                        },
                        _ => true
                    }
                };
                if !nodeath { return false; }
            }

            self.pfront = pfront;
            self.pcur = pcur;
            self.pcheck = pcheck;
            self.pthru = pthru;

            // determines if we should keep playing
            if self.bottom > (bms.nmeasures + 1) as float {
                if self.opts.is_autoplay() {
                    num_playing(None) != num_playing(Some(0))
                } else {
                    newest_in_group(Some(1)).is_some()
                }
            } else if self.bottom < self.infos.originoffset {
                false // special casing the negative BPM
            } else {
                true
            }
        }
    }

    /// Display interface.
    pub trait Display {
        /// Renders the current information from `player` to the screen or console. Called after
        /// each call to `Player::tick`.
        fn render(&mut self, player: &Player);
        /// Shows the game play result from `player` to the screen or console. Called only once.
        fn show_result(&self, player: &Player);
    }

    //----------------------------------------------------------------------------------------------
    // graphic display

    /// An appearance for each lane. (C: `struct tkeykind` and `tkeyleft`)
    struct LaneStyle {
        /// The left position of the lane in the final screen. (C: `tkeyleft`)
        left: uint,
        /// The left position of the lane in the object sprite. (C: `spriteleft` field)
        spriteleft: uint,
        /// The left position of the lane in the bomb sprite. (C: `spritebombleft` field)
        spritebombleft: uint,
        /// The width of lane. (C: `width` field)
        width: uint,
        /// The base color of object. The actual `Gradient` for drawing is derived from this color.
        /// (C: `basecolor` field)
        basecolor: Color
    }

    impl LaneStyle {
        /// Constructs a new `LaneStyle` object from given key kind and the left (`Left(pos)`) or
        /// right (`Right(pos)`) position. (C: `tkeykinds`)
        pub fn from_kind(kind: KeyKind, pos: Either<uint,uint>) -> LaneStyle {
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
            LaneStyle { left: left, spriteleft: spriteleft, spritebombleft: spritebombleft,
                        width: width, basecolor: color }
        }

        /// Renders required object and bomb images to the sprite.
        pub fn render_to_sprite(&self, sprite: &Surface) {
            let left = self.spriteleft;
            let noteleft = self.spriteleft + SCREENW;
            let bombleft = self.spritebombleft + SCREENW;
            assert!(sprite.get_width() as uint >= cmp::max(noteleft, bombleft) + self.width);

            // render a background sprite (0 at top, <1 at bottom)
            let backcolor = Gradient { zero: RGB(0,0,0), one: self.basecolor };
            for i in range(140, SCREENH - 80) {
                sprite.fill_area((left, i), (self.width, 1), backcolor.blend(i as int - 140, 1000));
            }

            // render note and bomb sprites (1/2 at middle, 1 at border)
            let denom = self.width as int;
            let notecolor = Gradient { zero: RGB(0xff,0xff,0xff), one: self.basecolor };
            let bombcolor = Gradient { zero: RGB(0,0,0),          one: RGB(0xc0,0,0) };
            for i in range(0, self.width / 2) {
                let num = (self.width - i) as int;
                sprite.fill_area((noteleft+i, 0), (self.width-i*2, SCREENH),
                                 notecolor.blend(num, denom));
                sprite.fill_area((bombleft+i, 0), (self.width-i*2, SCREENH),
                                 bombcolor.blend(num, denom));
            }
        }

        /// Renders the lane background to the screen from the sprite.
        pub fn render_back(&self, screen: &Surface, sprite: &Surface, pressed: bool) {
            screen.fill_area((self.left, 30), (self.width, SCREENH-110), RGB(0,0,0));
            if pressed {
                screen.blit_area(sprite, (self.spriteleft, 140), (self.left, 140),
                                 (self.width, SCREENH-220));
            }
        }

        /// Renders an object to the screen from the sprite.
        pub fn render_note(&self, screen: &Surface, sprite: &Surface, top: uint, bottom: uint) {
            screen.blit_area(sprite, (self.spriteleft + SCREENW, 0),
                             (self.left, top), (self.width, bottom - top));
        }

        /// Renders a bomb object to the screen from the sprite.
        pub fn render_bomb(&self, screen: &Surface, sprite: &Surface, top: uint, bottom: uint) {
            screen.blit_area(sprite, (self.spritebombleft + SCREENW, 0),
                             (self.left, top), (self.width, bottom - top));
        }
    }

    /// Builds a list of `LaneStyle`s from the key specification.
    fn build_lane_styles(keyspec: &KeySpec) ->
                                    Result<(uint, Option<uint>, ~[(Lane,LaneStyle)]), ~str> {
        let mut leftmost = 0;
        let mut rightmost = SCREENW;
        let mut styles = ~[];
        for &lane in keyspec.left_lanes().iter() {
            let kind = keyspec.kinds[*lane];
            assert!(kind.is_some());
            let kind = kind.unwrap();
            let style = LaneStyle::from_kind(kind, Left(leftmost));
            styles.push((lane, style));
            leftmost += style.width + 1;
            if leftmost > SCREENW - 20 {
                return Err(~"The screen can't hold that many lanes");
            }
        }
        for &lane in keyspec.right_lanes().iter() {
            let kind = keyspec.kinds[*lane];
            assert!(kind.is_some());
            let kind = kind.unwrap();
            let style = LaneStyle::from_kind(kind, Right(rightmost));
            styles.push((lane, style));
            if rightmost < leftmost + 40 {
                return Err(~"The screen can't hold that many lanes");
            }
            rightmost -= style.width + 1;
        }
        let mut rightmost = if rightmost == SCREENW {None} else {Some(rightmost)};

        // move lanes to the center if there are too small number of lanes
        let cutoff = 165;
        if leftmost < cutoff {
            for i in range(0, keyspec.split) {
                let (lane, style) = styles[i];
                let mut style = style;
                style.left += (cutoff - leftmost) / 2;
                styles[i] = (lane, style);
            }
            leftmost = cutoff;
        }
        if rightmost.map_default(false, |&x| x > SCREENW - cutoff) {
            for i in range(keyspec.split, styles.len()) {
                let (lane, style) = styles[i];
                let mut style = style;
                style.left -= (rightmost.unwrap() - (SCREENW - cutoff)) / 2;
                styles[i] = (lane, style);
            }
            rightmost = Some(SCREENW - cutoff);
        }

        Ok((leftmost, rightmost, styles))
    }

    /// Creates a sprite. (C: sprite construction portion of `play_prepare`)
    fn create_sprite(opts: &Options, leftmost: uint, rightmost: Option<uint>,
                     styles: &[(Lane,LaneStyle)]) -> ~Surface {
        let sprite = new_surface(SCREENW + 400, SCREENH);
        let black = RGB(0,0,0);
        let gray = RGB(0x40,0x40,0x40); // gray used for separators

        // render notes and lane backgrounds
        for &(_lane,style) in styles.iter() {
            style.render_to_sprite(sprite);
        }

        // render panels
        do sprite.with_pixels |pixels| {
            let topgrad = Gradient { zero: RGB(0x60,0x60,0x60), one: RGB(0xc0,0xc0,0xc0) };
            let botgrad = Gradient { zero: RGB(0x40,0x40,0x40), one: RGB(0xc0,0xc0,0xc0) };
            for j in range(-244, 556) {
                for i in range(-10, 20) {
                    let c = (i*2+j*3+750) % 2000;
                    pixels.put_pixel((j+244) as uint, (i+10) as uint,
                                     topgrad.blend(850 - num::abs(c-1000), 700));
                }
                for i in range(-20, 60) {
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
        do sprite.with_pixels |pixels| {
            for i in range(0, 20u) {
                // Rust: this cannot be `uint` since `-1u` underflows!
                for j in iter::range_step(20, 0, -1) {
                    let j = j as uint;
                    if i*i + j*j <= 400 { break; } // circled border
                    pixels.put_pixel(leftmost + j, 10 + i, black);
                    pixels.put_pixel(leftmost + j, (SCREENH-61) - i, black);
                    for &right in rightmost.iter() {
                        pixels.put_pixel((right-j) - 1, 10 + i, black);
                        pixels.put_pixel((right-j) - 1, (SCREENH-61) - i, black);
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

    /// Full-featured graphic display. Used for the normal game play and automatic play mode.
    pub struct GraphicDisplay {
        /// Sprite surface generated by `create_sprite`. (C: `sprite`)
        sprite: ~Surface,
        /// Display screen. (C: `screen`)
        screen: ~Surface,
        /// Bitmap font.
        font: ~Font,
        /// Image resources. (C: `imgres`)
        imgres: ~[ImageResource],

        /// The leftmost X coordinate of the area next to the lanes, that is, the total width of
        /// left-hand-side lanes. (C: `tpanel1`)
        leftmost: uint,
        /// The rightmost X coordinate of the area next to the lanes, that is, the screen width
        /// minus the total width of right-hand-side lanes if any. `None` indicates the absence of
        /// right-hand-side lanes. (C: `tpanel2`)
        rightmost: Option<uint>,
        /// The order and appearance of lanes. (C: `tkey` and `tkeyleft`)
        lanestyles: ~[(Lane,LaneStyle)],
        /// The left coordinate of the BGA. (C: `tbgax`)
        bgax: uint,
        /// The top coordinate of the BGA. (C: `tbgay`)
        bgay: uint,

        /// If not `None`, indicates that the POOR BGA should be displayed until this timestamp.
        /// (C: `poorlimit`)
        poorlimit: Option<uint>,
        /// If not `None`, indicates that the grading information should be displayed until
        /// this timestamp. (C: `gradetime`)
        gradelimit: Option<uint>,
        /// Currently known state of BGAs.
        lastbga: BGAState,
    }

    /// Creates a new graphic display from the options, key specification, pre-allocated (usually
    /// by `init_video`) screen, pre-created bitmap fonts and pre-loaded image resources. The last
    /// three are owned by the display, others are not (in fact, should be owned by `Player`).
    pub fn GraphicDisplay(opts: &Options, keyspec: &KeySpec, screen: ~Surface, font: ~Font,
                          imgres: ~[ImageResource]) -> Result<GraphicDisplay,~str> {
        let (leftmost, rightmost, styles) = match build_lane_styles(keyspec) {
            Ok(styles) => styles,
            Err(err) => { return Err(err); }
        };
        let centerwidth = rightmost.unwrap_or(SCREENW) - leftmost;
        let bgax = leftmost + (centerwidth - BGAW) / 2;
        let bgay = (SCREENH - BGAH) / 2;
        let sprite = create_sprite(opts, leftmost, rightmost, styles);

        let display = GraphicDisplay {
            sprite: sprite, screen: screen, font: font, imgres: imgres,
            leftmost: leftmost, rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
            poorlimit: None, gradelimit: None, lastbga: initial_bga_state(),
        };

        display.screen.fill(RGB(0,0,0));
        display.restore_panel();
        display.screen.flip();

        Ok(display)
    }

    /// The list of grade names and corresponding color scheme. (C: `tgradestr` and `tgradecolor`)
    static GRADES: &'static [(&'static str,Gradient)] = &[
        // Rust: can we just use `Gradient()`???
        ("MISS",  Gradient { zero: RGB(0xff,0xc0,0xc0), one: RGB(0xff,0x40,0x40) }),
        ("BAD",   Gradient { zero: RGB(0xff,0xc0,0xff), one: RGB(0xff,0x40,0xff) }),
        ("GOOD",  Gradient { zero: RGB(0xff,0xff,0xc0), one: RGB(0xff,0xff,0x40) }),
        ("GREAT", Gradient { zero: RGB(0xc0,0xff,0xc0), one: RGB(0x40,0xff,0x40) }),
        ("COOL",  Gradient { zero: RGB(0xc0,0xc0,0xff), one: RGB(0x40,0x40,0xff) }),
    ];

    impl GraphicDisplay {
        /// Restores the panels by blitting upper and bottom panels to the screen.
        fn restore_panel(&self) {
            let screen: &Surface = self.screen;
            let sprite: &Surface = self.sprite;
            screen.blit_area(sprite, (0,0), (0,0), (SCREENW,30));
            screen.blit_area(sprite, (0,SCREENH-80), (0,SCREENH-80), (SCREENW,80));
        }
    }

    impl Display for GraphicDisplay {
        fn render(&mut self, player: &Player) {
            let screen = &*self.screen;
            let sprite = &*self.sprite;
            let font = &*self.font;
            let bms = &*player.bms;

            // update display states
            let mut poorlimit = self.poorlimit;
            let mut gradelimit = self.gradelimit;
            for &(grade,when) in player.lastgrade.iter() {
                if grade == MISS {
                    // switches to the normal BGA after 600ms
                    poorlimit = poorlimit.merge(Some(when + 600), cmp::max);
                }
                // grade disappears after 700ms
                gradelimit = gradelimit.merge(Some(when + 700), cmp::max);
            }
            if poorlimit < Some(player.now) { poorlimit = None; }
            if gradelimit < Some(player.now) { gradelimit = None; }
            self.lastbga.update(&player.bga, self.imgres);
            *&mut self.poorlimit = poorlimit;
            *&mut self.gradelimit = gradelimit;

            // render BGAs (should render before the lanes since lanes can overlap with BGAs)
            if player.opts.has_bga() {
                let layers = if poorlimit.is_some() {&[PoorBGA]} else {&[Layer1, Layer2, Layer3]};
                self.lastbga.render(self.screen, layers, self.imgres, self.bgax, self.bgay);
            }

            // fill the lanes to the border color
            screen.fill_area((0, 30), (self.leftmost, SCREENH-110), RGB(0x40,0x40,0x40));
            for &rightmost in self.rightmost.iter() {
                screen.fill_area((rightmost, 30), (SCREENH-rightmost, 490), RGB(0x40,0x40,0x40));
            }
            for &(lane,style) in self.lanestyles.iter() {
                style.render_back(screen, sprite, player.key_pressed(lane));
            }

            // set the clip area to avoid drawing on the panels
            screen.set_clip_area((0, 30), (SCREENW, SCREENH-110));

            // render objects
            let time_to_y = |time| {
                let adjusted = bms.adjust_object_position(player.bottom, time);
                (SCREENH-70) - (400.0 * player.playspeed * adjusted) as uint
            };
            for &(lane,style) in self.lanestyles.iter() {
                let front = do player.pfront.find_next_of_type |&obj| {
                    obj.object_lane() == Some(lane) && obj.is_renderable()
                };
                if front.is_none() { loop; }
                let front = front.unwrap();

                // LN starting before the bottom and ending after the top
                if front.time() > player.top && front.is_lndone() {
                    style.render_note(screen, sprite, 30, SCREENH - 80);
                } else {
                    let mut i = front.pos;
                    let mut nextbottom = None;
                    let nobjs = bms.objs.len();
                    let top = player.top;
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
                                                  nextbottom.unwrap_or(bottom));
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

                    for &y in nextbottom.iter() {
                        style.render_note(screen, sprite, 30, y);
                    }
                }
            }

            // render measure bars
            for i in range(player.bottom.floor() as int, player.top.floor() as int + 1) {
                let y = time_to_y(i as float);
                screen.fill_area((0, y), (self.leftmost, 1), RGB(0xc0,0xc0,0xc0));
                for &rightmost in self.rightmost.iter() {
                    screen.fill_area((rightmost, y), (800-rightmost, 1), RGB(0xc0,0xc0,0xc0));
                }
            }

            // render grading text
            if gradelimit.is_some() && player.lastgrade.is_some() {
                let gradelimit = gradelimit.unwrap();
                let (lastgrade,_) = player.lastgrade.unwrap();
                let (gradename,gradecolor) = GRADES[lastgrade as uint];
                let delta = (cmp::max(gradelimit - player.now, 400) - 400) / 15;
                do screen.with_pixels |pixels| {
                    font.print_string(pixels, self.leftmost/2, SCREENH/2 - 40 - delta, 2,
                                      Centered, gradename, gradecolor);
                    if player.lastcombo > 1 {
                        font.print_string(pixels, self.leftmost/2, SCREENH/2 - 12 - delta, 1,
                                          Centered, fmt!("%u COMBO", player.lastcombo),
                                          Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80)));
                    }
                    if player.opts.is_autoplay() {
                        font.print_string(pixels, self.leftmost/2, SCREENH/2 + 2 - delta, 1,
                                          Centered, "(AUTO)",
                                          Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x40,0x40,0x40)));
                    }
                }
            }

            screen.set_clip_rect(&screen.get_rect());

            self.restore_panel();

            // render panel
            let elapsed = (player.now - player.origintime) / 1000;
            let duration = player.duration as uint;
            let durationmsec = (player.duration * 1000.0) as uint;
            do screen.with_pixels |pixels| {
                let black = RGB(0,0,0);
                font.print_string(pixels, 10, 8, 1, LeftAligned,
                                  fmt!("SCORE %07u", player.score), black);
                let nominalplayspeed = player.nominal_playspeed();
                font.print_string(pixels, 5, SCREENH-78, 2, LeftAligned,
                                  fmt!("%4.1fx", nominalplayspeed), black);
                font.print_string(pixels, self.leftmost-94, SCREENH-35, 1, LeftAligned,
                                  fmt!("%02u:%02u / %02u:%02u", elapsed/60, elapsed%60,
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
                let cycle = (160.0 * player.startshorten * player.bottom).floor() % 40.0;
                let width = if player.gauge < 0 {0}
                            else {player.gauge * 400 / MAXGAUGE - (cycle as int)};
                let width = num::clamp(width, 5, 360);
                let color = if player.gauge >= player.survival {RGB(0xc0,0,0)}
                            else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
                screen.fill_area((4, SCREENH-12), (width, 8), color);
            }

            screen.flip();
        }

        fn show_result(&self, player: &Player) {
            if player.opts.is_autoplay() { return; }

            // check if the song reached the last gradable object (otherwise the game play was
            // terminated by the user)
            let nextgradable = player.pcur.find_next_of_type(|obj| obj.is_gradable());
            if nextgradable.is_some() { return; }

            if player.gauge >= player.survival {
                println(fmt!("*** CLEARED! ***\n\
                              COOL  %4u    GREAT %4u    GOOD  %4u\n\
                              BAD   %4u    MISS  %4u    MAX COMBO %u\n\
                              SCORE %07u (max %07d)",
                             player.gradecounts[4], player.gradecounts[3],
                             player.gradecounts[2], player.gradecounts[1],
                             player.gradecounts[0], player.bestcombo,
                             player.score, player.infos.maxscore));
            } else {
                println("YOU FAILED!");
            }
        }
    }

    //----------------------------------------------------------------------------------------------
    // text display

    /// Text-only display. Used for the exclusive mode with BGA disabled.
    pub struct TextDisplay {
        /// Ticker used for printing to the console.
        ticker: Ticker
    }

    /// Creates a new text-only display.
    pub fn TextDisplay() -> TextDisplay {
        TextDisplay { ticker: Ticker() }
    }

    impl Display for TextDisplay {
        fn render(&mut self, player: &Player) {
            if !player.opts.showinfo { return; }

            do self.ticker.on_tick(player.now) {
                let elapsed = (player.now - player.origintime) / 100;
                let duration = (player.duration * 10.0) as uint;
                update_line(fmt!("%02u:%02u.%u / %02u:%02u.%u (@%9.4f) | BPM %6.2f | %u / %d notes",
                                 elapsed/600, elapsed/10%60, elapsed%10,
                                 duration/600, duration/10%60, duration%10,
                                 player.bottom, *player.bpm,
                                 player.lastcombo, player.infos.nnotes));
            }
        }

        fn show_result(&self, _player: &Player) {
            update_line("");
        }
    }

    //----------------------------------------------------------------------------------------------
    // BGA-only display

    /// BGA-only display. Used for the exclusive mode with BGA enabled.
    pub struct BGAOnlyDisplay {
        /// The underlying text-only display (as the BGA-only display lacks the on-screen display).
        textdisplay: TextDisplay,
        /// Display screen. (C: `screen`)
        screen: ~Surface,
        /// Image resources. (C: `imgres`)
        imgres: ~[ImageResource],
        /// Currently known state of BGAs.
        lastbga: BGAState,
    }

    /// Creates a new BGA-only display from the pre-created screen (usually by `init_video`) and
    /// pre-loaded image resources.
    pub fn BGAOnlyDisplay(screen: ~Surface, imgres: ~[ImageResource]) -> BGAOnlyDisplay {
        BGAOnlyDisplay { textdisplay: TextDisplay(), screen: screen,
                         imgres: imgres, lastbga: initial_bga_state() }
    }

    impl Display for BGAOnlyDisplay {
        fn render(&mut self, player: &Player) {
            self.lastbga.update(&player.bga, self.imgres);

            let layers = &[Layer1, Layer2, Layer3];
            self.lastbga.render(self.screen, layers, self.imgres, 0, 0);
            self.screen.flip();

            self.textdisplay.render(player);
        }

        fn show_result(&self, player: &Player) {
            self.textdisplay.show_result(player);
        }
    }

    //----------------------------------------------------------------------------------------------

}

//==================================================================================================
// entry point

/// Parses the BMS file, initializes the display, shows the loading screen and runs the game play
/// loop. (C: `play`)
pub fn play(opts: ~player::Options) {
    // parses the file and sanitizes it
    let mut r = ::std::rand::rng();
    let mut bms = match parser::parse_bms(opts.bmspath, &mut r) {
        Ok(bms) => ~bms,
        Err(err) => die!("Couldn't load BMS file: %s", err)
    };
    parser::sanitize_bms(bms);

    // parses the key specification and further sanitizes `bms` with it
    let keyspec = match player::key_spec(bms, opts) {
        Ok(keyspec) => keyspec,
        Err(err) => die!("%s", err)
    };
    parser::compact_bms(bms, keyspec);
    let infos = ~parser::analyze_bms(bms);

    // applies the modifier if any
    for &modf in opts.modf.iter() {
        player::apply_modf(bms, modf, &mut r, keyspec, 0, keyspec.split);
        if keyspec.split < keyspec.order.len() {
            player::apply_modf(bms, modf, &mut r, keyspec, keyspec.split, keyspec.order.len());
        }
    }

    // initialize SDL
    player::init_sdl();
    for &joyidx in opts.joystick.iter() { player::init_joystick(joyidx); }

    // read the input mapping (dependent to the SDL initialization)
    let keymap = ~player::read_keymap(keyspec, std::os::getenv);

    // uncompress and populate the bitmap font.
    let mut font = ~gfx::Font();
    font.create_zoomed_font(1);
    font.create_zoomed_font(2);
    let font = font;

    // initialize the screen if required
    let mut screen = None;
    if opts.has_screen() {
        screen = Some(player::init_video(opts.is_exclusive(), opts.fullscreen));
    }

    let atexit = || { if opts.is_exclusive() { player::update_line(""); } };

    // render the loading screen
    let mut ticker = player::Ticker();
    let mut saved_screen = None; // XXX should be in a trait actually
    let _ = saved_screen; // Rust: avoids incorrect warning. (#3796)
    let update_status;
    if !opts.is_exclusive() {
        let screen_: &gfx::Surface = *screen.get_ref();
        player::show_stagefile_screen(bms, infos, keyspec, opts, screen_, font);
        if opts.showinfo {
            saved_screen = Some(player::save_screen_for_loading(screen_));
            update_status = |path| {
                let screen: &gfx::Surface = *screen.get_ref();
                let saved_screen: &gfx::Surface = *saved_screen.get_ref();
                player::graphic_update_status(path, screen, saved_screen,
                                              font, &mut ticker, || atexit()) // XXX #7363
            };
        } else {
            update_status = |_path| {};
        }
    } else if opts.showinfo {
        player::show_stagefile_noscreen(bms, infos, keyspec, opts);
        update_status = |path| {
            player::text_update_status(path, &mut ticker, || atexit()) // XXX #7363
        };
    } else {
        update_status = |_path| {};
    }

    // wait for resources
    let start = ::sdl::get_ticks() + 3000;
    let (sndres, imgres) =
        player::load_resource(bms, opts, |msg| update_status(msg)); // XXX #7363
    if opts.showinfo {
        ticker.reset(); // force update
        update_status(None);
    }
    while ::sdl::get_ticks() < start { player::check_exit(|| atexit()); } // XXX #7363

    // create the player and transfer ownership of other resources to it
    let duration = parser::bms_duration(bms, infos.originoffset,
                                        |sref| sndres[**sref].duration());
    let mut player = player::Player(opts, bms, infos, duration, keyspec, keymap, sndres);

    // create the display and runs the actual game play loop
    let display = match screen {
        Some(screen) => {
            if player.opts.is_exclusive() {
                @mut player::BGAOnlyDisplay(screen, imgres) as @mut player::Display
            } else {
                let display_ = player::GraphicDisplay(player.opts, player.keyspec,
                                                      screen, font, imgres);
                match display_ {
                    Ok(display) => @mut display as @mut player::Display,
                    Err(err) => die!("%s", err)
                }
            }
        },
        None => @mut player::TextDisplay() as @mut player::Display
    };
    while player.tick() {
        display.render(&player);
    }
    display.show_result(&player);

    // remove all channels before sound resources are deallocated.
    // halting alone is not sufficient due to rust-sdl's bug.
    ::sdl::mixer::allocate_channels(0);

    // it's done!
    atexit();
}

/// Prints the usage. (C: `usage`)
pub fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
    std::io::stderr().write_str(fmt!("\
%s -- the simple BMS player
http://mearie.org/projects/angolmois/
https://github.com/lifthrasiir/angolmois-rust/

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
    util::exit(1);
}

/// The entry point. Parses the command line options and delegates other things to `play`.
/// (C: `main`)
pub fn main() {
    use player::*;

    let longargs = (~[
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'),
        (~"--fullscreen", ' '), (~"--info", ' '), (~"--no-info", 'q'),
        (~"--mirror", 'm'), (~"--shuffle", 's'), (~"--shuffle-ex", 'S'),
        (~"--random", 'r'), (~"--random-ex", 'R'), (~"--preset", 'k'),
        (~"--key-spec", 'K'), (~"--bga", ' '), (~"--no-bga", 'B'),
        (~"--movie", ' '), (~"--no-movie", 'M'), (~"--joystick", 'j'),
    ]).move_iter().collect::<std::hashmap::HashMap<~str,char>>();

    let args = std::os::args();
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
            if bmspath.is_none() {
                bmspath = Some(args[i].clone());
            }
        } else if args[i] == ~"--" {
            i += 1;
            if bmspath.is_none() && i < nargs {
                bmspath = Some(args[i].clone());
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
                    args[i].slice_from(1).to_owned()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for (j, c) in shortargs.iter().enumerate() {
                // Reads the argument of the option. Option string should be consumed first.
                let fetch_arg = |opt| {
                    let off = if inside {j+1} else {j};
                    let nextarg =
                        if inside && off < nshortargs {
                            // remaining portion of `args[i]` is an argument
                            shortargs.slice_from(off)
                        } else {
                            // `args[i+1]` is an argument as a whole
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
                    'h' => { usage(); }
                    'V' => { println(version()); return; }
                    'v' => { mode = AutoPlayMode; }
                    'x' => { mode = ExclusiveMode; }
                    'X' => { mode = ExclusiveMode; bga = NoBga; }
                    'w' => { fullscreen = false; }
                    'q' => { showinfo = false; }
                    'm' => { modf = Some(MirrorModf); }
                    's' => { modf = Some(ShuffleModf); }
                    'S' => { modf = Some(ShuffleExModf); }
                    'r' => { modf = Some(RandomModf); }
                    'R' => { modf = Some(RandomExModf); }
                    'k' => { preset = Some(fetch_arg('k').to_owned()); }
                    'K' => { leftkeys = Some(fetch_arg('K').to_owned());
                             rightkeys = Some(fetch_arg('K').to_owned()); }
                    'a' => {
                        match from_str::<float>(fetch_arg('a')) {
                            Some(speed) if speed > 0.0 => {
                                playspeed = if speed < 0.1 {0.1}
                                            else if speed > 99.0 {99.0}
                                            else {speed};
                            }
                            _ => die!("Invalid argument to option -a")
                        }
                    }
                    'B' => { bga = NoBga; }
                    'M' => { bga = BgaButNoMovie; }
                    'j' => {
                        match from_str::<uint>(fetch_arg('j')) {
                            Some(n) => { joystick = Some(n); }
                            _ => die!("Invalid argument to option -j")
                        }
                    }
                    ' ' => {} // for ignored long options
                    '1'..'9' => { playspeed = char::to_digit(c, 10).unwrap() as float; }
                    _ => die!("Invalid option: -%c", c)
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    // shows a file dialog if the path to the BMS file is missing and the system supports it
    if bmspath.is_none() {
        bmspath = util::get_path_from_dialog();
    }

    match bmspath {
        None => { usage(); }
        Some(bmspath) => {
            let opts = ~Options { bmspath: bmspath, mode: mode, modf: modf, bga: bga,
                                  showinfo: showinfo, fullscreen: fullscreen, joystick: joystick,
                                  preset: preset, leftkeys: leftkeys, rightkeys: rightkeys,
                                  playspeed: playspeed };
            play(opts);
        }
    }
}
