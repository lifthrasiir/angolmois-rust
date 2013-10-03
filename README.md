# Angolmois Rust Edition

*2013-10-03*

This is a direct, one-to-one translation of Angolmois to [Rust](http://www.rust-lang.org/) programming language. [Angolmois](http://mearie.org/projects/angolmois/) is a [BM98](http://bm98.yaneu.com/bm98/)-like minimalistic music video game which supports the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.

Angolmois is a combination of string parsing, path manipulation, two-dimensional graphics and complex game play carefully packed into some thousand lines of code. This translation is intended to provide an example of translating a moderately-sized C code to Rust, and also to investigate additional library supports required for such moderately-sized programs. The resulting translation is certainly one of the largest applications written in Rust to date (most Rust projects are libraries for now), weighing more than 6,000 lines of Rust code. This is partly due to the fact that, unlike the original Angolmois code (which sacrifices most comments due to code size concerns), the Rust version has much more comments which can be beneficial for understanding Angolmois itself too.

I'm currently writing a detailed post on this translation; I hope it to be a starting point to the advanced programming in Rust. (Yes, I know it is being delayed a lot, I have lots of other businesses to run, so don't hold your breath please!) For the other things (especially the usage), please refer to the [README](https://github.com/lifthrasiir/angolmois/) file of the original Angolmois. If you want to learn more about [what the BMS format is](https://github.com/lifthrasiir/angolmois/blob/master/README.md#appendix-what-is-bms) and [how Angolmois is structured](https://github.com/lifthrasiir/angolmois/blob/master/INTERNALS.md), it turns out that I've already written a lot about them.

## Screenshots

![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois1.jpg?raw=true) 
![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois2.jpg?raw=true)  
*Game data credits: "sunken azure world", music by maki, courtesy of [Daida Three Brothers](http://daida.tv/). All rights reserved.*

## License

Angolmois is distributed under GNU GPL version 2+, so is this translation. The portions of it is intended to be sent as a patch to Rust, so those portions are licensed under Apache License 2.0 and MIT license. See the code for specifics.

## FAQ

### How to compile?

Assuming a proper Rust environment, get [rust-sdl](https://github.com/brson/rust-sdl) and run `make`, or the following command:

    rustc -O -L path/to/rust-sdl angolmois.rs -o angolmois

Since both rustc and rust-sdl are moving targets, you will probably need a specific version of rustc and rust-sdl. The following combinations are known to work:

- rustc 0.8 + rust-sdl `5598e68` 2013-10-03

If you use Windows and you are super lazy, then try the following:

1. Follow the [compilation instruction](https://github.com/lifthrasiir/angolmois/blob/master/README.md#how-to-compile-windows) of Angolmois up to the actual compilation.
2. Download and run [Rust 0.8 installer](http://static.rust-lang.org/dist/rust-0.8-install.exe).
3. Download and extract Angolmois Rust edition (direct link [here](https://github.com/lifthrasiir/angolmois-rust/archive/master.zip)) to some directory (let me call it `angolmois\`). Download and extract rust-sdl (direct link [here](https://github.com/brson/rust-sdl/archive/5598e6814cf39fad3f0a601122ab64c80c604ad2.zip)) to `angolmois\rust-sdl\`.
4. Run MinGW shell and `cd` into `angolmois\`. (Tip: `C:\Foo\Bar` translates to `/c/Foo/Bar`.)
5. Type `make` to compile. This will also compile rust-sdl as well.
6. Type `./angolmois` to run. See the remainder of Angolmois' compilation instruction for later usage.

### Why did you put everything to one file?

Mainly because this is a direct translation of Angolmois, which consists of a single C file with less than 2,000 lines of code. I tried to keep the entire structure of code while doing some refactoring since this makes the translation much more obvious. (This also explains why we have rather big bindings; the original Angolmois was also self-contained besides from SDL libraries.) I intend to write more idiomatic version of Angolmois Rust Edition in the future.

### What's a difference between the Rust Edition and the original Angolmois?

In theory, Angolmois Rust Edition should be identical to Angolmois 2.0.0 alpha 2. In practice, there are some differences:

- They behave differently on the files with invalid UTF-8 sequences.
- SDL\_image and SDL\_mixer are known to be noticeably (up to 10x) slower than C.
- The C version is unable to handle 2,000 or more measures in the display. (Fixed in 2.0 alpha 3)
- The C version allows a non-native path separator in `#PATH_WAV`; the Rust Edition doesn't.
- The C version allows "rewinding" the movie by repeatedly setting the same alphanumeric key to the BGA layer; the Rust Edition doesn't. I'm not sure that this *feature* is portable at all.

Please also note that the Rust Edition, based on 2.0.0 alpha 2, is also substantially different to 2.0 alpha 3 in the development. Targeting 2.0 alpha 3 is not hard but currently blocked by the immaturity of SDL 2.0 bindings for Rust.

