# Angolmois Rust Edition

*2013-07-06*

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

- rustc 0.7 + rust-sdl `48cb490` 2013-07-02 (an unmerged branch from sstewartgallus/rust-sdl)

If you use Windows and you are super lazy, then try the following:

1. Follow the [compilation instruction](https://github.com/lifthrasiir/angolmois/blob/master/README.md#how-to-compile-windows) of Angolmois up to the actual compilation.
2. Download and run [Rust 0.7 installer](http://static.rust-lang.org/dist/rust-0.7-install.exe).
3. Download and extract Angolmois Rust edition (direct link [here](https://github.com/lifthrasiir/angolmois-rust/archive/master.zip)) to some directory (let me call it `angolmois\`). Download and extract rust-sdl (direct link [here](https://github.com/sstewartgallus/rust-sdl/archive/48cb49047c48a16ad2c75f4d0fa225c4de5a101d.zip)) to `angolmois\rust-sdl\`.
4. Run MinGW shell and `cd` into `angolmois\`. (Tip: `C:\Foo\Bar` translates to `/c/Foo/Bar`.)
5. Type `make` to compile. This will also compile rust-sdl as well.
6. Type `./angolmois` to run. See the remainder of Angolmois' compilation instruction for later usage.

### Why did you put everything to one file?

Mainly because this is a direct translation of Angolmois, which consists of a single C file with less than 2,000 lines of code. I tried to keep the entire structure of code while doing some refactoring since this makes the translation much more obvious. (This also explains why we have rather big bindings; the original Angolmois was also self-contained besides from SDL libraries.) I intend to write more idiomatic version of Angolmois Rust Edition in the future.

