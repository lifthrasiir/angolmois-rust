# Angolmois Rust Edition

[![Angolmois Rust Edition on Travis CI][travis-image]][travis]

[travis-image]: https://travis-ci.org/lifthrasiir/angolmois-rust.png
[travis]: https://travis-ci.org/lifthrasiir/angolmois-rust

This is a direct, one-to-one translation of Angolmois to [Rust](http://www.rust-lang.org/) programming language. [Angolmois](http://mearie.org/projects/angolmois/) is a [BM98](http://bm98.yaneu.com/bm98/)-like minimalistic music video game which supports the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.

Angolmois is a combination of string parsing, path manipulation, two-dimensional graphics and complex game play carefully packed into some thousand lines of code. This translation is intended to provide an example of translating a moderately-sized C code to Rust, and also to investigate additional library supports required for such moderately-sized programs. The resulting translation is certainly one of the largest applications written in Rust to date (most Rust projects are libraries for now), weighing more than 6,000 lines of Rust code. This is partly due to the fact that, unlike the original Angolmois code (which sacrifices most comments due to code size concerns), the Rust version has much more comments which can be beneficial for understanding Angolmois itself too.

For more informations (especially the usage), please refer to the [README](https://github.com/lifthrasiir/angolmois/) file of the original Angolmois. If you want to learn more about [what the BMS format is](https://github.com/lifthrasiir/angolmois/blob/master/README.md#appendix-what-is-bms) and [how Angolmois is structured](https://github.com/lifthrasiir/angolmois/blob/master/INTERNALS.md), it turns out that I've already written a lot about them.

## Screenshots

![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois1.jpg?raw=true) 
![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois2.jpg?raw=true)  
*Game data credits: "sunken azure world", music by maki, courtesy of [Daida Three Brothers](http://daida.tv/). All rights reserved.*

## License

Angolmois is distributed under GNU GPL version 2+, so is this translation. The portions of it is intended to be sent as a patch to Rust, so those portions are licensed under Apache License 2.0 and MIT license. See the code for specifics.

## FAQ

### How to compile?

Clone this repo and run the following commands (yes, you need the MinGW shell for Windows):

    cargo build --release
    target/release/angolmois --help

You require the most recent Rust master and [Cargo](https://github.com/rust-lang/cargo). You can install nightlies for both.

### Why did you put everything to one file?

Mainly because this is a direct translation of Angolmois, which consists of a single C file with less than 2,000 lines of code. I tried to keep the entire structure of code while doing some refactoring since this makes the translation much more obvious. This also explains why we have rather big bindings; the original Angolmois was also self-contained besides from SDL libraries.

There is now a fork of Angolmois called [Sonorous](https://github.com/snrs/sonorous) which has outgrown the original to become a full-featured game. If you don't like one big file, please consider taking a look at it.

### What's a difference between the Rust Edition and the original Angolmois?

In theory, Angolmois Rust Edition should be identical to Angolmois 2.0.0 alpha 2. In practice, there are some differences:

- They behave differently on the files with invalid UTF-8 sequences.
- The Rust version caches the directory entries in order to avoid the performance penalty due to the different directory listing semantics.
- The C version is unable to handle 2,000 or more measures in the display. (Fixed in 2.0 alpha 3)
- The C version allows a non-native path separator in `#PATH_WAV`; the Rust Edition doesn't.
- The C version allows "rewinding" the movie by repeatedly setting the same alphanumeric key to the BGA layer; the Rust Edition doesn't. I'm not sure that this *feature* is portable at all.

Please also note that the Rust Edition, based on 2.0.0 alpha 2, is also substantially different to 2.0 alpha 3 in the development. Targeting 2.0 alpha 3 is not hard but currently blocked by the immaturity of SDL 2.0 bindings for Rust.

