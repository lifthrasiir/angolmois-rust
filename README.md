# Angolmois Rust Edition

*2013-05-27*

This is a direct, one-to-one translation of Angolmois to [Rust](http://www.rust-lang.org/) programming language. [Angolmois](http://mearie.org/projects/angolmois/) is a [BM98](http://bm98.yaneu.com/bm98/)-like minimalistic music video game which supports the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.

Angolmois is a combination of string parsing, path manipulation, two-dimensional graphics and complex game play carefully packed into some thousand lines of code. This translation is intended to provide an example of translating a moderately-sized C code to Rust, and also to investigate additional library supports required for such moderately-sized programs. The resulting translation is certainly one of the largest applications written in Rust to date (most Rust projects are libraries for now), weighing more than 6,000 lines of Rust code.

Angolmois is distributed under GNU GPL version 2+, so is this translation. The portions of it is intended to be sent as a patch to Rust, so those portions are licensed under Apache License 2.0 and MIT license. Also note that:

- This code is known to compile with the following combinations of rustc and rust-sdl:
    - rustc 0.6 + rust-sdl `999abbc` 2013-04-13, with `--cfg legacy` option
    - rustc 0.6 `510d0f2` 2013-05-25 (pre-0.7) + rust-sdl `efa4b24` 2013-05-12 (an unmerged branch from sstewartgallus/rust-sdl) + `s/core::/std::/g` in rust-sdl

- Unlike the original Angolmois code (which sacrifices most comments due to code size concerns), the Rust version has much more comments which can be beneficial for understanding Angolmois itself too.

I'm currently writing a detailed post on this translation; I hope it to be a starting point to the advanced programming in Rust. For the other things, please refer to the [README](https://github.com/lifthrasiir/angolmois/) file of the original Angolmois. If you want to learn more about [what the BMS format is](https://github.com/lifthrasiir/angolmois/blob/master/README.md#appendix-what-is-bms) and [how Angolmois is structured](https://github.com/lifthrasiir/angolmois/blob/master/INTERNALS.md), it turns out that I've already written a lot about them.

## Screenshots

![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois1.jpg?raw=true) 
![](../b106fef2c19cd093ea84de75bbd5f7b3e2aa84d9/img/angolmois2.jpg?raw=true)  
*Game data credits: "sunken azure world", music by maki, courtesy of [Daida Three Brothers](http://daida.tv/). All rights reserved.*

