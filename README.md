# Luf #
Luf is a statically typed embeddable scripting language written in [Zig](https://ziglang.org).

The goal of this project is to create a simple, expressive scripting language that can be used to implement new ideas. As most of it is experimental, I would currently not recommend this for any serious use.

## Resources
- [Examples](examples)
- [Documentation](docs/README.md)
- [Building from source](#building-from-source)
- [Contributing](CONTRIBUTING)
- [Editor Support](#editor-support)

## Building from source
[![Linux status](https://github.com/Luukdegram/luf/workflows/Linux/badge.svg)](https://github.com/Luukdegram/luf/actions) 
[![Windows](https://github.com/Luukdegram/luf/workflows/Windows/badge.svg)](https://github.com/Luukdegram/luf/actions) [![MacOS](https://github.com/Luukdegram/luf/workflows/MacOS/badge.svg)](https://github.com/Luukdegram/luf/actions)

I try to keep up with the latest version of Zig to be able to use all of its features (and bug fixes).
Currently, Luf expects atleast version `0.6.0+69de1a51c` while an earlier version may work.

### Building

To build Luf as a static library, execute the following Zig command:
```
zig build
```

### Tests

Currently all tests are written in Zig, but there's plans to also write behavioural tests in Luf.

To run all tests, execute the following command:
```
zig build test
```

## Editor support

As of yet, no editor support has been implemented, but VS Code will be the primary target as that is what I'm currently using. Other possible targets are Vim and Kakoune.


