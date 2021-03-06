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
Currently, you'll need atleast version 0.8.0-dev.2641+55811d8da to build Luf.

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

Currently there's support for syntax highlighting for vscode, which can be found [here](editors/vscode).


