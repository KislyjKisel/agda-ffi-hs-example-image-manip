# Image manipulation example for agda-ffi-hs

An example demonstrating usage of [`agda-ffi-hs`](https://github.com/KislyjKisel/agda-ffi-hs) to build interactive graphical application in Agda with OpenGL, SDL2 and DearImGui.

## Screenshot

![Example](/screenshot.png)

## Features

* Resizing (nearest neighbor, 2 variants of bilinear)
* Rotation
* Filters (3x3: median, convolutional)

## Requirements

* `ghc-9.4`
* `cabal-2.4`
* `libsdl2`
* Agda compatible with `agda-ffi-hs` (ffi pragmas with type arity annotation)
* [`agda-stdlib`](https://github.com/agda/agda-stdlib) experimental
* [`agda-ffi-hs`](https://github.com/KislyjKisel/agda-ffi-hs) (tested with `133e2d0`)

## Running

```sh
./build.sh && cabal run
```
