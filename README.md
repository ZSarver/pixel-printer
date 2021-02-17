# pixel-printer
Pixel-printer is a utility for turning pixel art into 3d prints.

## Installation
Use `stack install pixel-printer` or `cabal install pixel-printer`

## Usage
`stack run filename` or `cabal run pixel-printer-exe filename` will output
[OpenSCAD](https://www.openscad.org/index.html) code to standard out to do as
you please. To get a file, redirect like so `stack run filename > filename.scad`
or `cabal run pixel-printer-exe filename > filename.scad`. To get an STL from
here, open the OpenSCAD code in OpenSCAD.

Any filetype supported by
[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) will work.

## Roadmap
- [x] v0.1 - Reads file given on command line, outputs .scad code to stdout.
      Height, width, length predefined
- [ ] v0.2 - Command line options for:
    - [x] invert height
    - [ ] pixel width, pixel length, mutally exclusive with
    - [ ] total width, total length
    - [ ] height
    - [ ] output to a file
- [ ] v0.3 - stl output
    - [ ] ascii
    - [ ] binary
- [ ] v0.4 - Presets
    - [ ] lithophane mode
- [ ] v1.0 - same height pixel output, useful for painting the final product
    - [ ] paint by pixels
