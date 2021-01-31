# pixel-printer
A utility for turning pixel art into 3d prints

## Usage
`pixelprint filename` will output
[OpenSCAD](https://www.openscad.org/index.html) code to standard out to do as
you please. To get a file, redirect like so `pixelprint filename >
filename.scad`. To get an STL from here, open the OpenSCAD code in OpenSCAD.

Any filetype supported by
[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) will work.

## Roadmap
- [ ] v0.1 - Reads file given on command line, outputs .scad code to stdout.
      Height, width, length predefined
- [ ] v0.2 - Command line options for:
    - [ ] pixel width, pixel width, mutally exclusive with
    - [ ] total width, total width
    - [ ] height
    - [ ] output to a file
- [ ] v0.3 - stl output
- [ ] v1.0 - same height pixel output, useful for painting the final product
