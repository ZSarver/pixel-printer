# pixel-printer
Pixel-printer is a utility for turning pixel art into 3d prints.

## Installation from Hackage
Make sure that you've installed
[Stack](https://docs.haskellstack.org/en/stable/README/) or
[Cabal](https://www.haskell.org/cabal/). Ensure you have a semi-recent version
of stack (version 2.5 or higher) or cabal (version 3.0 or higher.) After that,
you can install using the commands `stack install --resolver lts-16.31
pixel-printer` or `cabal install pixel-printer`

## Basic Usage
`pixel-printer image.png` or `cabal run pixel-printer-exe image.png` will output
[OpenSCAD](https://www.openscad.org/index.html) code to the file "out.scad" in
the current directory. To get an STL from here, open and compile the OpenSCAD
code in OpenSCAD.

While I use `.png` in the examples above, most common image formats are
supported. Any filetype supported by
[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) will work.

You can get more information about supported command line options with
`pixel-printer --help`. These command line options allow you to for example set
the width of the final output and the output file with `pixel-printer --width 42
-o my-cool-file.scad my-cool-image.png`.

## Dev Installation
You will need stack or cabal as above. To install from source, clone this
repository and navigate to it in your command line shell of choice. From there
you can use `stack build` or `cabal build` to build from source, and `stack run`
or `cabal exec` to run the program.

## Roadmap
- [x] v0.1 - Reads file given on command line, outputs .scad code to stdout.
      Height, width, length predefined
- [ ] v0.2 - Command line options for:
    - [x] invert height
    - [x] total width, total length
    - [x] height
    - [x] output to a file
    - [ ] determine pixel height based on hue rather than brightness
- [ ] v0.3 - stl output
    - [ ] ascii
    - [ ] binary
- [ ] v0.4 - Presets
    - [ ] lithophane mode
- [ ] v1.0 - same height pixel output, useful for painting the final product
    - [ ] paint by pixels
