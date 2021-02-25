# Changelog for pixel-printer

## v0.1.2 February 2021
* Added --length and --width options to adjust the length and width of the final
  output object
* Changed all internal calculations and output from Int to Double for sub-mm
  resolution.

## v0.1.1 February 2021
* Added an --invert command line option so that you can choose whether lighter
  pixels are taller or darker pixels are taller.
* Fixed a bug where pixel height was getting clamped due to erroneously doing
  calculations with 8-bit integers.
* Updated readme

## v0.1.0 February 2021
* Initial release.
