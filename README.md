# FFT Synthesis

Project goals:

* Modify FFTW3's GenFFT to output inline OpenCL kernels.
* Generate OpenCL code to synthesise larger FFTs (create inline plans).
* Optimize for Intel (former Altera) FPGA OpenCL pipeline + compiler extensions
* Modernise and document FFTW3's codebase

## Development setup

FFTW3 GenFFT is written in OCaml. We modified some parts of this code to create valid OpenCL code.

The part where we synthesise larger FFTs from the FFTW codelets is written in Haskell.

## Project structure

```
project root
|--+ lit          (* literate code sources *)
|--+ genfft       (* "classic" FFTW3 genfft *)
|--+ src          (* library source files *)
|--+ app          (* application source files *)
|- Makefile
|- README.md
|- LICENSE        (* GPL3 *)
```

