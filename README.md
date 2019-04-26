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
|--+ app          (* application source files *)
|--+ docs         (* generated HTML documentation *)
|--+ genfft       (* "classic" FFTW3 genfft *)
|--+ lit          (* literate code sources *)
|--+ scripts      (* shell scripts for weaving *)
|--+ src          (* library source files *)
|--+ test         (* unit tests *)
|- README.md
|- LICENSE        (* GPL3 *)
```

