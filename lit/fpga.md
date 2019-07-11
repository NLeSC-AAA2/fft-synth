# Run models

For testing a function from the host, we can call a kernel directly if it is decorated with `__kernel` and the appropriate name spaces in the arguments. Usually this is not how the Fourier transforms will be called. If we just leave the codelets in their original C form, we can call them from inside other kernels. These calls are then inlined by the OpenCL compiler.

There are three modes to run our functions:

- `__kernel` from host directly
- write a small `main` kernel to test kernel on GPU
- use channels to compute on input stream

The third option is not entirely trivial.

## Channels
A generic channel component can be tested as follows. We first enable the `cl_intel_channels` extension.

``` {.opencl #enable-channel-extension}
#pragma OPENCL EXTENSION cl_intel_channels : enable
```

Define input and output channels,

``` {.opencl #define-channels}
channel float2 in_channel, out_channel;
```

and have a `autorun` kernel that runs an infinite loop.

``` {.opencl #autorun-kernel}
__kernel
__attribute__((autorun))
__attribute__((max_global_work_dim(0)))
void do_fft()
{
  while (true) {
    <<-compute->>
  }
}
```

Inside the computation we read from input and write to output,

``` {.opencl #-compute-}
float2 a[CHUNK_SIZE];

for (unsigned i = 0; i < CHUNK_SIZE; ++i)
  a[i] = read_channel_intel(in_channel);

// do Fourier transform

for (unsigned i = 0; i < CHUNK_SIZE; ++i)
  write_channel_intel(out_channel, a[i]);
}
```

where `CHUNK_SIZE` is some integer constant. We'll define two more kernels that fill the input channel from some source,

``` {.opencl #channel-source}
__attribute__((max_global_work_dim(0)))
__kernel void source(__global const volatile float2 *in, unsigned count)
{
  for (unsigned i = 0; i < count; i ++)
    write_channel_intel(in_channel, in[i]);
}
```

and read the output channel onto some sink.

``` {.opencl #channel-sink}
__attribute__((max_global_work_dim(0)))
__kernel void sink(__global float2 *out, unsigned count)
{
  for (unsigned i = 0; i < count; i ++)
    out[i] = read_channel_intel(out_channel);
}
```

This channel architecture enables a form of composability that matches the data-flow character of the FPGA.

## Auto-kernel generator

Given an input and output channel name we can automatically generate a Fourier transform auto-kernel.

