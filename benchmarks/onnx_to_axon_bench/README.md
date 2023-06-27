# OnnxToAxonBench

<!-- MODULEDOC -->
A benchmark program of loading ONNX to Axon.
<!-- MODULEDOC -->

## Benchmark Results

Here are the results that are obtained when running on an M2 MacBook Air:

```elixir
% cd benchmarks/onnx_to_axon_bench
% mix run -e "OnnxToAxonBench.run"

05:29:35.627 [info] File cats_v_dogs.onnx has already been downloaded.

05:29:35.628 [info] File cat_dog_breeds.onnx has already been downloaded.

05:29:35.629 [info] File resnet101-v1-7.onnx has already been downloaded.
Operating System: macOS
CPU Information: Apple M2
Number of Available Cores: 8
Available memory: 24 GB
Elixir 1.14.4
Erlang 26.0

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 2 s
reduction time: 0 ns
parallel: 1
inputs: cat_dog_breeds.onnx, cats_v_dogs.onnx, resnet101-v1-7.onnx
Estimated total run time: 27 s

Benchmarking AxonOnnx.import |> Nx.serialize with input cat_dog_breeds.onnx ...
Benchmarking AxonOnnx.import |> Nx.serialize with input cats_v_dogs.onnx ...
Benchmarking AxonOnnx.import |> Nx.serialize with input resnet101-v1-7.onnx ...

05:30:16.802 [warning] N has no specified dimension, assuming nil

05:30:39.132 [warning] N has no specified dimension, assuming nil

05:31:02.724 [warning] N has no specified dimension, assuming nil

##### With input cat_dog_breeds.onnx #####
Name                                      ips        average  deviation         median         99th %
AxonOnnx.import |> Nx.serialize        246.91        4.05 ms     ±6.85%        3.98 ms        4.98 ms

Memory usage statistics:

Name                                    average  deviation         median         99th %
AxonOnnx.import |> Nx.serialize         1.04 MB     ±0.01%        1.04 MB        1.04 MB

##### With input cats_v_dogs.onnx #####
Name                                      ips        average  deviation         median         99th %
AxonOnnx.import |> Nx.serialize        249.56        4.01 ms     ±4.53%        3.95 ms        4.68 ms

Memory usage statistics:

Name                                    average  deviation         median         99th %
AxonOnnx.import |> Nx.serialize         1.04 MB     ±0.01%        1.04 MB        1.04 MB

##### With input resnet101-v1-7.onnx #####
Name                                      ips        average  deviation         median         99th %
AxonOnnx.import |> Nx.serialize        0.0455        21.96 s     ±0.00%        21.96 s        21.96 s

Memory usage statistics:

Name                               Memory usage
AxonOnnx.import |> Nx.serialize         8.97 GB

**All measurements for memory usage were the same**
% ls -l _build/dev/lib/onnx_to_axon_bench/priv/models/onnx/
total 533544
-rw-r--r--  1 zacky  staff   47162250  6  6 11:13 cat_dog_breeds.onnx
-rw-r--r--  1 zacky  staff   47090548  6  6 11:13 cats_v_dogs.onnx
-rw-r--r--  1 zacky  staff  178914043  6  6 12:34 resnet101-v1-7.onnx
```

These suggest that the concept of Pelemay Backend makes sense as follows:

* The size of files `cat_dog_breeds` and `cats_v_dogs.onnx` is approximately 47MB, and they requires approximately the memory of 1MB.
* In contrast, the size of file `resnet101-v1-7.onnx` is approximately 178MB, and it requires approximately the memory of 9GB.

`resnet101-v1-7.onnx` is one of files in [ResNet](https://github.com/onnx/models/blob/main/vision/classification/resnet/README.md), which is one of popular models that perform image classification. So, many developers may hope to use it on embedded systems. However, the results deny this hope.

## License

Copyright (c) 2023 University of Kitakyushu

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

## Acknowledgement

The author of the prototype of this is Goody27.

This work was supported by Asahi Kohsan Group Research Support Program of Kitakyushu Foundation for the Advancement of Industry Science and Technology (FAIS).

