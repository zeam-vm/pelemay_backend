---
layout: _layout.slime
---
# Pelemay Backend

[![Elixir CI status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci.yml/badge.svg) [![Nerves CI status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/nerves-build.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/nerves-build.yml/badge.svg)

A memory-saving, fault-tolerant and distributed collection of Nx compilers and
backends for embedded systems.

This repository currently holds the following projects:

Backends:

* [`PelemayBackend`](https://github.com/zeam-vm/pelemay_backend/tree/main/backends/pelemay_backend#readme) (WIP) - A memory-saving, fault-tolerant and distributed collection of Nx compilers and backends for embedded systems.
* [`LoggingBackend`](https://github.com/zeam-vm/pelemay_backend/blob/main/backends/logging_backend#readme) (WIP) - A backend to log the behavior of the specified `based_backend`.

Utilities:

* [`BackendDecorator`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/backend_decorator#readme) (WIP) - A backend generator to decorate the specified `based_backend` with the functions before and after a set of functions in the backend. The set can be specified with the style of [AspectJ, which is an AOP language](https://en.wikipedia.org/wiki/Aspect-oriented_programming), and with grouping written in [hexdocs of Nx](https://hexdocs.pm/nx/Nx.html), for example, Aggregates, Backend, Conversion, and so on.
* [`NodeActivator`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/node_activator#readme) - A module to activate VM nodes.

Benchmarks:

* [`OnnxToAxonBench`](https://github.com/zeam-vm/pelemay_backend/blob/main/benchmarks/onnx_to_axon_bench#readme) - A benchmark program of loading ONNX to Axon. The results deny the hope to use ResNet, which is one of popular models that perform image classification, for embedded systems due to too much memory consumption. This fact supports the concept of the Pelemay Backend.

Each has their own README, which you can access above to learn more.

## Supported Erlang/OTP and Elixir versions:

Supported Erlang/OTP and Elixir versions:

* OTP: 24, 25, 26
* Elixir: 1.14, 1.15

## Supported Platforms

Tested Platforms by CI:

* Ubuntu 22.04 / 20.04
* macOS 12 Monterey / 13 Ventura (x86_64)
* Windows 2022

Nerves platforms tested only building by CI:

* rpi4, rpi3a, rpi3, rpi2, rpi0, rpi
* bbb
* osd32mp1
* npi_imx6ull
* grisp2
* mangopi_mq_pro

Other manually tested platforms:

* macOS 13 Ventura (Apple Silicon)
* groovEPIC

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

This work was supported by Asahi Kohsan Group Research Support Program of Kitakyushu Foundation for the Advancement of Industry Science and Technology (FAIS).
