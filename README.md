# Pelemay Backend (Collection)

<!-- MODULEDOC -->
A memory-saving, fault-tolerant and distributed collection of Nx compilers and
backends for embedded systems.
<!-- MODULEDOC -->

This repository currently holds the following projects:

Backends:

* [`PelemayBackend`](https://github.com/zeam-vm/pelemay_backend/tree/main/backends/pelemay_backend#readme) (WIP) - A memory-saving, fault-tolerant and distributed collection of Nx compilers and backends for embedded systems.
* [`LoggingBackend`](https://github.com/zeam-vm/pelemay_backend/blob/main/backends/logging_backend#readme) (WIP) - A backend to log the behavior of the specified `based_backend`.

Utilities:

* [`BackendDecorator`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/backend_decorator#readme) (WIP) - A backend generator to decorate the specified `based_backend` with the functions before and after a set of functions in the backend. The set can be specified with the style of [AspectJ, which is an AOP language](https://en.wikipedia.org/wiki/Aspect-oriented_programming), and with grouping written in [hexdocs of Nx](https://hexdocs.pm/nx/Nx.html), for example, Aggregates, Backend, Conversion, and so on.
* [`NodeActivator`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/node_activator#readme) [![Hex.pm](https://img.shields.io/hexpm/v/node_activator.svg?style=flat&color=blue)](https://hex.pm/packages/node_activator)  [![Elixir CI (NodeActivator) status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_node_activator.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_node_activator.yml/badge.svg) - A module to activate VM nodes.
* [`SpawnCoElixir`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/spawn_co_elixir#readme) [![Hex.pm](https://img.shields.io/hexpm/v/spawn_co_elixir.svg?style=flat&color=blue)](https://hex.pm/packages/spawn_co_elixir) [![Elixir CI (SpawnCoElixir) status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_spawn_co_elixir.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_spawn_co_elixir.yml/badge.svg) - SpawnCoElixir spawns cooperative Elixir nodes that are supervised.
* [`HttpDownloader`](https://github.com/zeam-vm/pelemay_backend/blob/main/utilities/http_downloader#readme) [![Hex.pm](https://img.shields.io/hexpm/v/http_downloader.svg?style=flat&color=blue)](https://hex.pm/packages/http_downloader) [![Elixir CI (HttpDownloader) status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_http_downloader.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_http_downloader.yml/badge.svg) - Downloads remote file with progress bar.


Benchmarks:

* [`DistributedComputingBench`](https://github.com/zeam-vm/pelemay_backend/blob/main/benchmarks/distributed_computing_bench#readme) [![Elixir CI (DistributedComputingBench) status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_distributed_computing_bench.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci_distributed_computing_bench.yml/badge.svg)

Each has their own README, which you can access above to learn more.

## Supported Erlang/OTP and Elixir versions:

Supported Erlang/OTP and Elixir versions:

* OTP: 25, 26, 27
* Elixir: 1.16, 1.17, 1.18

## Supported Platforms

Tested Platforms by CI:

* Ubuntu 22.04, 24.04

Other manually tested platforms:

* macOS 14 Sonoma (Apple Silicon and x86_64)
* groovEPIC

Not tested by CI:

* macOS 13 Ventura and 12 Monterey (Apple Silicon and x86_64)

Temporally not tested by CI of Nerves:

* rpi4
* rpi3a, rpi3, rpi2, rpi0, rpi
* bbb
* osd32mp1
* npi_imx6ull
* grisp2
* mangopi_mq_pro

Temporally unsupportted:

* Windows 2022

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
