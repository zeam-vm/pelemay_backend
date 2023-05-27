# Pelemay Backend (Collection)

[![Elixir CI status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/ci.yml/badge.svg) [![Nerves CI status](https://github.com/zeam-vm/pelemay_backend/actions/workflows/nerves-build.yml/badge.svg)](https://github.com/zeam-vm/pelemay_backend/actions/workflows/nerves-build.yml/badge.svg)


This repository currently holds the following projects:

Backends:

* [`PelemayBackend`](https://github.com/zeam-vm/pelemay_backend/tree/main/backends/pelemay_backend#readme) - A memory-saving, fault-tolerant and distributed collection of Nx compilers and backends for embedded systems.
* LoggingBackend - A backend to log the behavior of the specified `based_backend`.

Utilities:

* BackendDecorator - A backend generator to decorate the specified `based_backend` with the functions before and after a set of functions in the backend. The set can be specified with the style of [AspectJ, which is an AOP language](https://en.wikipedia.org/wiki/Aspect-oriented_programming), and with grouping written in [hexdocs of Nx](https://hexdocs.pm/nx/Nx.html), for example, Aggregates, Backend, Conversion, and so on.

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

## Acknowledgement

This work was supported by Asahi Kohsan Group Research Support Program of Kitakyushu Foundation for the Advancement of Industry Science and Technology (FAIS).
