# BackendDecorator

A backend generator to decorate the specified `based_backend` with the functions before and after a set of functions in the backend. The set can be specified with the style of [AspectJ, which is an AOP language](https://en.wikipedia.org/wiki/Aspect-oriented_programming), and with grouping written in [hexdocs of Nx](https://hexdocs.pm/nx/Nx.html), for example, Aggregates, Backend, Conversion, and so on.

## Installation

```elixir
def deps do
  [
    {:backend_decorator, "~> 0.1.0", github: "zeam-vm/pelemay_backend", sparse: "utilities/backend_decorator"}
  ]
end
```
