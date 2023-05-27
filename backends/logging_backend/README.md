# LoggingBackend

A backend to log the behavior of the specified `based_backend`.

## Installation

```elixir
def deps do
  [
    {:logging_backend, "~> 0.1.0", github: "zeam-vm/pelemay_backend", sparse: "backends/logging_backend"}, 
    {:backend_decorator, "~> 0.1.0", github: "zeam-vm/pelemay_backend", sparse: "utilities/backend_decorator", override: true}
  ]
end
```
