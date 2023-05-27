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
