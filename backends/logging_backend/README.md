# LoggingBackend

<!-- MODULEDOC -->
A backend to log the behavior of the specified `based_backend`.
<!-- MODULEDOC -->

## Installation

```elixir
def deps do
  [
    {:logging_backend, "~> 0.1.0-dev", github: "zeam-vm/pelemay_backend", sparse: "backends/logging_backend"},
    {:backend_decorator, "~> 0.1.0-dev", github: "zeam-vm/pelemay_backend", sparse: "utilities/backend_decorator", override: true}
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
