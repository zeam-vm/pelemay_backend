# SpawnCoElixir

<!-- MODULEDOC -->
SpawnCoElixir spawns cooperative Elixir nodes that are supervised.
<!-- MODULEDOC -->

## Installation

Just add `spawn_co_elixir` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:spawn_co_elixir, "~> 0.1.0"}
  ]
end
```

Documentation can be found at <https://hexdocs.pm/spawn_co_elixir>.

## Procedure when publishing

1. Publish NodeActivator before publishing SpawnCoElixir;
2. Modify `deps` in `mix.exs` to use `node_activator` of the latest hex package instead of relative path;
3. Publish SpawnCoElixir;
4. Modify `deps` in `run` in `lib/spawn_co_elixir/co_elixir_worker_spawner.ex` to use `spawn_co_elixir` of the latest hex package instead of relative path;
5. Republish SpawnCoElixir. 

## License

Copyright (c) 2023 University of Kitakyushu

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

## Acknowledgement

This work was supported by Asahi Kohsan Group Research Support Program of
Kitakyushu Foundation for the Advancement of Industry Science and Technology
(FAIS).
