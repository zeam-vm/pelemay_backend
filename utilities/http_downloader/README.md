# HttpDownloader

<!-- MODULEDOC -->
Downloads remote file with progress bar.
<!-- MODULEDOC -->

## Installation

Just add `http_downloader` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:http_downloader, "~> 0.1.0",
     github: "zeam-vm/pelemay_backend", sparse: "utilities/http_downloader"}
  ]
end
```

In notebooks and scripts, use the following `Mix.install/2` call:

```elixir
Mix.install(
  [
    {:http_downloader, "~> 0.1.0",
     github: "zeam-vm/pelemay_backend", sparse: "utilities/http_downloader"}
  ]
)
```

## Usage

```elixir
{:ok, data} = HttpDownloader.download!("http://speedtest.ftp.otenet.gr/files/test100k.db")
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

This work was supported by Asahi Kohsan Group Research Support Program of
Kitakyushu Foundation for the Advancement of Industry Science and Technology
(FAIS).
