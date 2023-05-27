#!/usr/bin/env elixir

defmodule Sub do
  def get_app(dir) do
    dir
    |> Path.basename()
    |> String.to_atom()
  end

  def mix_exs_path(subdir) do
    subdir
    |> Enum.map(&{get_app(&1), Path.join(&1, "mix.exs")})
    |> Enum.filter(fn {_app, mix_exs_path} -> File.exists?(mix_exs_path) end)
  end

  def mix_exs_stream(mix_exs_path) do
    File.stream!(mix_exs_path)
  end

  defp get_release_versions(mix_exs_stream) do
    mix_exs_stream
    |> Stream.map(&Regex.named_captures(~r/\@version \"(?<version>.*[^dev])\"/, &1))
    |> Stream.reject(&is_nil/1)
    |> Stream.map(&Map.get(&1, "version"))
    |> Enum.to_list()
  end

  def get_release_version(mix_exs) do
    versions = get_release_versions(mix_exs)

    case versions do
      [] -> nil
      [version] -> version
      _ -> raise RuntimeError, "define multiple versions"
    end
  end

  def app_version(subdir) do
    subdir
    |> Sub.mix_exs_path()
    |> Enum.map(fn {app, path} ->
      {
        app,
        path
        |> Sub.mix_exs_stream()
        |> Sub.get_release_version()
      }
    end)
    |> Map.new()
  end

  def mix_exs_streams(subdir) do
    subdir
    |> Sub.mix_exs_path()
    |> Enum.map(fn {_app, path} ->
      {
        path,
        Sub.mix_exs_stream(path)
      }
    end)
  end

  def replace_to_release(subdir) do
    replace_table =
      app_version(subdir)
      |> Enum.reject(fn {_app, version} -> is_nil(version) end)
      |> Enum.map(fn {app, version} ->
        {
          app,
          {
            "{#{inspect(app)}, .*}" |> Regex.compile!(),
            "{#{inspect(app)}, \"~> #{version}\"}"
          }
        }
      end)

    subdir
    |> Sub.mix_exs_streams()
    |> Enum.map(fn {path, stream} ->
      {
        path,
        Stream.map(stream, fn line ->
          replace_table
          |> Enum.reduce(line, fn {_app, {regex, replacement}}, line ->
            Regex.replace(regex, line, replacement)
          end)
        end)
      }
    end)
  end

  def write_replaced_to_release(subdir) do
    replace_to_release(subdir)
    |> Enum.map(fn {path, stream} ->
      {
        path,
        stream
        |> Enum.to_list()
        |> Enum.join()
      }
    end)
    |> Enum.map(fn {path, content} ->
      File.write!(path, content)
    end)
  end
end

subdir = System.argv()

Sub.write_replaced_to_release(subdir)
