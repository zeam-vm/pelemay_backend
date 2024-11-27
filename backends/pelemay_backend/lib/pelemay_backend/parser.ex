defmodule PelemayBackend.Parser do
  import NimbleParsec

  defparsec(:parse_integer, ignore(repeat(string(" "))) |> integer(min: 1) |> tag(:integer))

  defparsec(
    :parse_atom,
    ignore(repeat(string(" ")))
    |> ignore(string(":"))
    |> ascii_string([?a..?z], min: 1)
    |> tag(:atom)
  )

  defparsec(
    :parse_bool,
    ignore(repeat(string(" "))) |> choice([string("true"), string("false")]) |> tag(:atom)
  )

  defparsec(
    :parse_string,
    ignore(repeat(string(" ")))
    |> ignore(string("'"))
    |> utf8_string([not: ?\'], min: 0)
    |> ignore(string("'"))
    |> tag(:string)
  )

  defcombinator(
    :args,
    ignore(repeat(string(" ")))
    |> parsec(:parse_constant)
    |> repeat(
      ignore(repeat(string(" ")))
      |> ignore(string(","))
      |> ignore(repeat(string(" ")))
      |> concat(parsec(:parse_constant))
    )
  )

  defparsec(
    :parse_tuple,
    ignore(repeat(string(" ")))
    |> ignore(string("{"))
    |> parsec(:args)
    |> ignore(string("}"))
    |> tag(:tuple)
  )

  defparsec(
    :parse_constant,
    choice([
      parsec(:parse_integer),
      parsec(:parse_atom),
      parsec(:parse_bool),
      parsec(:parse_tuple),
      parsec(:parse_string),
      empty()
    ])
    |> ignore(repeat(string(" ")))
  )
end
