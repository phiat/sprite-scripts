defmodule SpriteTool.Config do
  @moduledoc """
  Configuration: .env parsing and environment variable loading.
  """

  defstruct sprite_token: "",
            agent: "opencode",
            claude_auth: "subscription",
            anthropic_api_key: "",
            model: "",
            checkpoint_interval: 300,
            env_file: "./.env",
            dry_run: false,
            checkpointing: true,
            upload_dirs: []

  @type t :: %__MODULE__{
          sprite_token: String.t(),
          agent: String.t(),
          claude_auth: String.t(),
          anthropic_api_key: String.t(),
          model: String.t(),
          checkpoint_interval: non_neg_integer(),
          env_file: String.t(),
          dry_run: boolean(),
          checkpointing: boolean(),
          upload_dirs: [String.t()]
        }

  @doc """
  Parse a .env file and set values into the process environment.

  Skips blank lines and comments. Parses KEY=VALUE with optional
  single/double quote stripping. Sets parsed values via System.put_env/2
  (does not overwrite existing env vars).
  """
  @spec parse_env_file(String.t()) :: %{String.t() => String.t()}
  def parse_env_file(path) do
    if File.regular?(path) do
      path
      |> File.read!()
      |> String.split("\n")
      |> Enum.reduce(%{}, &parse_env_line/2)
    else
      %{}
    end
  end

  @doc """
  Load configuration from .env file and environment variables.
  """
  @spec load() :: t()
  def load do
    env_file = env_or("ENV_FILE", "./.env")
    parse_env_file(env_file)

    %__MODULE__{
      sprite_token: env_or("SPRITE_TOKEN", env_or("SPRITES_TOKEN", "")),
      agent: env_or("AGENT", "opencode"),
      claude_auth: env_or("CLAUDE_AUTH", "subscription"),
      anthropic_api_key: env_or("ANTHROPIC_API_KEY", ""),
      model: env_or("MODEL", ""),
      checkpoint_interval: parse_interval(env_or("CHECKPOINT_INTERVAL", "300")),
      env_file: env_file
    }
  end

  defp env_or(key, default), do: System.get_env(key) || default

  defp parse_interval(str) do
    case Integer.parse(str) do
      {n, ""} -> n
      _ ->
        IO.puts(:stderr, "Error: invalid CHECKPOINT_INTERVAL #{inspect(str)} (must be integer)")
        System.halt(1)
    end
  end

  defp parse_env_line(line, acc) do
    line = String.trim(line)

    if line == "" or String.starts_with?(line, "#") or not String.contains?(line, "=") do
      acc
    else
      [key | rest] = String.split(line, "=", parts: 2)
      key = String.trim(key)
      value = rest |> List.first("") |> String.trim() |> strip_quotes()
      if System.get_env(key) == nil, do: System.put_env(key, value)
      Map.put(acc, key, value)
    end
  end

  defp strip_quotes(value) when byte_size(value) >= 2 do
    first = String.first(value)
    last = String.last(value)

    if first == last and first in ["\"", "'"] do
      String.slice(value, 1..-2//1)
    else
      value
    end
  end

  defp strip_quotes(value), do: value
end
