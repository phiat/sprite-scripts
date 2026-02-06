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
      |> Enum.reduce(%{}, fn line, acc ->
        line = String.trim(line)

        cond do
          line == "" ->
            acc

          String.starts_with?(line, "#") ->
            acc

          not String.contains?(line, "=") ->
            acc

          true ->
            [key | rest] = String.split(line, "=", parts: 2)
            key = String.trim(key)
            value = rest |> List.first("") |> String.trim() |> strip_quotes()

            # Set in environment (don't overwrite existing)
            if System.get_env(key) == nil do
              System.put_env(key, value)
            end

            Map.put(acc, key, value)
        end
      end)
    else
      %{}
    end
  end

  @doc """
  Load configuration from .env file and environment variables.
  """
  @spec load() :: t()
  def load do
    env_file = System.get_env("ENV_FILE") || "./.env"

    # Parse .env file first (populates env for keys not already set)
    parse_env_file(env_file)

    # SPRITE_TOKEN with SPRITES_TOKEN fallback
    sprite_token = System.get_env("SPRITE_TOKEN") || System.get_env("SPRITES_TOKEN") || ""

    # Agent
    agent = System.get_env("AGENT") || "opencode"

    # Claude auth
    claude_auth = System.get_env("CLAUDE_AUTH") || "subscription"

    # Anthropic API key
    anthropic_api_key = System.get_env("ANTHROPIC_API_KEY") || ""

    # Model
    model = System.get_env("MODEL") || ""

    # Checkpoint interval
    interval_str = System.get_env("CHECKPOINT_INTERVAL") || "300"

    checkpoint_interval =
      case Integer.parse(interval_str) do
        {n, ""} ->
          n

        _ ->
          IO.puts(:stderr, "Error: invalid CHECKPOINT_INTERVAL #{inspect(interval_str)} (must be integer)")
          System.halt(1)
      end

    %__MODULE__{
      sprite_token: sprite_token,
      agent: agent,
      claude_auth: claude_auth,
      anthropic_api_key: anthropic_api_key,
      model: model,
      checkpoint_interval: checkpoint_interval,
      env_file: env_file
    }
  end

  # Strip matching single or double quotes from a value string.
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
