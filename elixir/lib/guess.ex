defmodule Guess do
  def send_guess(id, cookie, word) do
    url = "https://wordle.we4shakthi.in/game/guess"
    headers = [
      {"Content-Type", "application/json"},
      {"Cookie", cookie}
    ]

    body = Jason.encode!(%{id: id, word: word})

    case HTTPoison.post(url, body, headers) do
      {:ok, %HTTPoison.Response{status_code: code, body: raw_body}} ->
        IO.puts("STATUS: #{code}")
        IO.puts("RESPONSE BODY:\n" <> raw_body)

        case Jason.decode(raw_body) do
          {:ok, json} -> json
          {:error, _} -> %{"error" => "Response is not JSON"}
        end

      {:error, reason} ->
        IO.inspect(reason)
        %{"error" => "Request failed"}
    end
  end
end
