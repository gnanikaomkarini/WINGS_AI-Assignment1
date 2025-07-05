defmodule Register do
  @url "https://wordle.we4shakthi.in/game"

  def start(name \\ "harini") do
    body = Jason.encode!(%{mode: "wordle", name: name})
    headers = [{"Content-Type", "application/json"}]

    case HTTPoison.post("#{@url}/register", body, headers) do
      {:ok, %{body: body, headers: resp_headers}} ->
        %{"id" => id} = Jason.decode!(body)
        cookie = get_cookie(resp_headers)
        {id, cookie}

      {:error, reason} ->
        IO.inspect(reason)
        :error
    end
  end

  defp get_cookie(headers) do
    {"Set-Cookie", cookie} = Enum.find(headers, fn {k, _} -> k == "Set-Cookie" end)
    String.split(cookie, ";") |> hd()
  end
end
