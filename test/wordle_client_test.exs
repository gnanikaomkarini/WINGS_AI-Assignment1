defmodule WordleClientTest do
  use ExUnit.Case
  doctest WordleClient

  test "greets the world" do
    assert WordleClient.hello() == :world
  end
end
