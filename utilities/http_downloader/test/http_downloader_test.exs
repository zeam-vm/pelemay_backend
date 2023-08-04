defmodule HttpDownloaderTest do
  use ExUnit.Case
  doctest HttpDownloader

  setup do
    url = "http://example.com"
    ok_mock = fn request -> {request, Req.Response.new(status: 200, body: "😊")} end
    bad_mock = fn request -> {request, Req.Response.new(status: 400, body: "😡")} end

    %{url: url, mock: %{ok: ok_mock, bad: bad_mock}}
  end

  describe "download/2" do
    test "returns ok tuple when successful", %{url: url, mock: mock} do
      assert {:ok, "😊"} = HttpDownloader.download(url, adapter: mock.ok)
    end

    test "returns error tuple with bad request", %{url: url, mock: mock} do
      assert {:error, "😡"} = HttpDownloader.download(url, adapter: mock.bad)
    end
  end

  describe "download!/2" do
    test "returns data when successful", %{url: url, mock: mock} do
      assert "😊" = HttpDownloader.download!(url, adapter: mock.ok)
    end

    test "raises with bad request", %{url: url, mock: mock} do
      assert_raise RuntimeError, "😡", fn ->
        HttpDownloader.download!(url, adapter: mock.bad)
      end
    end
  end

  describe "basename_from_uri/1" do
    test "returns basename when string url is provided" do
      assert "3.dat" = HttpDownloader.basename_from_uri("http://example.com/1/2/3.dat")
    end

    test "returns basename when struct url is provided" do
      uri = URI.new!("http://example.com/1/2/3.dat")
      assert "3.dat" = HttpDownloader.basename_from_uri(uri)
    end
  end
end
