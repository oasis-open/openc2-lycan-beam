defmodule HaHaWeb.PageController do
  use HaHaWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
