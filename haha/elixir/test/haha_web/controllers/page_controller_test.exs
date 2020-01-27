defmodule HaHaWeb.PageControllerTest do
  use HaHaWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "Welcome to HaHa using Phoenix!"
  end
end
