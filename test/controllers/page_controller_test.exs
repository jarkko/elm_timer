defmodule ElmTimer.PageControllerTest do
  use ElmTimer.ConnCase

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "<div id=\"elm-main\">"
  end
end
