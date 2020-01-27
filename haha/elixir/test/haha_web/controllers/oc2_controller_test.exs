defmodule HaHaWeb.Oc2ControllerTest do
  use HaHaWeb.ConnCase

  test "POST /openc2 alive", %{conn: conn} do
    params = get_json("query_features.json")
    conn = post conn, "/openc2", params
    assert json_response(conn, 200)
  end

  test "POST /openc2 empty features", %{conn: conn} do
    params = get_json("query_features.json")
    expected = get_json("query_features.reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 features versions", %{conn: conn} do
    params = get_json("query_features_versions.json")
    expected = get_json("query_features_versions.reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 features profiles", %{conn: conn} do
    params = get_json("query_features_profiles.json")
    expected = get_json("query_features_profiles_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 features pairs", %{conn: conn} do
    params = get_json("query_features_pairs.json")
    expected = get_json("query_features_pairs_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 features rate", %{conn: conn} do
    params = get_json("query_features_rate.json")
    expected = get_json("query_features_rate_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 features all", %{conn: conn} do
    params = get_json("query_features_all.json")
    expected = get_json("query_features_all_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 sbom", %{conn: conn} do
    params = get_json("query_sbom.json")
    expected = get_json("query_sbom_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 sbom diff order", %{conn: conn} do
    params = get_json("query_sbom2.json")
    ## note same reply since only cyclone supported so far
    expected = get_json("query_sbom_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 blinky", %{conn: conn} do
    params = get_json("query_blinky.json")
    expected = get_json("query_blinky_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set led on", %{conn: conn} do
    params = get_json("set_led_on.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set led off", %{conn: conn} do
    params = get_json("set_led_off.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set led_pattern flash1", %{conn: conn} do
    params = get_json("set_led_flash1.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set buzzer on", %{conn: conn} do
    params = get_json("set_buzz_on.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set valve on", %{conn: conn} do
    params = get_json("set_valve_on.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end

  test "POST /openc2 set spa key", %{conn: conn} do
    params = get_json("set_spa_key.json")
    expected = get_json("ok_reply.json")
    conn = post conn, "/openc2", params
    body = conn |> response(200) |> Jason.decode
    assert body == {:ok, expected}
  end




  #############################
  ## tests on bad input

  test "POST /openc2 no action", %{conn: conn} do
    params = get_json("err_no_action.json")
    conn = post conn, "/openc2", params
    assert response(conn, 422)
  end

  test "POST /openc2 bad action", %{conn: conn} do
    params = get_json("err_bad_action.json")
    conn = post conn, "/openc2", params
    assert response(conn, 422)
  end

  test "POST /openc2 no target", %{conn: conn} do
    params = get_json("err_no_target.json")
    conn = post conn, "/openc2", params
    assert response(conn, 422)
  end

  test "POST /openc2 empty target", %{conn: conn} do
    params = get_json("err_empty_target.json")
    conn = post conn, "/openc2", params
    assert response(conn, 501)
  end

  test "POST /openc2 2 targets", %{conn: conn} do
    params = get_json("err_2_targets.json")
    conn = post conn, "/openc2", params
    assert response(conn, 501)
  end

  test "POST /openc2 no sbom type", %{conn: conn} do
    params = get_json("err_no_sbom_format.json")
    conn = post conn, "/openc2", params
    assert response(conn, 422)
  end

  test "POST /openc2 set led bad", %{conn: conn} do
    params = get_json("err_set_led_bad.json")
    conn = post conn, "/openc2", params
    assert response(conn, 422)
  end




  #############################
  ## private helper functions
  defp get_json(filename) do
    filepath = "./test/data/" <> filename
    {:ok, query_text} = File.read(filepath)
    {:ok, results} = Jason.decode(query_text)
    results
  end



end
