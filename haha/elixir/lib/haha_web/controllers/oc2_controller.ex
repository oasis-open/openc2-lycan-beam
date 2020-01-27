defmodule HaHaWeb.OC2Controller do
  require Logger
  use HaHaWeb, :controller

  def command(conn, params) do
    ## check top level components of command json
    tops = Map.keys(params)
    cond do
      ## is action missing?
      "action" not in tops ->
        ## :unprocessable_entity - 422
        Logger.debug "command no action #{inspect params}"
        send_resp(conn, :unprocessable_entity, "Oops! no action?")
      ## is target missing?
      "target" not in tops ->
        Logger.debug "command no target #{inspect params}"
        send_resp(conn, :unprocessable_entity, "Oops! no target?")
      ## extra top level fields
      0 != length(tops -- ["action", "target", "args", "actuator"]) ->
        Logger.debug "command top level error #{inspect params}"
        send_resp(conn, :unprocessable_entity, "Oops! top level fields?")
      true ->
        ## good so far
        do_action(conn, params)
    end
  end

  defp do_action(conn, params = %{"action" => "set"}) do
    Logger.debug "do_action set #{inspect params}"
    %{"target" => target} = params
    case check_one_map_key(target) do
      ## validate is a map and extract the one target and it's attributes
      {:ok, "x-sfractal-blinky:led", attr} ->
        Logger.debug "do_action set led #{inspect attr}"
        do_action_set_led(conn,attr)
      {:ok, "x-sfractal-blinky:led_pattern", attr} ->
        Logger.debug "do_action set led_pattern #{inspect attr}"
        Logger.debug "stubbing for now"
        json(conn, %{status: :ok})
      {:ok, "x-sfractal-blinky:buzzer", attr} ->
        Logger.debug "do_action set buzzer #{inspect attr}"
        Logger.debug "stubbing for now"
        json(conn, %{status: :ok})
      {:ok, "x-sfractal-blinky:valve", attr} ->
        Logger.debug "do_action set valve #{inspect attr}"
        Logger.debug "stubbing for now"
        json(conn, %{status: :ok})
      {:ok, "x-sfractal-blinky:spa_key", attr} ->
        Logger.debug "do_action set spa_key #{inspect attr}"
        Logger.debug "stubbing for now"
        json(conn, %{status: :ok})
      {:error, error_msg, _attr} ->
        Logger.debug "do_action set #{inspect error_msg}"
        send_resp(conn, :unprocessable_entity, "Oops! bad target")
      end
  end

  defp do_action(conn, params = %{"action" => "query"}) do
    Logger.debug "do_action query #{inspect params}"
    %{"target" => target} = params

    case check_one_map_key(target) do
      ## validate is a map and extract the one target and it's attributes
      {:ok, "sbom", sbom_attr} ->
        Logger.debug "do_action sbom"
        do_action_sbom(conn, sbom_attr)
      {:ok, "features", attr} ->
        Logger.debug "do_action features"
        do_action_features(conn, attr)
      {:ok, "x-sfractal-blinky:hello_world", _attr} ->
        Logger.debug "do_action hello"
        json(conn, "Hello World!")
      other ->
        Logger.debug "do_action error #{other}"
        ## :not_implemented = 501
        send_resp(conn, :not_implemented, "Oops! bad query target")
    end
  end

  defp do_action(conn, params = %{"action" => "allow"}) do
    Logger.debug "do_action allow #{inspect params}"
    ##json(conn, %{ok: "got to allow, need to finish coding"})
    ## :not_implemented = 501
    send_resp(conn, :not_implemented, "sorry: allow action not yet implemented")
  end

  defp do_action(conn, params = %{"action" => "cancel"}) do
    Logger.debug "do_action cancel #{inspect params}"
    ## :not_implemented = 501
    send_resp(conn, :not_implemented, "sorry: cancel action not yet implemented")
  end

  defp do_action(conn, params) do
    Logger.debug "do_action oops action #{inspect params}"
    ## unrecognized action
    %{"action" => whataction} = params
    ## :not_found - 404
    ## :unprocessable_entity - 422
    send_resp(conn, :unprocessable_entity, "Oops! action=#{whataction}?")
  end

  defp do_action_sbom(conn, sbom_attr) do
    Logger.debug "do_action_sbom"
    case check_one_map_key(sbom_attr) do
      ##check just one target attribute and it's type
      {:ok, "type", sbom_type_list} ->
        ## good so far
        Logger.debug "do_action sbom type #{inspect sbom_type_list}"
        case return_sbom(sbom_type_list) do
          {:ok, result} ->
            ## respond with answer
            json(conn, result)
          _ ->
            ## oops occurred on format
            Logger.debug "do_action - no matching"
            error_msg = "Oops! No matching sbom format"
            send_resp(conn, :unprocessable_entity, error_msg)
        end
      _ ->
        ## something wrong with type
        Logger.debug "do_action sbom type error"
        error_msg = "Oops! query sbom type semantics"
        send_resp(conn, :unprocessable_entity, error_msg)
    end
  end

  defp do_action_features(conn, []) do
    Logger.debug "do_action_features empty list"
    json(conn, %{status: :ok})
  end

  defp do_action_features(conn, features) when is_list(features) do
    Logger.debug "do_action_features list"
    output = %{status: :ok, results: %{}}
    case iterate_features(output,features) do
      {:ok, result} ->
        ## respond with answer
        json(conn, result)
      _ ->
        ## oops occurred on format
        Logger.debug "do_action_features error"
        error_msg = "Oops! error on features"
        send_resp(conn, :unprocessable_entity, error_msg)
    end
  end

  defp do_action_features(conn, attr) when is_list(attr) do
    Logger.debug "do_action_features"
    json(conn, %{ok: "got to query target=features, attr = #{attr}"})
  end

  defp do_action_features(conn, _attr) do
    Logger.debug "do_action_features not a list"
    error_msg = "Oops! query features not a list"
    send_resp(conn, :unprocessable_entity, error_msg)
  end

  defp do_action_set_led(conn,attr) when attr == "on" do
    Logger.debug "do_action_set_led on"
    set_led_on(conn)
  end

  defp do_action_set_led(conn,attr) when attr == "off" do
    Logger.debug "do_action_set_led off"
    set_led_off(conn)
  end

  defp do_action_set_led(conn,attr) do
    Logger.debug "do_action_set_led attr= #{attr} not recognized"
    send_resp(conn, :unprocessable_entity, "led state should be on or off")
  end


  defp check_one_map_key(in_map) do
    ## check it is a map and has one key.
    ## return error or the key and the value of the key
    Logger.debug "check_one_map_key #{inspect in_map}"
    case is_map(in_map) do
      ## check if a map
      true ->
        ## yes it is a map
        ## now check only one key
        in_map_keys = Map.keys(in_map)
        case length(in_map_keys) do
          1 ->
            ## one key as expected. return it and value
            [ one_key | _ ] = in_map_keys
            mapvalue = in_map[one_key]
            {:ok, one_key, mapvalue}
          _ ->
            ## wrong number keys, return error
            Logger.debug "check_one_map_key: wrong number keys in map"
            :error
        end
      false ->
        ## expecting a map and it is not therefore return error
        Logger.debug "check_one_map_key: not a map"
        :error
    end
  end

  defp return_sbom( [] ) do
    ## empty list - error since can't do any
    Logger.debug "return_sbom - empty list"
    :error
  end

  defp return_sbom([first | rest]) do
    ## do first on list that is doable
    case first do
      "cyclonedx" ->
        Logger.debug "return_sbom: CycloneDX"
        result = cyclonedx()
        {:ok, result}
      "spdx" ->
        Logger.debug "return_sbom: SPDX"
        ## don't know how yet so go to next
        return_sbom(rest)
      "swid" ->
        Logger.debug "return_sbom: SWID"
        ## don't know how yet so go to next
        return_sbom(rest)
      _ ->
        ## no matching format
        Logger.debug "return_sbom error: #{inspect first}??"
        {:error, "unknown format"}
    end
  end

  defp return_sbom(_) do
    ## sbom list format mismatch
    Logger.debug "return_sbom: sbom list format mismatch"
    {:error, "sbom format error"}
  end

  defp cyclonedx() do
    ## return sbom in cyclonedx format
    ## build result starting innermost at binary
    cyclonedx_bin = "PD94bWwgdmVyc2lvbj0nMS4wJz8+PGJvbSBzZXJpYWxOdW1iZXI9JzllMjUzZjkyLTRlMWMtNDk3ZS04Zjg3LTUwNzMwZDI0ZjE4YScgeG1sbnM9J2h0dHA6Ly9jeWNsb25lZHgub3JnL3NjaGVtYS9ib20vMS4xJz48Y29tcG9uZW50cz48Y29tcG9uZW50IHR5cGU9J2xpYnJhcnknPjxkZXNjcmlwdGlvbj5OZXJ2ZXMgU3lzdGVtIEJSIC0gQnVpbGRyb290IGJhc2VkIGJ1aWxkIHBsYXRmb3JtIGZvciBOZXJ2ZXMgU3lzdGVtczwvZGVzY3JpcHRpb24+PGhhc2hlcz48aGFzaCBhbGc9J1NIQS0yNTYnPmUzZmRhNmJjNDlmOGUzNjYyZDM3MzU1YWFkODhjMDgzOTI5NjU5N2MwYjZmNjY1M2QyMTk2N2RiMTg5MGIwMzg8L2hhc2g+PC9oYXNoZXM+PGxpY2Vuc2VzPjxsaWNlbnNlPjxpZD5BcGFjaGUtMi4wPC9pZD48L2xpY2Vuc2U+PGxpY2Vuc2U+PG5hbWU+R1BMdjI8L25hbWU+PC9saWNlbnNlPjwvbGljZW5zZXM+PG5hbWU+bmVydmVzX3N5c3RlbV9icjwvbmFtZT48cHVybD5wa2c6aGV4L25lcnZlc19zeXN0ZW1fYnJAMS45LjU8L3B1cmw+PHZlcnNpb24+MS45LjU8L3ZlcnNpb24+PC9jb21wb25lbnQ+PGNvbXBvbmVudCB0eXBlPSdsaWJyYXJ5Jz48ZGVzY3JpcHRpb24+TmVydmVzIC0gQ3JlYXRlIGZpcm13YXJlIGZvciBlbWJlZGRlZCBkZXZpY2VzIGxpa2UgUmFzcGJlcnJ5IFBpLCBCZWFnbGVCb25lIEJsYWNrLCBhbmQgbW9yZTwvZGVzY3JpcHRpb24+PGhhc2hlcz48aGFzaCBhbGc9J1NIQS0yNTYnPjA3MDc5MzQyZGIzYTAzZDE5Njk0MTE4YTkzZjIyMDM1OWZiZDk0YjZlMTc0Yjk4ZDFlYTI3MDlkYjllODFkYTk8L2hhc2g+PC9oYXNoZXM+PGxpY2Vuc2VzPjxsaWNlbnNlPjxpZD5BcGFjaGUtMi4wPC9pZD48L2xpY2Vuc2U+PC9saWNlbnNlcz48bmFtZT5uZXJ2ZXM8L25hbWU+PHB1cmw+cGtnOmhleC9uZXJ2ZXNAMS41LjE8L3B1cmw+PHZlcnNpb24+MS41LjE8L3ZlcnNpb24+PC9jb21wb25lbnQ+PGNvbXBvbmVudCB0eXBlPSdsaWJyYXJ5Jz48ZGVzY3JpcHRpb24+U29ja2V0IGhhbmRsaW5nIGxpYnJhcnkgZm9yIEVsaXhpcjwvZGVzY3JpcHRpb24+PGhhc2hlcz48aGFzaCBhbGc9J1NIQS0yNTYnPjk4YTJhYjIwY2UxN2Y5NWZiNTEyYzVjYWRkZGJhMzJiNTcyNzNlMGQyZGJhMmQyZTVmOTc2YzU5NjlkMGM2MzI8L2hhc2g+PC9oYXNoZXM+PGxpY2Vuc2VzPjxsaWNlbnNlPjxpZD5XVEZQTDwvaWQ+PC9saWNlbnNlPjwvbGljZW5zZXM+PG5hbWU+c29ja2V0PC9uYW1lPjxwdXJsPnBrZzpoZXgvc29ja2V0QDAuMy4xMzwvcHVybD48dmVyc2lvbj4wLjMuMTM8L3ZlcnNpb24+PC9jb21wb25lbnQ+PGNvbXBvbmVudCB0eXBlPSdsaWJyYXJ5Jz48ZGVzY3JpcHRpb24+UmVhZCBhbmQgd3JpdGUgdG8gVS1Cb290IGVudmlyb25tZW50IGJsb2NrczwvZGVzY3JpcHRpb24+PGhhc2hlcz48aGFzaCBhbGc9J1NIQS0yNTYnPmIwMWUzZWMwOTczZTk5NDczMjM0ZjI3ODM5ZTI5ZTYzYjViODFlYmE2YTEzNmExOGE3OGQwNDlkNDgxM2Q2YzU8L2hhc2g+PC9oYXNoZXM+PGxpY2Vuc2VzPjxsaWNlbnNlPjxpZD5BcGFjaGUtMi4wPC9pZD48L2xpY2Vuc2U+PC9saWNlbnNlcz48bmFtZT51Ym9vdF9lbnY8L25hbWU+PHB1cmw+cGtnOmhleC91Ym9vdF9lbnZAMC4xLjE8L3B1cmw+PHZlcnNpb24+MC4xLjE8L3ZlcnNpb24+PC9jb21wb25lbnQ+PGNvbXBvbmVudCB0eXBlPSdsaWJyYXJ5Jz48ZGVzY3JpcHRpb24+TmVydmVzIFRvb2xjaGFpbiBDVE5HIC0gVG9vbGNoYWluIFBsYXRmb3JtPC9kZXNjcmlwdGlvbj48aGFzaGVzPjxoYXNoIGFsZz0nU0hBLTI1Nic+NDUyZjg1ODljMWE1OGFjNzg3NDc3Y2FhYjIwYThjZmM2NjcxZTM0NTgzN2NjYzE5YmVlZmU0OWFlMzViYTk4MzwvaGFzaD48L2hhc2hlcz48bGljZW5zZXM+PGxpY2Vuc2U+PGlkPkFwYWNoZS0yLjA8L2lkPjwvbGljZW5zZT48L2xpY2Vuc2VzPjxuYW1lPm5lcnZlc190b29sY2hhaW5fY3RuZzwvbmFtZT48cHVybD5wa2c6aGV4L25lcnZlc190b29sY2hhaW5fY3RuZ0AxLjYuMDwvcHVybD48dmVyc2lvbj4xLjYuMDwvdmVyc2lvbj48L2NvbXBvbmVudD48Y29tcG9uZW50IHR5cGU9J2xpYnJhcnknPjxkZXNjcmlwdGlvbj5BIHJpbmcgYnVmZmVyIGJhY2tlbmQgZm9yIEVsaXhpciBMb2dnZXIgd2l0aCBJTyBzdHJlYW1pbmcuPC9kZXNjcmlwdGlvbj48aGFzaGVzPjxoYXNoIGFsZz0nU0hBLTI1Nic+YjFiYWRkYzI2OTA5OWIyYWZlMmVhM2E4N2I4ZTJiNzFlNTczMzFjMDAwMDAzOGFlNTUwOTAwNjhhYWM2NzlkYjwvaGFzaD48L2hhc2hlcz48bGljZW5zZXM+PGxpY2Vuc2U+PGlkPkFwYWNoZS0yLjA8L2lkPjwvbGljZW5zZT48L2xpY2Vuc2VzPjxuYW1lPnJpbmdfbG9nZ2VyPC9uYW1lPjxwdXJsPnBrZzpoZXgvcmluZ19sb2dnZXJAMC44LjA8L3B1cmw+PHZlcnNpb24+MC44LjA8L3ZlcnNpb24+PC9jb21wb25lbnQ+PGNvbXBvbmVudCB0eXBlPSdsaWJyYXJ5Jz48ZGVzY3JpcHRpb24+TmVydmVzIFN5c3RlbSBMaW50ZXIgLSBMaW50IE5lcnZlcyBTeXN0ZW0gRGVmY29uZmlncy48L2Rlc2NyaXB0aW9uPjxoYXNoZXM+PGhhc2ggYWxnPSdTSEEtMjU2Jz44NGUwZjYzYzhhYzE5NmIxNmI3NzYwOGJiZTdkZjY2ZGNmMzUyODQ1YzRlNGZiMzk0YmZmZDJiNTcyMDI1NDEzPC9oYXNoPjwvaGFzaGVzPjxsaWNlbnNlcz48bGljZW5zZT48aWQ+QXBhY2hlLTIuMDwvaWQ+PC9saWNlbnNlPjwvbGljZW5zZXM+PG5hbWU+bmVydmVzX3N5c3RlbV9saW50ZXI8L25hbWU+PHB1cmw+cGtnOmhleC9uZXJ2ZXNfc3lzdGVtX2xpbnRlckAwLjMuMDwvcHVybD48dmVyc2lvbj4wLjMuMDwvdmVyc2lvbj48L2NvbXBvbmVudD48Y29tcG9uZW50IHR5cGU9J2xpYnJhcnknPjxkZXNjcmlwdGlvbj5ETlMgbGlicmFyeSBmb3IgRWxpeGlyIHVzaW5nIGBpbmV0X2Ruc2AgbW9kdWxlLiIgbmFtZT0iQ295b3RlIFNlcnZpY2VzLCBJbmMuIiByZWdpZD0ibXljb3lvdGUuY29tIiByb2xlPSJkaXN0cmlidXRvciIvPiA8TGluayByZWw9ImxpY2Vuc2UiIGhyZWY9Ind3dy5nbnUub3JnL2xpY2Vuc2VzL2dwbC50eHQiLz4gPE1ldGEgYWN0aXZhdGlvblN0YXR1cz0idHJpYWwiIHByb2R1Y3Q9IlJvYWRydW5uZXIgRGV0ZWN0b3IiIGNvbGxvcXVpYWxWZXJzaW9uPSIyMDEzIiBlZGl0aW9uPSJjb3lvdGUiIHJldmlzaW9uPSJzcDEiLz4gPFBheWxvYWQ+IDxEaXJlY3Rvcnkgcm9vdD0iJXByb2dyYW1kYXRhJSIgbmFtZT0icnJkZXRlY3RvciI+IDxGaWxlIG5hbWU9InJyZGV0ZWN0b3IuZXhlIiBzaXplPSI1MzI3MTIiIFNIQTI1NjpoYXNoPSJhMzE0ZmMyZGM2NjNhZTdhNmI2YmM2Nzg3NTk0MDU3Mzk2ZTZiM2Y1NjljZDUwZmQ1ZGRiNGQxYmJhZmQyYjZhIi8+IDwvRGlyZWN0b3J5PiA8L1BheWxvYWQ+IDwvU29mdHdhcmVJZGVudGl0eT4"
    ## put binary in payload map
    payload = %{bin: cyclonedx_bin}
    ## build manifest dictionary of payload and mime
    manifest = %{mime_type: "application/cyclonedx+xml",
                 payload: payload}
    ## build sbom_results from manifest and descriptors
    sbom_results = %{type: "CycloneDX",
                    depth: "one-hop",
                    manifest: manifest}
    ## build results sbom_results
    results = %{sbom: sbom_results}
    ## build response from status and results
    return_map = %{status: 200,
                  results: results}
    return_map
  end

  defp iterate_features(output,[]) do
    ## done
    {:ok, output}
  end

  defp iterate_features(output,[head | tail]) do
    ## iterate thru feature list adding results
    old_results = output[:results]
    case head do
      "versions" ->
        Logger.debug "iterate_features - versions"
        ver = "1.0"
        new_results = Map.put(old_results, :versions, [ver])
        new_output = Map.replace!(output, :results, new_results)
        ## now iterate again
        iterate_features(new_output,tail)
      "profiles" ->
        Logger.debug "iterate_features - profiles"
        profileout = "Duncan needs to do profiles output"
        new_results = Map.put(old_results, :profiles, profileout)
        new_output = Map.replace!(output, :results, new_results)
        ## now iterate again
        iterate_features(new_output,tail)
      "pairs" ->
        Logger.debug "iterate_features - pairs"
        pairsout = %{
          query: [
            :features,
            :sbom,
            :"x-sfractal-blinky:hello_world"
            ],
          set: [
            :"x-sfractal-blinky:led",
            :"x-sfractal-blinky:led_pattern",
            :"x-sfractal-blinky:buzzer",
            :"x-sfractal-blinky:valve",
            :"x-sfractal-blinky:spa_key"
            ],
           allow: [
            :ipv4_net,
            :ipv6_net
            ],
           cancel: [:command_id]
        }
        new_results = Map.put(old_results, :pairs, pairsout)
        new_output = Map.replace!(output, :results, new_results)
        ## now iterate again
        iterate_features(new_output,tail)
      "rate_limit" ->
        rate_limit = 100000
        new_results = Map.put(old_results, :rate_limit, rate_limit)
        new_output = Map.replace!(output, :results, new_results)
        ## now iterate again
        iterate_features(new_output,tail)
      _ ->
        Logger.debug "iterate_features - unknown feature"
        {:error, "unknown feature"}
    end
  end

  defp set_led_on(conn) do
    Logger.debug("set_led_on - just returning ok for now")
    json(conn, %{status: :ok})
  end

  defp set_led_off(conn) do
    Logger.debug("set_led_off - just returning ok for now")
    json(conn, %{status: :ok})
  end




end
