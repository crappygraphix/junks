defmodule JunksWeb.Api.UpdateWorkflowTest do
  use JunksWeb.ConnCase

  def contains_prop_val(p, v) do
    fn (m, acc) ->
      if (acc) do
        acc
      else
        m[p] == v
      end
    end
  end

  def fetch_by_prop_val(p, v) do
    fn (m, acc) ->
      if m[p] == v do
        m
      else
        acc
      end
    end
  end

  describe "update workflows" do
    test "update note", %{conn: conn} do
      # Default registration
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _} = json_response(conn, 200)

      # Create group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})

      assert %{"id" => gid, "name" => "Pardonemwa"} = json_response(conn, 200)

      # Create Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes", %{title: "The Title", body: "I am a sexy bod.", group_id: to_string(gid)})

      assert %{"id" => nid, "title" => "The Title", "body" => "I am a sexy bod."} = json_response(conn, 200)

      # Update Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes/" <> to_string(nid), %{title: "The Title 2", body: "I am a sexy bod 2."})

      assert %{"id" => _, "title" => "The Title 2", "body" => "I am a sexy bod 2."} = json_response(conn, 200)
    end
  end
end
