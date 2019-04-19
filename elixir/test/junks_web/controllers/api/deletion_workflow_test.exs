defmodule JunksWeb.Api.NoteControllerTest do
  use JunksWeb.ConnCase

  alias Junks.Auth.Group
  alias Junks.Repo

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

  describe "delete workflows" do
    test "delete group", %{conn: conn} do
      # Default Registration
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      # Create group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})
      g = json_response(conn, 200)

      # Delete group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> delete("/api/groups/" <> to_string(g["id"]))

      assert conn.state == :sent
      assert conn.status == 204
      assert conn.resp_body == ""

      g = Repo.get(Group, g["id"])
      assert nil == g
    end

    test "delete tag", %{conn: conn} do
      # Default registration
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      # Create group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})
      g = json_response(conn, 200)

      # Add tag to group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups/" <> to_string(g["id"]) <> "/tags", %{name: "Dummy Tag"})
      t = json_response(conn, 200)

      # Delete tag from group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> delete("/api/groups/" <> to_string(g["id"]) <> "/tags/" <> to_string(t["id"]))

      assert %{"id" => _, "name" => "Dummy Tag"} = json_response(conn, 200)

      # Delete tag again from group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> delete("/api/groups/" <> to_string(g["id"]) <> "/tags/" <> to_string(t["id"]))

      assert %{"errors" => ["NOT_FOUND"]} = json_response(conn, 422)
    end

    test "detach tag", %{conn: conn} do
      # Default registration
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      # Create group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})
      g = json_response(conn, 200)

      # Add tag to group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups/" <> to_string(g["id"]) <> "/tags", %{name: "Dummy Tag"})

      assert %{"id" => tid, "name" => "Dummy Tag"} = json_response(conn, 200)

      # Create Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes", %{title: "The Title", body: "I am a sexy bod.", group_id: g["id"]})

      assert %{"id" => nid, "title" => "The Title", "body" => "I am a sexy bod."} = json_response(conn, 200)

      # Tag Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes/" <> to_string(nid) <> "/tags/" <> to_string(tid), %{})

      assert response(conn, 204)

      # Un-Tag Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> delete("/api/notes/" <> to_string(nid) <> "/tags/" <> to_string(tid), %{})

      assert response(conn, 204)
    end
  end
end
