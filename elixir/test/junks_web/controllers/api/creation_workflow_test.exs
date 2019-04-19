defmodule JunksWeb.Api.CreationWorkflowTest do
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

  describe "create workflows" do
    test "create group", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})

      assert %{"id" => _, "name" => "Pardonemwa"} = json_response(conn, 200)
    end

    test "create tag", %{conn: conn} do
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

      assert %{"id" => _, "name" => "Dummy Tag"} = json_response(conn, 200)
    end

    test "create note", %{conn: conn} do
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

      assert %{"id" => _, "title" => "The Title", "body" => "I am a sexy bod."} = json_response(conn, 200)
    end

    test "attach tag", %{conn: conn} do
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
    end
  end
end
