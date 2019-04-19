defmodule JunksWeb.Api.UserControllerTest do
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

  describe "fetch workflows" do
    test "fetch group tags", %{conn: conn} do
      # Default registration
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      # Create group
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Pardonemwa"})
      g = json_response(conn, 200)

      # Add tags to group
      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups/" <> to_string(g["id"]) <> "/tags", %{name: "Dummy Tag 1"})
      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups/" <> to_string(g["id"]) <> "/tags", %{name: "Dummy Tag 2"})
      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups/" <> to_string(g["id"]) <> "/tags", %{name: "Dummy Tag 3"})

      # Fetch
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> get("/api/groups/" <> to_string(g["id"]) <> "/tags")

      l = json_response(conn, 200)
      assert true == Enum.reduce(l, false, contains_prop_val("name", "Dummy Tag 1"))
      assert true == Enum.reduce(l, false, contains_prop_val("name", "Dummy Tag 2"))
      assert true == Enum.reduce(l, false, contains_prop_val("name", "Dummy Tag 3"))
    end

    test "fetch grouped note names", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Dummy Group"})

      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> get("/api/groups")

      groups = json_response(conn, 200)

      assert length(groups) == 2

      assert true == Enum.reduce(groups, false, contains_prop_val("name", "Dummy Group"))
      assert true == Enum.reduce(groups, false, contains_prop_val("name", "John Doe's Junks"))

      g = Enum.reduce(groups, false, fetch_by_prop_val("name", "Dummy Group"))
      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes", %{title: "Dummy Note 1", body: "Message.", group_id: to_string(g["id"])})
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> get("/api/groups")

      groups = json_response(conn, 200)
      g = Enum.reduce(groups, false, fetch_by_prop_val("name", "Dummy Group"))
      assert true == Enum.reduce(g["notes"], false, contains_prop_val("title", "Dummy Note 1"))
    end

    test "fetch note", %{conn: conn} do
      # Bolerplate Register and Log In
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      # Boilerplate Create Group
      build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/groups", %{name: "Dummy Group"})
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> get("/api/groups")

      groups = json_response(conn, 200)

      g = Enum.reduce(groups, false, fetch_by_prop_val("name", "Dummy Group"))
      # Create Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/notes", %{title: "Dummy Note 1", body: "Message.", group_id: to_string(g["id"])})
      n = json_response(conn, 200)

      # Fetch Note
      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> get("/api/notes/" <> to_string(n["id"]))

      assert %{"body" => "Message.", "group_id" => _, "id" => _, "title" => "Dummy Note 1"} = json_response(conn, 200)
    end
  end
end
