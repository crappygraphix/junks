defmodule JunksWeb.Api.AuthWorkflowTest do
  use JunksWeb.ConnCase

  alias Junks.Auth
  alias Junks.Auth.User
  alias Junks.Auth.ResetToken
  alias Junks.Errors
  alias Junks.Repo
  alias Junks.Util

  describe "unauthed workflows" do
    test "register user", %{conn: conn} do

      # All Bad
      conn = post conn, "/api/user/register", %{name: "", email: "", password1: "", password2: ""}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "EMAIL_MISSING")
      assert Enum.member?(resp, "NAME_MISSING")
      assert Enum.member?(resp, "PASSWORD_IN_MISSING")
      assert length(resp) == 3

      # Bad Name
      conn = post conn, "/api/user/register", %{name: "", email: "poops@com", password1: "asdf", password2: "asdf"}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "NAME_MISSING")
      assert length(resp) == 1

      # Bad Email
      conn = post conn, "/api/user/register", %{name: "Name", email: "", password1: "asdf", password2: "asdf"}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "EMAIL_MISSING")
      assert length(resp) == 1

      conn = post conn, "/api/user/register", %{name: "Name", email: "nope", password1: "asdf", password2: "asdf"}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "EMAIL_INVALID")
      assert length(resp) == 1

      # Bad Password
      conn = post conn, "/api/user/register", %{name: "Name", email: "hello@nurse", password1: "", password2: ""}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "PASSWORD_IN_MISSING")
      assert length(resp) == 1

      conn = post conn, "/api/user/register", %{name: "Name", email: "hello@nurse", password1: "asdf", password2: ""}
      %{ "errors" => resp } = json_response(conn, 422)
      assert Enum.member?(resp, "PASSWORD_MISMATCH")
      assert length(resp) == 1

      # Success
      conn = post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      assert conn.status == 204
      assert conn.resp_body == ""

      # Check user exists
      user = Repo.get_by(User, email: "test@test.com")
      assert user != nil

      # Check default group exists
      gs = Auth.user_groups(%{user_id: user.id})
      assert 1 == length gs
      assert "John Doe's Junks" == List.first(gs).name
    end

    test "reauthenticate user", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      user = Repo.get_by(User, email: "test@test.com")

      # Log in
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      assert conn.status == 200
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      assert {:ok, user} == Auth.check_token(%{user: user, token: Util.strip_prefix(to_string(user.id) <> "::", token)})

      conn = build_conn()
        |> put_req_header("authorization", "Bearer " <> token)
        |> post("/api/user/reauth")
      assert conn.status == 200
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      assert {:ok, user} == Auth.check_token(%{user: user, token: Util.strip_prefix(to_string(user.id) <> "::", token)})
    end

    test "authenticate user", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      user = Repo.get_by(User, email: "test@test.com")

      # Log in
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "asdf"}
      assert conn.status == 200
      %{"token" => token, "user" => _user} = json_response(conn, 200)

      assert {:ok, user} == Auth.check_token(%{user: user, token: Util.strip_prefix(to_string(user.id) <> "::", token)})
    end

    test "forgot password", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      post conn, "/api/user/register", %{name: "John Doe 1", email: "test1@test.com", password1: "asdf", password2: "asdf"}
      post conn, "/api/user/register", %{name: "John Doe 2", email: "test2@test.com", password1: "asdf", password2: "asdf"}
      user = Repo.get_by(User, email: "test@test.com")
      user1 = Repo.get_by(User, email: "test1@test.com")
      user2 = Repo.get_by(User, email: "test2@test.com")

      # Request Recovery
      conn = post conn, "/api/user/forgot", %{email: user.email}
      assert conn.status == 204
      assert conn.resp_body == ""

      token = Repo.get_by(ResetToken, user_id: user.id)
      assert token != nil

      # Multi Requests
      conn = post conn, "/api/user/forgot", %{email: user1.email}
      assert conn.status == 204
      assert conn.resp_body == ""

      conn = post conn, "/api/user/forgot", %{email: user2.email}
      assert conn.status == 204
      assert conn.resp_body == ""

      token1 = Repo.get_by(ResetToken, user_id: user1.id)
      assert token1 != nil
      token2 = Repo.get_by(ResetToken, user_id: user2.id)
      assert token2 != nil

      assert token1.token != token2.token
    end

    test "reset password", %{conn: conn} do
      post conn, "/api/user/register", %{name: "John Doe", email: "test@test.com", password1: "asdf", password2: "asdf"}
      user = Repo.get_by(User, email: "test@test.com")

      # Request Recovery
      conn = post conn, "/api/user/forgot", %{email: user.email}
      assert conn.status == 204
      assert conn.resp_body == ""

      token = Repo.get_by(ResetToken, user_id: user.id)
      assert token != nil

      # Empty Password
      conn = post conn, "/api/user/reset", %{reset_token: token.token, password1: "", password2: ""}
      %{"errors" => ["PASSWORD_IN_MISSING"]} = json_response(conn, 422)

      # Reset Password Mismatch
      conn = post conn, "/api/user/reset", %{reset_token: token.token, password1: "fdsa", password2: "asdf"}
      %{"errors" => ["PASSWORD_MISMATCH"]} = json_response(conn, 422)

      # Reset Password Bad Token
      conn = post conn, "/api/user/reset", %{reset_token: "", password1: "fdsa", password2: "fdsa"}
      %{"errors" => ["BAD_TOKEN"]} = json_response(conn, 422)

      # Reset Password
      conn = post conn, "/api/user/reset", %{reset_token: token.token, password1: "fdsa", password2: "fdsa"}
      assert conn.status == 204

      # Log in
      conn = post conn, "/api/user/login", %{email: "test@test.com", password: "fdsa"}
      assert conn.status == 200
      %{"token" => token, "user" => _} = json_response(conn, 200)
      {:ok, user_new_seed} = Auth.user_by_email(%{email: "test@test.com"})

      # Check bad seed
      assert {:error, Errors.bad_token} == Auth.check_token(%{user: user, token: Util.strip_prefix(to_string(user.id) <> "::", token)})
      # Check good seed
      assert {:ok, user_new_seed} == Auth.check_token(%{user: user_new_seed, token: Util.strip_prefix(to_string(user_new_seed.id) <> "::", token)})
    end
  end
end
