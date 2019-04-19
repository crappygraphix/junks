defmodule Junks.AuthTest do
  use Junks.DataCase

  alias Junks.Auth
  alias Junks.Errors

  def user_fixture(name, email, password) do
    {:ok, t} = Auth.create_user(%{name: name, email: email, password: password})
    t.user
  end

  def group_fixture(name, user_id) do
    {:ok, group} = Auth.create_group(%{name: name, user_id: user_id})
    group
  end

  def contains_name(n) do
    fn (g, acc) ->
      if (acc) do
        acc
      else
        g.name == n
      end
    end
  end

  describe "fetching" do
    test "fetch user by email" do
      user_fixture("John Doe", "test@test.com", "ABCD")
      # Fetch Failed
      {r, _error} = Auth.user_by_email(%{email: "test@test1.com"})
      assert r == :error

      # Fetch Success
      {r, _user} = Auth.user_by_email(%{email: "test@test.com"})
      assert r == :ok
    end
  end

  describe "authentication workflows" do
    test "authenticate" do
      user_fixture("John Doe", "test@test.com", "ABCD")

      # Check failed authentication.
      r = Auth.authenticate(%{email: "test@test.com", password: ""})
      assert r == {:error, "BAD_AUTH"}

      r = Auth.authenticate(%{email: "", password: "ABCD"})
      assert r == {:error, "BAD_AUTH"}

      # Check successful authentication.
      {:ok, user, token} = Auth.authenticate(%{email: "test@test.com", password: "ABCD"})
      assert {:ok, user} == Auth.check_token(%{user: user, token: token})
    end
  end

  describe "creation workflows" do
    test "create user" do
      # Create user
      {r, t} = Auth.create_user(%{name: "John Doe", email: "john@doe.com", password: "ASDF1234"})
      assert r == :ok

      # Check Default Group
      gs = Auth.user_groups(%{user_id: t.user.id})
      assert 1 == length gs
      assert "John Doe's Junks" == List.first(gs).name

      # Duplicate Email
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: "john@doe.com", password: "ASDF1234"})
      assert r == :error

      # Bad Email
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: "johndoe.com", password: "ASDF1234"})
      assert r == :error

      # Bad Email
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: nil, password: "ASDF1234"})
      assert r == :error

      # Bad Email
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: "", password: "ASDF1234"})
      assert r == :error

      # Bad Email
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: " ", password: "ASDF1234"})
      assert r == :error

      # Bad Name
      {r, _o, _v, _c} = Auth.create_user(%{name: nil, email: "john@doe.com", password: "ASDF1234"})
      assert r == :error

      # Bad Name
      {r, _o, _v, _c} = Auth.create_user(%{name: "", email: "john@doe.com", password: "ASDF1234"})
      assert r == :error

      # Bad Name
      {r, _o, _v, _c} = Auth.create_user(%{name: " ", email: "john@doe.com", password: "ASDF1234"})
      assert r == :error

      # Bad Password
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: "john@doe.com", password: nil})
      assert r == :error

      # Bad Name
      {r, _o, _v, _c} = Auth.create_user(%{name: "John Doe", email: "john@doe.com", password: ""})
      assert r == :error
    end

    test "create group" do
      # Make a user
      u = user_fixture("Dummy", "blah@smeh.com", "ASDF1234")
      # Create group
      {r, _} = Auth.create_group(%{name: "Super Group", user_id: u.id})
      assert r == :ok

      # Check Group
      gs = Auth.user_groups(%{user_id: u.id})
      assert 2 == length gs
      assert true == Enum.reduce(gs, false, contains_name("Super Group"))

      # Duplicate Group
      {r, _, _, _} = Auth.create_group(%{name: "Super Group", user_id: u.id})
      assert r == :error

      # Case doesn't matter
      {r, _,} = Auth.create_group(%{name: "super group", user_id: u.id})
      assert r == :ok

      # Null name
      {r, _, _, _} = Auth.create_group(%{name: nil, user_id: u.id})
      assert r == :error

      # Empty Name
      {r, _, _, _} = Auth.create_group(%{name: "", user_id: u.id})
      assert r == :error

      # Empty Name
      {r, _, _, _} = Auth.create_group(%{name: " ", user_id: u.id})
      assert r == :error
    end
  end

  describe "delete workflows" do
    test "delete group" do
      # Make a user
      u = user_fixture("Dummy", "blah@smeh.com", "ASDF1234")
      # Create group
      {r, %{group: g, relation: _}} = Auth.create_group(%{name: "Super Group", user_id: u.id})
      assert r == :ok
      # Delete Group
      {:ok, gd} = Auth.delete_group(%{user_id: u.id, group_id: g.id})
      assert gd.id == g.id
    end
  end

  describe "password recovery workflows" do
    test "create reset token" do
      # Make a user
      u = user_fixture("Dummy", "blah@smeh.com", "ASDF1234")

      # Request a token
      {r, t1} = Auth.create_reset_token(%{user: u})
      assert r == :ok

      # Request a token again
      {r, t2} = Auth.create_reset_token(%{user: u})
      assert r == :ok

      # Tokens should be different
      assert t1.token != t2.token
    end
  end

  test "verify reset token" do
    # Make a user
    u = user_fixture("Dummy", "blah@smeh.com", "ASDF1234")

    # Request a token
    {r, t1} = Auth.create_reset_token(%{user: u})
    assert r == :ok

    # Verify valid the token
    {r, u} = Auth.user_by_reset_token(%{token: t1.token})
    assert r == :ok
    assert u.email == "blah@smeh.com"

    # Invalid token
    {r, e} = Auth.user_by_reset_token(%{token: "ABC"})
    assert r == :error
    assert e == Errors.bad_token
  end

  test "change password" do
    # Make a user
    u = user_fixture("Dummy", "blah@smeh.com", "ASDF1234")

    # Empty password
    {r, _e} = Auth.change_password(%{user: u, password: ""})
    assert r == :error

    # Change their password
    {r, _e} = Auth.change_password(%{user: u, password: "1234ASDF"})
    assert r == :ok
    {r, u, t} = Auth.authenticate(%{email: "blah@smeh.com", password: "1234ASDF"})
    assert r == :ok
    assert {:ok, u} == Auth.check_token(%{user: u, token: t})
  end
end
