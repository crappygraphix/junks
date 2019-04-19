defmodule Junks.ContentTest do
  use Junks.DataCase

  alias Junks.Auth
  alias Junks.Content

  def group_fixture(name, user_id) do
    {:ok, %{group: group, relation: _}} = Auth.create_group(%{name: name, user_id: user_id})
    group
  end

  def join_fixture(group_id, user_id, owner_id) do
    {:ok, relation} = Auth.join_group(%{group_id: group_id, user_id: user_id, invitor_id: owner_id})
    relation
  end

  def user_fixture(name, email, password) do
    {:ok, t} = Auth.create_user(%{name: name, email: email, password: password})
    t.user
  end

  def note_fixture(title, body, group_id, user_id) do
    {:ok, n} = Content.create_note(%{title: title, body: body, group_id: group_id, user_id: user_id})
    n
  end

  def tag_fixture(name, group_id, user_id) do
    {:ok, t} = Content.create_tag(%{name: name, group_id: group_id, user_id: user_id})
    t
  end

  describe "permissions test" do
    test "all" do
      tim = user_fixture("Tim", "john@doe.com", "ASDF1234")
      bri = user_fixture("Bri", "john2@doe.com", "ASDF1234")
      tim_group1 = group_fixture("Dummy Group", tim.id)
      tim_group2 = group_fixture("Dummy Group 2", tim.id)
      tim_note_group1 = note_fixture("Dummy Note", "Body", tim_group1.id, tim.id)
      join_fixture(tim_group2.id, bri.id, tim.id)

      tim_group1_tag = tag_fixture("Tag", tim_group1.id, tim.id)
      tim_group2_tag = tag_fixture("Tag 2", tim_group2.id, tim.id)

      assert :yes == Auth.owns_group(%{user_id: tim.id, group_id: tim_group1.id})
      assert :yes == Auth.owns_group(%{user_id: tim.id, group_id: tim_group2.id})
      assert :no == Auth.owns_group(%{user_id: bri.id, group_id: tim_group1.id})

      assert :yes == Auth.in_group(%{user_id: tim.id, group_id: tim_group1.id})
      assert :yes == Auth.in_group(%{user_id: tim.id, group_id: tim_group2.id})
      assert :no == Auth.in_group(%{user_id: bri.id, group_id: tim_group1.id})
      assert :yes == Auth.in_group(%{user_id: bri.id, group_id: tim_group2.id})

      assert :yes == Content.can_view(%{user_id: tim.id, note_id: tim_note_group1.id})
      assert :no == Content.can_view(%{user_id: bri.id, note_id: tim_note_group1.id})

      assert :yes == Content.owns_note(%{user_id: tim.id, note_id: tim_note_group1.id})
      assert :no == Content.owns_note(%{user_id: bri.id, note_id: tim_note_group1.id})

      assert :yes == Content.can_tag(%{note_id: tim_note_group1.id, tag_id: tim_group1_tag.id})
      assert :no == Content.can_tag(%{note_id: tim_note_group1.id, tag_id: tim_group2_tag.id})

      assert :yes == Content.owns_tag(%{user_id: tim.id, tag_id: tim_group1_tag.id})
      assert :yes == Content.owns_tag(%{user_id: tim.id, tag_id: tim_group2_tag.id})
      assert :no == Content.owns_tag(%{user_id: bri.id, tag_id: tim_group1_tag.id})
    end
  end

  describe "creation workflows" do
    test "create note" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      u2 = user_fixture("John Doe", "john1@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)

      # Create note
      {r, _n} = Content.create_note(%{title: "Title", body: "Body", group_id: g.id, user_id: u.id})
      assert r == :ok

      # Duplicate title
      {r, _n} = Content.create_note(%{title: "Title", body: "Body", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.create_note(%{title: nil, body: "Body", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.create_note(%{title: "", body: "Body", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.create_note(%{title: " ", body: "Body", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Body
      {r, _n} = Content.create_note(%{title: "Title 1", body: nil, group_id: g.id, user_id: u.id})
      assert r == :error

      # Empty Body
      {r, _n} = Content.create_note(%{title: "Title 2", body: "", group_id: g.id, user_id: u.id})
      assert r == :ok

      # Empty Body
      {r, _n} = Content.create_note(%{title: "Title 3", body: " ", group_id: g.id, user_id: u.id})
      assert r == :ok

      # Bad Group Id
      {r, _n} = Content.create_note(%{title: "Title 4", body: "Body", group_id: 0, user_id: u.id})
      assert r == :error

      # Bad User Id
      {r, _n} = Content.create_note(%{title: "Title", body: "Body", group_id: g.id, user_id: 0})
      assert r == :error

      # Bad User Permission
      {r, _n} = Content.create_note(%{title: "Title", body: "Body", group_id: g.id, user_id: u2.id})
      assert r == :error
    end

    test "create tag" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)

      # Create tag
      {r, _t} = Content.create_tag(%{name: "Tag", group_id: g.id, user_id: u.id})
      assert r == :ok

      # Duplicate title
      {r, _t} = Content.create_tag(%{name: "Tag", group_id: g.id, user_id: u.id})
      assert r == :error

      # Duplicate title
      {r, _t} = Content.create_tag(%{name: "tAg", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _t} = Content.create_tag(%{name: nil, group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _t} = Content.create_tag(%{name: "", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _t} = Content.create_tag(%{name: "   ", group_id: g.id, user_id: u.id})
      assert r == :error

      # Bad Group Id
      {r, _t} = Content.create_tag(%{name: "Tag 1", group_id: 0, user_id: u.id})
      assert r == :error

      # Bad User Id
      {r, _t} = Content.create_tag(%{name: "Tag 1", group_id: g.id, user_id: 0})
      assert r == :error
    end

    test "attach tag" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      u2 = user_fixture("John Doe 2", "john2@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)
      g2 = group_fixture("Dummy Group 2", u.id)
      n = note_fixture("Dummy Note", "Body", g.id, u.id)
      t = tag_fixture("Tag", g.id, u.id)
      t2 = tag_fixture("Tag 2", g2.id, u.id)

      # Create relation
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: t.id, user_id: u.id})
      assert r == :ok

      # Bad Note
      {r, _nt} = Content.attach_tag(%{note_id: 0, tag_id: t.id, user_id: u.id})
      assert r == :error

      # Bad Tag
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: 0, user_id: u.id})
      assert r == :error

      # Duplicate Tag
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: t.id, user_id: u.id})
      assert r == :error

      # Not user's note
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: t.id, user_id: u2.id})
      assert r == :error

      # Not tag's group
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: t2.id, user_id: u.id})
      assert r == :error
    end
  end

  describe "delete workflows" do
    test "detach tag" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      u2 = user_fixture("John Doe 2", "john2@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)
      n = note_fixture("Dummy Note", "Body", g.id, u.id)
      t = tag_fixture("Tag", g.id, u.id)

      # Attach tag
      {r, _nt} = Content.attach_tag(%{note_id: n.id, tag_id: t.id, user_id: u.id})
      assert r == :ok

      # Not user's note
      {r, _nt} = Content.detach_tag(%{note_id: n.id, tag_id: t.id, user_id: u2.id})
      assert r == :error

      # Bad Note
      {r, _nt} = Content.detach_tag(%{note_id: 0, tag_id: t.id, user_id: u.id})
      assert r == :error

      # Bad Tag
      {r, _nt} = Content.detach_tag(%{note_id: n.id, tag_id: 0, user_id: u.id})
      assert r == :error

      # Detach tag
      {r, _nt} = Content.detach_tag(%{note_id: n.id, tag_id: t.id, user_id: u.id})
      assert r == :ok
    end

    test "delete tag" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      u2 = user_fixture("John Doe 2", "john2@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)
      t = tag_fixture("Tag", g.id, u.id)

      # Not user's tag
      {r, _nt} = Content.delete_tag(%{tag_id: t.id, user_id: u2.id})
      assert r == :error

      # Delete tag
      {r, _nt} = Content.delete_tag(%{tag_id: t.id, user_id: u.id})
      assert r == :ok
    end

    test "delete note" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      u2 = user_fixture("John Doe 2", "john2@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)
      n = note_fixture("Dummy Note", "Body", g.id, u.id)

      # Not user's note
      {r, _nt} = Content.delete_note(%{note_id: n.id, user_id: u2.id})
      assert r == :error

      # Delete Note
      {r, _nt} = Content.delete_note(%{note_id: n.id, user_id: u.id})
      assert r == :ok
    end
  end

  describe "fetch workflows" do
    test "fetch groups with notes" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group 1", u.id)
      note_fixture("Dummy Note 1", "Body 1", g.id, u.id)
      note_fixture("Dummy Note 2", "Body 2", g.id, u.id)

      gs = Content.notes_by_group(%{user_id: u.id})
      assert 2 == length(gs)
    end

    test "fetch note" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group 1", u.id)
      n = note_fixture("Dummy Note 1", "Body 1", g.id, u.id)

      {r, _} = Content.get_note(%{note_id: n.id, user_id: u.id})
      assert r == :ok
    end
  end

  describe "update workflows" do
    test "update note" do
      u = user_fixture("John Doe", "john@doe.com", "ASDF1234")
      g = group_fixture("Dummy Group", u.id)

      # Create note
      {_, _} = Content.create_note(%{title: "Title 2", body: "Body", group_id: g.id, user_id: u.id})
      {r, n} = Content.create_note(%{title: "Title", body: "Body", group_id: g.id, user_id: u.id})
      assert r == :ok

      # Duplicate title
      {r, _n} = Content.update_note(%{note_id: n.id, title: "Title 2", body: "Body", user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.update_note(%{note_id: n.id, title: nil, body: "Body", user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.update_note(%{note_id: n.id, title: "", body: "Body", user_id: u.id})
      assert r == :error

      # Bad Title
      {r, _n} = Content.update_note(%{note_id: n.id, title: " ", body: "Body", user_id: u.id})
      assert r == :error

      # Bad Body
      {r, _n} = Content.update_note(%{note_id: n.id, title: "Title", body: nil, user_id: u.id})
      assert r == :error

      # Empty Body
      {r, _n} = Content.update_note(%{note_id: n.id, title: "Title", body: "", user_id: u.id})
      assert r == :ok

      # Empty Body
      {r, _n} = Content.update_note(%{note_id: n.id, title: "Title", body: " ", user_id: u.id})
      assert r == :ok
    end
  end
end
