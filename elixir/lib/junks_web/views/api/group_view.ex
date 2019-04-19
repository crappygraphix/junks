defmodule JunksWeb.Api.GroupView do
  use JunksWeb, :view

  def render("group.json", %{group: group}) do
    %{
      id: group.id,
      name: group.name
    }
  end

  def render("groups.json", %{groups: groups}) do
    Enum.map(groups, &group_json/1)
  end

  def render("tag.json", %{tag: tag}) do
    %{
      id: tag.id,
      name: tag.name
    }
  end

  def render("tags.json", %{tags: tags}) do
    Enum.map(tags, &tag_json/1)
  end

  def tag_json(tag) do
    %{
      id: tag.id,
      name: tag.name,
    }
  end

  def group_json(group) do
    %{
      id: group.id,
      name: group.name,
      notes: Enum.map(group.notes, &note_json/1)
    }
  end

  def note_json(note) do
    %{
      id: note.id,
      title: note.title
    }
  end
end
