defmodule JunksWeb.Api.NoteView do
  use JunksWeb, :view

  def render("note.json", %{note: note}) do
    %{
      id: note.id,
      group_id: note.group_id,
      title: note.title,
      body: note.body
    }
  end

  def render("tag.json", %{tag: tag}) do
    %{
      id: tag.id,
      name: tag.name
    }
  end
end
