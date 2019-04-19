defmodule Junks.Content.NoteTag do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Errors
  alias Junks.Content.Note
  alias Junks.Content.Tag

  schema "note_tags" do
    belongs_to :note, Note
    belongs_to :tag, Tag

    timestamps()
  end

  @doc false
  def cs_create(note_tag, attrs = %{tag_id: _, note_id: _}) do
    note_tag
    |> cast(attrs, [:tag_id, :note_id])
    |> validate_required([:tag_id, :note_id], message: Errors.missing)
    |> foreign_key_constraint(:note_id, message: Errors.no_fk)
    |> foreign_key_constraint(:tag_id, message: Errors.no_fk)
    |> unique_constraint(:relation, name: :note_tag, message: Errors.exists)
  end
end
