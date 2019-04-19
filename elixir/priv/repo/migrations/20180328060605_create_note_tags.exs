defmodule Junks.Repo.Migrations.CreateNoteTags do
  use Ecto.Migration

  def change do
    create table(:note_tags) do
      add :note_id, references(:notes, on_delete: :nothing)
      add :tag_id, references(:tags, on_delete: :nothing)

      timestamps()
    end

    create unique_index(:note_tags, [:note_id, :tag_id], name: :note_tag)
    create index(:note_tags, [:note_id])
    create index(:note_tags, [:tag_id])
  end
end
