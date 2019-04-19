defmodule Junks.Repo.Migrations.CreateNotes do
  use Ecto.Migration

  def change do
    create table(:notes) do
      add :title, :string, null: false
      add :body, :text, null: false, default: ""
      add :group_id, references(:groups, on_delete: :delete_all)
      add :user_id, references(:users, on_delete: :delete_all)

      timestamps()
    end

    create unique_index(:notes, [:title, :group_id], name: :note_title_group)
    create index(:notes, [:group_id])
    create index(:notes, [:user_id])
  end
end
