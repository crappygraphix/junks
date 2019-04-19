defmodule Junks.Repo.Migrations.CreateTags do
  use Ecto.Migration

  def change do
    execute "CREATE EXTENSION citext", "DROP EXTENSION citext"

    create table(:tags) do
      add :name, :citext, null: false
      add :group_id, references(:groups, on_delete: :delete_all)

      timestamps()
    end

    create unique_index(:tags, [:name, :group_id], name: :tag_name_group)
    create index(:tags, [:group_id])
  end
end
