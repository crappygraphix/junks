defmodule Junks.Repo.Migrations.CreateUserGroups do
  use Ecto.Migration

  def change do
    create table(:user_groups) do
      add :user_id, references(:users, on_delete: :delete_all)
      add :group_id, references(:groups, on_delete: :delete_all)

      timestamps()
    end

    create unique_index(:user_groups, [:user_id, :group_id], name: :user_group)
    create index(:user_groups, [:user_id])
    create index(:user_groups, [:group_id])
  end
end
