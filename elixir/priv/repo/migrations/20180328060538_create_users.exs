defmodule Junks.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string, null: false
      add :name, :string, null: false
      add :password, :text, null: false
      add :salt, :integer, null: false, default: 0

      timestamps()
    end

    create unique_index(:users, [:email])

    alter table(:groups) do
      add :user_id, references(:users, on_delete: :delete_all)
    end

    create unique_index(:groups, [:name, :user_id], name: :group_name_user)

    create index(:groups, [:user_id])
  end
end
