defmodule Junks.Repo.Migrations.CreateResetTokens do
  use Ecto.Migration

  def change do
    create table(:reset_tokens) do
      add :token, :string
      add :expires, :naive_datetime
      add :user_id, references(:users, on_delete: :delete_all)

      timestamps()
    end

    create index(:reset_tokens, [:user_id])
    create unique_index(:reset_tokens, [:user_id], name: :reset_tokens_user_id_unique)
  end
end
