defmodule Junks.Auth.UserGroup do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.Group
  alias Junks.Auth.User
  alias Junks.Errors

  schema "user_groups" do
    belongs_to :user, User
    belongs_to :group, Group

    timestamps()
  end

  @doc false
  def cs_create(user_group, attrs = %{user_id: _, group_id: _}) do
    user_group
    |> cast(attrs, [:user_id, :group_id])
    |> validate_required([:user_id, :group_id], message: Errors.missing)
    |> unique_constraint(:user_group, name: :user_group, message: Errors.exists)
  end
end
