defmodule Junks.Auth.Group do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.User
  alias Junks.Content.Note
  alias Junks.Content.Tag
  alias Junks.Errors

  schema "groups" do
    field :name, :string

    many_to_many :users, User, join_through: "user_groups"
    has_many :notes, Note
    has_many :tags, Tag
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def cs_create(group, attrs = %{name: _, user_id: _}) do
    group
    |> cast(attrs, [:name, :user_id])
    |> validate_required([:name, :user_id], message: Errors.missing)
    |> update_change(:name, &String.trim/1)
    |> validate_length(:name, min: 1, message: Errors.empty)
    |> unique_constraint(:group_name_user, name: :group_name_user, message: Errors.exists)
  end
end
