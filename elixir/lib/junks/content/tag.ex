defmodule Junks.Content.Tag do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.Group
  alias Junks.Errors

  schema "tags" do
    field :name, :string
    belongs_to :group, Group

    timestamps()
  end

  @doc false
  def cs_create(tag, attrs = %{name: _, group_id: _}) do
    tag
    |> cast(attrs, [:name, :group_id])
    |> validate_required([:name, :group_id], message: Errors.missing)
    |> update_change(:name, &String.trim/1)
    |> validate_length(:name, min: 1, message: Errors.empty)
    |> foreign_key_constraint(:group_id, message: Errors.no_fk)
    |> unique_constraint(:name, name: :tag_name_group, message: Errors.exists)
  end
end
