defmodule Junks.Content.Note do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.Group
  alias Junks.Auth.User
  alias Junks.Errors
  alias Junks.Util

  schema "notes" do
    field :body, :string, default: ""
    field :title, :string
    belongs_to :group, Group
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def cs_create(note, attrs = %{title: _title, body: _body, group_id: _group_id, user_id: _user_id}) do
    note
    |> cast(attrs, [:title, :body, :group_id, :user_id])
    |> validate_required([:title, :group_id, :user_id], message: Errors.missing)
    |> Util.validate_not_nil([:body])
    |> update_change(:title, &String.trim/1)
    |> validate_length(:title, min: 1, message: Errors.short)
    |> foreign_key_constraint(:group_id, message: Errors.no_fk)
    |> foreign_key_constraint(:user_id, message: Errors.no_fk)
    |> unique_constraint(:title, name: :note_title_group, message: Errors.exists)
  end

  @doc false
  def cs_update(note, attrs = %{title: _title, body: _body}) do
    note
    |> cast(attrs, [:title, :body])
    |> validate_required([:title], message: Errors.missing)
    |> Util.validate_not_nil([:body])
    |> update_change(:title, &String.trim/1)
    |> validate_length(:title, min: 1, message: Errors.short)
    |> unique_constraint(:title, name: :note_title_group, message: Errors.exists)
  end
end
