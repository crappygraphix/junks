defmodule Junks.Auth.User do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.Group
  alias Junks.Errors

  schema "users" do
    field :email, :string
    field :name, :string
    field :password, :string
    field :salt, :integer, default: 0

    many_to_many :groups, Group, join_through: "user_groups"

    field :password_in, :string, virtual: true

    timestamps()
  end

  @doc false
  def cs_create(user, attrs = %{email: _, name: _, password_in: _}) do
    user
    |> cast(attrs, [:email, :name, :password_in])
    |> validate_required([:email, :name, :password_in], message: Errors.missing)
    |> update_change(:name, &String.trim/1)
    |> validate_length(:name, min: 1, message: Errors.short)
    |> update_change(:email, &String.trim/1)
    |> update_change(:email, &String.downcase/1)
    |> validate_format(:email, ~r/@/, message: Errors.invalid)
    |> validate_length(:password_in, min: 1, message: Errors.empty)
    |> unique_constraint(:email, message: Errors.exists)
    |> hash_password
    |> put_change(:salt, 1)
  end

  @doc false
  def cs_password(user, attrs) do
    user
    |> cast(attrs, [:password_in])
    |> validate_required([:password_in], message: Errors.missing)
    |> validate_length(:password_in, min: 1, message: Errors.empty)
    |> hash_password
    |> put_change(:salt, user.salt + 1)
  end

  @doc false
  defp hash_password(cs) do
    case cs do
      %Ecto.Changeset{valid?: true, changes: %{password_in: password_in}} ->
        put_change(cs, :password, Comeonin.Bcrypt.hashpwsalt(password_in))
      _ -> cs
    end
  end
end
