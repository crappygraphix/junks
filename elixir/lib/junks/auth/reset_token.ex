defmodule Junks.Auth.ResetToken do
  use Ecto.Schema
  import Ecto.Changeset

  alias Junks.Auth.User
  alias Junks.Errors

  schema "reset_tokens" do
    field :expires, :naive_datetime
    field :token, :string
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def cs_create(reset_token, attrs = %{token: _, expires: _, user_id: _}) do
    reset_token
    |> cast(attrs, [:token, :expires, :user_id])
    |> validate_required([:token, :expires, :user_id], message: Errors.missing)
    |> unique_constraint(:user_id, message: Errors.exists)
  end
end
