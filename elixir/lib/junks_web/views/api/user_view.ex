defmodule JunksWeb.Api.UserView do
  use JunksWeb, :view

  def render("auth.json", %{user: user, token: token}) do
    %{
      user: %{
        id: user.id,
        name: user.name,
        email: user.email
      },
      token: to_string(user.id) <> "::" <> token
    }
  end
end
