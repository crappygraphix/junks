defmodule JunksWeb.Controller.Helpers do
  import Plug.Conn
  import Phoenix.Controller, only: [render: 4]

  def process_error(conn, {:error, %Ecto.Changeset{} = cs}) do
    conn
    |> put_status(:unprocessable_entity)
    |> render(JunksWeb.ErrorView, "changeset_error.json", changeset: cs)
  end
  def process_error(conn, {:error, err}) do
    conn
    |> put_status(:unprocessable_entity)
    |> render(JunksWeb.ErrorView, "custom_error.json", errors: [err])
  end
end
