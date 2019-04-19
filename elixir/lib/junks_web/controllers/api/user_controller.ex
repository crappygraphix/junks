defmodule JunksWeb.Api.UserController do
  use JunksWeb, :controller

  alias Junks.Auth
  alias Junks.Errors
  alias Junks.Mailer
  alias Junks.Util

  def register(conn, %{"name" => n, "email" => e, "password1" => pw1, "password2" => pw2}) do
    if pw1 == pw2 do
      case Auth.create_user(%{name: n, email: e, password: pw1}) do
        {:ok, _} -> Util.empty_json(conn)
        {:error, _, changeset, %{}} -> process_error(conn, {:error, changeset})
      end
    else
      process_error(conn, {:error, Errors.no_match})
    end
  end

  def login(conn, %{"email" => e, "password" => p}) do
    case Auth.authenticate(%{email: e, password: p}) do
      {:error, _} -> process_error(conn, {:error, Errors.bad_auth})
      {:ok, user, token} -> render(conn, "auth.json", user: user, token: token)
    end
  end

  def reauth(conn, _) do
    case Auth.reauthenticate(conn) do
      {:error, _} -> process_error(conn, {:error, Errors.bad_auth})
      {:ok, user, token} -> render(conn, "auth.json", user: user, token: token)
    end
  end

  @doc """
    Always returns empty json, even on failure.
    We don't want a way for attackers spam requests to see if a email is or is not in use.
  """
  def forgot(conn, %{"email" => e}) do
    case Auth.user_by_email(%{email: e}) do
        {:ok, user} ->
          case Auth.create_reset_token(%{user: user}) do
            {:ok, t} ->
              Mailer.forgot_password(user, t.token)
              Util.empty_json(conn)
            _ -> process_error(conn, {:error, Errors.server})
          end
        _ ->
          Util.empty_json(conn)
    end
  end

  @doc """
    TODO: Doc
  """
  def reset(conn, %{"reset_token" => t, "password1" => p1, "password2" => p2}) do
    with {:ok, user} <- Auth.user_by_reset_token(%{token: t}),
         {:ok, password} <- (if p1 == p2, do: {:ok, p1}, else: {:error, Errors.no_match})
         do Auth.change_password(%{user: user, password: password}) end |>
    case do
      {:ok, _} -> Util.empty_json(conn)
      err -> process_error(conn, err)
    end
  end
end
