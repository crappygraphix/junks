defmodule JunksWeb.Router.AlterSecureHeaders do
  alias Plug.Conn

  def init(default), do: default

  def call(conn, _opts) do
    Conn.delete_resp_header(conn, "x-content-type-options")
  end
end

defmodule JunksWeb.Router do
  @moduledoc false

  use JunksWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug JunksWeb.Router.AlterSecureHeaders
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", JunksWeb do
    pipe_through :api

    # No Auth
    post "/user/login", Api.UserController, :login
    post "/user/register", Api.UserController, :register
    post "/user/forgot", Api.UserController, :forgot
    post "/user/reset", Api.UserController, :reset
    post "/user/reauth", Api.UserController, :reauth
    # With Auth
    get    "/groups", Api.GroupController, :groups
    post   "/groups", Api.GroupController, :new
    delete "/groups/:group_id", Api.GroupController, :delete
    delete "/groups/:group_id/tags/:tag_id", Api.GroupController, :delete_tag
    post   "/groups/:group_id/tags", Api.GroupController, :new_tag
    get    "/groups/:group_id/tags", Api.GroupController, :tags

    get    "/notes/:note_id", Api.NoteController, :note
    post   "/notes/:note_id", Api.NoteController, :edit_note
    post   "/notes", Api.NoteController, :new_note
    post   "/notes/:note_id/tags/:tag_id", Api.NoteController, :attach_tag
    delete "/notes/:note_id/tags/:tag_id", Api.NoteController, :detach_tag
  end

  scope "/", JunksWeb do
    pipe_through :browser

    get "/rd/*any", PageController, :reflex_dom
    get "/rw/*any", PageController, :react
    get "/api/*any", PageController, :api
    get "/*any", PageController, :index
  end
end
