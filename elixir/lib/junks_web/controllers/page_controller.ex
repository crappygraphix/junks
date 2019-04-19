defmodule JunksWeb.PageController do
  use JunksWeb, :controller

  def index(conn, params) do
    case Application.get_env(:junks, :frontend) do
      "reflex-dom" -> reflex_dom(conn, params)
      "react" -> react(conn, params)
    end
  end

  def reflex_dom(conn, _params) do
    render conn, "reflex_dom.html"
  end

  def react(conn, _params) do
    render conn, "react.html"
  end

  def api(conn, _params) do
    render conn, "api.html"
  end
end
