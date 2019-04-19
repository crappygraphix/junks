defmodule JunksWeb.Api.NoteController do
  use JunksWeb, :controller

  alias Junks.Auth
  alias Junks.Content
  alias Junks.Util

  @doc """
    new_note(conn, %{"group_id" => group_id, "title" => title, "body" => body})
  """
  def new_note(conn, %{"group_id" => group_id, "title" => title, "body" => body}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.create_note(%{title: title, body: body, group_id: group_id, user_id: user.id}) do
          {:ok, note} -> render(conn, "note.json", note: note)
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    edit_note(conn, %{"title" => title, "body" => body})
  """
  def edit_note(conn, %{"note_id" => note_id, "title" => title, "body" => body}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.update_note(%{note_id: note_id, title: title, body: body, user_id: user.id}) do
          {:ok, note} -> render(conn, "note.json", note: note)
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    note(conn, %{"note_id" => note_id})
  """
  def note(conn, %{"note_id" => note_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.get_note(%{user_id: user.id, note_id: note_id}) do
          {:ok, note} -> render(conn, "note.json", note: note)
          err -> process_error(conn, err)
        end
      err -> process_error(conn, err)
    end
  end

  @doc """
    attach_tag(conn, %{"note_id" => note_id, "tag_id" => tag_id})
  """
  def attach_tag(conn, %{"note_id" => note_id, "tag_id" => tag_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.attach_tag(%{user_id: user.id, note_id: note_id, tag_id: tag_id}) do
          {:ok, _} -> Util.empty_json(conn)
          err -> process_error(conn, err)
        end
      err -> process_error(conn, err)
    end
  end

  @doc """
    detach_tag(conn, %{"note_id" => note_id, "tag_id" => tag_id})
  """
  def detach_tag(conn, %{"note_id" => note_id, "tag_id" => tag_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.detach_tag(%{user_id: user.id, note_id: note_id, tag_id: tag_id}) do
          {:ok, _} ->  Util.empty_json(conn)
          err -> process_error(conn, err)
        end
      err -> process_error(conn, err)
    end
  end
end
