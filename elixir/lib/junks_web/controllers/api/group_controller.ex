defmodule JunksWeb.Api.GroupController do
  use JunksWeb, :controller

  alias Junks.Auth
  alias Junks.Content
  alias Junks.Util
  alias Junks.Errors

  @doc """
    groups(conn, _)
  """
  def groups(conn, _) do
    case Auth.check_conn_token(conn) do
      {:ok, user} -> render(conn, "groups.json", groups: Content.notes_by_group(%{user_id: user.id}))
      err -> process_error(conn, err)
    end
  end

  @doc """
    new(conn, %{"name" => name})
  """
  def new(conn, %{"name" => name}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Auth.create_group(%{name: name, user_id: user.id}) do
          {:ok, %{group: group, relation: _}} -> render(conn, "group.json", group: group)
          {:error, _, changeset, %{}} -> process_error(conn, {:error, changeset})
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    delete(conn, %{"group_id" => group_id})
  """
  def delete(conn, %{"group_id" => group_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Auth.delete_group(%{group_id: group_id, user_id: user.id}) do
          {:ok, _} -> Util.empty_json(conn)
          {:noop, _} -> Util.empty_json(conn)
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    tags(conn, %{"group_id" => group_id})
  """
  def tags(conn, %{"group_id" => group_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.fetch_tags(%{group_id: group_id, user_id: user.id}) do
          {:ok, tags} -> render(conn, "tags.json", tags: tags)
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    new_tag(conn, %{"group_id" => group_id, "name" => name})
  """
  def new_tag(conn, %{"group_id" => group_id, "name" => name}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.create_tag(%{name: name, group_id: group_id, user_id: user.id}) do
          {:ok, tag} -> render(conn, "tag.json", tag: tag)
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end

  @doc """
    new_tag(conn, %{"group_id" => group_id, "name" => name})
  """
  def delete_tag(conn, %{"group_id" => _group_id, "tag_id" => tag_id}) do
    case Auth.check_conn_token(conn) do
      {:ok, user} ->
        case Content.delete_tag(%{tag_id: tag_id, user_id: user.id}) do
          {:ok, tag} -> render(conn, "tag.json", tag: tag)
          {:noop, _} -> process_error(conn, {:error, Errors.not_found})
          err -> process_error(conn, err)
        end
      error -> process_error(conn, error)
    end
  end
end
