defmodule Junks.Content do
  @moduledoc """
  The Content context.
  """

  import Ecto.Query, warn: false
  alias Junks.Repo
  alias Junks.Errors

  alias Junks.Auth
  alias Junks.Auth.Group
  alias Junks.Auth.User
  alias Junks.Auth.UserGroup

  alias Junks.Content.Note
  alias Junks.Content.NoteTag
  alias Junks.Content.Tag

  @doc """
    can_view(%{user_id: user_id, note_id: note_id})

    :yes if user is a note's group's member, :no otherwise
  """
  def can_view(%{user_id: user_id, note_id: note_id}) do
    case Repo.all(
      from n in Note,
      join: g in Group, on: g.id == n.group_id,
      left_join: ug in UserGroup, on: g.id == ug.group_id,
      where: n.id == ^note_id and (ug.user_id == ^user_id or g.user_id == ^user_id)
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    owns_note(%{user_id: user_id, note_id: note_id})

    :yes if user owns a note, :no otherwise
  """
  def owns_note(%{user_id: user_id, note_id: note_id}) do
    case Repo.all(
      from n in Note,
      where: n.id == ^note_id and
             n.user_id == ^user_id
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    can_tag(%{note_id: note_id, tag_id: tag_id})

    :yes if note and tag are in the same group, :no otherwise
  """
  def can_tag(%{note_id: note_id, tag_id: tag_id}) do
    case Repo.all(
      from n in Note,
      join: t in Tag, on: n.group_id == t.group_id,
      where: n.id == ^note_id and
             t.id == ^tag_id
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    owns_tag(%{user_id: user_id, tag_id: tag_id})

    :yes if user owns a tag's group, :no otherwise
  """
  def owns_tag(%{user_id: user_id, tag_id: tag_id}) do
    case Repo.all(
      from g in Group,
      join: t in Tag, on: g.id == t.group_id,
      join: u in User, on: g.user_id == u.id,
      where: u.id == ^user_id and
             t.id == ^tag_id
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    create_note(%{title: title, body: body, group_id: group_id, user_id: user_id})
  """
  def create_note(%{title: title, body: body, group_id: group_id, user_id: user_id}) do
    case Auth.owns_group(%{user_id: user_id, group_id: group_id}) do
      :yes ->
        attrs = %{
          title: title,
          body: body,
          group_id: group_id,
          user_id: user_id
        }
        %Note{}
        |> Note.cs_create(attrs)
        |> Repo.insert()
      _ -> {:error, translate_error(%{group_id: group_id})}
    end
  end

  @doc """
    get_note(%{note_id: note_id, user_id: user_id})
  """
  def get_note(%{note_id: note_id, user_id: user_id}) do
    case can_view(%{user_id: user_id, note_id: note_id}) do
      :yes -> case Repo.get(Note, note_id) do
          nil -> {:error, Errors.not_found}
          n -> {:ok, n}
        end
      _ -> {:error, translate_error(%{note_id: note_id})}
    end
  end

  @doc """
    fetch_tags(%{group_id: group_id, user_id: user_id})
  """
  def fetch_tags(%{group_id: group_id, user_id: user_id}) do
    case Auth.owns_group(%{user_id: user_id, group_id: group_id}) do
      :yes -> {:ok, Repo.all(from t in Tag, where: t.group_id == ^group_id)}
      _ -> {:error, translate_error(%{group_id: group_id})}
    end
  end

  @doc """
    create_tag(%{name: name, group_id: group_id, user_id: user_id})
  """
  def create_tag(%{name: name, group_id: group_id, user_id: user_id}) do
    case Auth.owns_group(%{user_id: user_id, group_id: group_id}) do
      :yes ->
        attrs = %{
          name: name,
          group_id: group_id
        }
        %Tag{}
        |> Tag.cs_create(attrs)
        |> Repo.insert()
      _ -> {:error, translate_error(%{group_id: group_id})}
    end
  end

  @doc """
    attach_tag(%{note_id: note_id, tag_id: tag_id, user_id: user_id})
  """
  def attach_tag(%{note_id: note_id, tag_id: tag_id, user_id: user_id}) do
    case { owns_note(%{user_id: user_id, note_id: note_id}), can_tag(%{note_id: note_id, tag_id: tag_id}) } do
      {:yes, :yes} ->
        attrs = %{
          note_id: note_id,
          tag_id: tag_id
        }
        %NoteTag{}
        |> NoteTag.cs_create(attrs)
        |> Repo.insert()
      _ -> {:error, translate_error(%{note_id: note_id, tag_id: tag_id})}
    end
  end

  @doc """
    update_note(%{note_id: note_id, title: title, body: body, user_id: user_id})

    {:ok, Note}

    {:error, error}
  """
  def update_note(%{note_id: note_id, title: title, body: body, user_id: user_id}) do
    case owns_note(%{user_id: user_id, note_id: note_id}) do
      :yes ->
        case Repo.get(Note, note_id) do
          nil -> {:error, Errors.not_found}
          n ->
            attrs = %{
              title: title,
              body: body
            }
            n
            |> Note.cs_update(attrs)
            |> Repo.update()
        end
      _ -> {:error,  translate_error(%{note_id: note_id})}
    end
  end

  @doc """
    detach_tag(%{note_id: note_id, tag_id: tag_id, user_id: user_id})

    {:ok, NoteTag}

    {:error, error}

    {:noop, %{}}
  """
  def detach_tag(%{note_id: note_id, tag_id: tag_id, user_id: user_id}) do
    case { owns_note(%{user_id: user_id, note_id: note_id}), can_tag(%{note_id: note_id, tag_id: tag_id}) } do
      {:yes, :yes} ->
        case Repo.get_by(NoteTag, [tag_id: tag_id, note_id: note_id]) do
          nil -> {:noop, %{}} # Lack of existence is the same as removal from existence.
          nt -> Repo.delete nt
        end
      _ -> {:error,  translate_error(%{note_id: note_id, tag_id: tag_id})}
    end
  end

  @doc """
    delete_tag(%{tag_id: tag_id, user_id: user_id})

    {:ok, Tag}

    {:error, error}

    {:noop, %{}}
  """
  def delete_tag(%{tag_id: tag_id, user_id: user_id}) do
    case owns_tag(%{user_id: user_id, tag_id: tag_id}) do
      :yes ->
        case Repo.get(Tag, tag_id) do
          nil -> {:error, Errors.not_found} # Lack of existence is the same as removal from existence.
          nt -> Repo.delete nt
        end
      _ -> {:error, translate_error(%{tag_id: tag_id})}
    end
  end

  @doc """
    delete_note(%{note_id: note_id, user_id: user_id})

    {:ok, Note}

    {:error, error}

    {:noop, %{}}
  """
  def delete_note(%{note_id: note_id, user_id: user_id}) do
    case owns_note(%{user_id: user_id, note_id: note_id}) do
      :yes ->
        case Repo.get(Note, note_id) do
          nil -> {:noop, %{}} # Lack of existence is the same as removal from existence.
          n -> Repo.delete n
        end
      _ -> {:error, translate_error(%{note_id: note_id})}
    end
  end

  @doc """
    List of groups with shallow list of notes under each group

    notes_by_group(%{user_id: user_id})
  """
  def notes_by_group(%{user_id: user_id}) do
    ( from groups in Group,
      left_join: users in assoc(groups, :users),
      where: users.id == ^user_id,
      preload: [:notes] )
    |> Repo.all
  end

  defp translate_error(%{note_id: note_id}) do
    case Repo.get(Note, note_id) do
      nil -> Errors.not_found
      _ -> Errors.no_permission
    end
  end
  defp translate_error(%{group_id: group_id}) do
    case Repo.get(Group, group_id) do
      nil -> Errors.not_found
      _ -> Errors.no_permission
    end
  end
  defp translate_error(%{tag_id: tag_id}) do
    case Repo.get(Tag, tag_id) do
      nil -> Errors.not_found
      _ -> Errors.no_permission
    end
  end
  defp translate_error(%{note_id: note_id, tag_id: tag_id}) do
    case {Repo.get(Note, note_id), Repo.get(Tag, tag_id)} do
      {nil, _} -> Errors.not_found
      {_, nil} -> Errors.not_found
      {nil, nil} -> Errors.not_found
      _ -> Errors.no_permission
    end
  end
end
