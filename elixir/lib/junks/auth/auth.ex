defmodule Junks.Auth do
  @moduledoc """
  The Auth context.
  """
  @auth_scheme "Bearer"

  import Ecto.Query, warn: false
  alias Ecto.Multi
  alias Junks.Repo

  alias Junks.Auth.Group
  alias Junks.Auth.ResetToken
  alias Junks.Auth.User
  alias Junks.Auth.UserGroup
  alias Junks.Errors

  @doc """
    owns_group(%{user_id: user_id, group_id: group_id})

    :yes if user owns group, :no otherwise
  """
  def owns_group(%{user_id: user_id, group_id: group_id}) do
    case Repo.all(
      from g in Group,
      where: g.id == ^group_id and g.user_id == ^user_id
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    in_group(%{user_id: user_id, group_id: group_id})

    :yes if user in group, :no otherwise
  """
  def in_group(%{user_id: user_id, group_id: group_id}) do
    case Repo.all(
      from ug in UserGroup,
      where: ug.group_id == ^group_id and ug.user_id == ^user_id
    ) do
      [] -> :no
      _ -> :yes
    end
  end

  @doc """
    user_groups(%{user_id: user_id})

    [Group]
  """
  def user_groups(%{user_id: user_id}) do
    q = from g in Group,
      join: ug in UserGroup, on: g.id == ug.group_id,
      where: ug.user_id == ^user_id
    Repo.all(q)
  end

  @doc """
  create_user(%{name: name, email: email, password: password})

  Automatically creates a group "{{name}}'s Junks'"

  {:ok, %{user: User, group: Group, relation: UserGroup}

  {:error, :user, changeset, %{}}

  {:error, :group, changeset, %{}}

  {:error, :relation, changeset, %{}}
  """
  def create_user(%{name: name, email: email, password: password}) do
    attrs = %{
      name: name,
      email: email,
      password_in: password
    }
    multi = Multi.new
    |> Multi.insert(:user, User.cs_create(%User{}, attrs))
    |> Multi.run(:group,
      fn %{user: user} ->
        Group.cs_create(
          %Group{},
          %{user_id: user.id, name: user.name <> "'s Junks"})
        |> Repo.insert()
      end
    )
    |> Multi.run(:relation,
      fn %{user: user, group: group} ->
        UserGroup.cs_create(
          %UserGroup{},
          %{user_id: user.id, group_id: group.id})
        |> Repo.insert()
      end
    )
    Repo.transaction(multi)
  end

  @doc """
    create_group(%{name: name, user_id: user_id})

    {:ok, %{group: Group, relation: UserGroup}

    {:error, :group, changeset, %{}}

    {:error, :relation, changeset, %{}}
  """
  def create_group(%{name: name, user_id: user_id}) do
    attrs = %{
      name: name,
      user_id: user_id
    }
    multi = Multi.new
    |> Multi.insert(:group, Group.cs_create(%Group{}, attrs))
    |> Multi.run(:relation,
      fn %{group: group} ->
        UserGroup.cs_create(
          %UserGroup{},
          %{user_id: user_id, group_id: group.id})
        |> Repo.insert()
      end
    )
    Repo.transaction(multi)
  end

  @doc """
    join_group(%{user_id: user_id, group_id: group_id, invitor_id: invitor_id})

    {:ok, UserGroup}

    {:error, changeset}
  """
  def join_group(%{user_id: user_id, group_id: group_id, invitor_id: invitor_id}) do
    case owns_group(%{user_id: invitor_id, group_id: group_id}) do
      :yes ->
        attrs = %{
          user_id: user_id,
          group_id: group_id
        }
        UserGroup.cs_create(%UserGroup{}, attrs)
        |> Repo.insert()
      _ -> {:error, Errors.no_permission}
    end
  end

  @doc """
    authenticate(%{email: email, password: password})

    {:ok, User, token}

    {:error, error}
  """
  def authenticate(%{email: email, password: password}) do
    user = Repo.get_by(User, email: String.downcase(email))
    case check_password(user, password) do
      true -> {:ok, user, gen_token(user)}
      _ -> {:error, Errors.bad_auth}
    end
  end

  @doc """
    reauthenticate(conn)

    {:ok, User, token}

    {:error, error}
  """
  def reauthenticate(conn) do
    check_conn_token(conn)
    |> case do
      {:ok, user} -> {:ok, user, gen_token(user)}
      x -> x
    end
  end

  @doc """
    check_conn_token(conn)

    {:ok, User}

    {:error, error}
  """
  def check_conn_token(conn) do
    with {:ok, token} <- extract_token(conn),
         {:ok, user_id} <- extract_user_id(token),
         {:ok, key} <- extract_key(token),
         {:ok, user} <- user_by_id(%{user_id: user_id})
         do check_token(%{user: user, token: key}) end |>
         case do
           {:ok, user} -> {:ok, user}
           {:error, err} -> {:error, err}
         end
  end

  # Accepts a Connection.
  # Returns {:ok, token_string} or {:error, error_message}
  defp extract_token(conn) do
    conn
    |> get_authorization_header
    |> String.slice(String.length(@auth_scheme) + 1..-1)
    |> case do
         "" -> {:error, Errors.bad_token}
         t -> {:ok, t}
       end
  end

  defp extract_user_id(token) do
    token
    |> String.split("::")
    |> case do
      [] -> {:error, Errors.bad_token}
      t ->
        Integer.parse(hd(t))
        |> case do
          :error -> {:error, Errors.bad_token}
          {x, _} -> {:ok, x}
        end
    end
  end

  defp extract_key(token) do
    token
    |> String.split("::")
    |> case do
      [] -> {:error, Errors.bad_token}
      t ->
        case Enum.fetch(t, 1) do
          {:ok, k} -> {:ok, k}
          _ -> {:error, Errors.bad_token}
        end
      end
  end

  defp get_authorization_header(conn) do
    conn
    |> Plug.Conn.get_req_header("authorization")
    |> List.first
    |> case do
         nil -> ""
         t -> t
       end
  end

  @doc """
    check_token(%{user: User, token: token})

    {:ok, User}

    {:error, error}
  """
  def check_token(%{user: user, token: token}) do
    case Phoenix.Token.verify(JunksWeb.Endpoint, to_string(user.id) <> " " <> to_string(user.salt), token, max_age: 1209600) do # 2 weeks
      {:ok, _} -> {:ok, user}
      _ -> {:error, Errors.bad_token}
    end
  end
  def check_token(_, _) do :error end

  defp check_password(%User{} = user, password) do
    Comeonin.Bcrypt.checkpw(password, user.password)
  end
  defp check_password(_, _) do false end

  defp gen_token(%User{} = user) do
    Phoenix.Token.sign(JunksWeb.Endpoint, to_string(user.id) <> " " <> to_string(user.salt), :crypto.strong_rand_bytes(32))
  end

  @doc """
    create_reset_token(%{user: User})

    %{:ok, ResetToken}

    %{:error, Changeset}
  """
  def create_reset_token(%{user: user}) do
    a = :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
    b = to_string(user.id) |> Base.url_encode64(padding: false)
    c = :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
    attrs = %{
      user_id: user.id,
      token: a <> "-" <> b <> "-" <> c,
      expires: NaiveDateTime.add(NaiveDateTime.utc_now(), 86400) # Good for 1 day
    }
    case Repo.get_by(ResetToken, user_id: user.id) do
      nil -> %ResetToken{}
      tkn -> tkn
    end
    |> ResetToken.cs_create(attrs)
    |> Repo.insert_or_update() # Only one per user will ever exist
  end

  @doc """
    change_password(%{user: User, password: password})

    %{:ok, User}

    %{:error, Changeset}
  """
  def change_password(%{user: user, password: password}) do
    attrs = %{
      password_in: password
    }
    User.cs_password(user, attrs)
    |> Repo.update()
  end

  @doc """
    user_by_reset_token(%{token: token})

    %{:ok, User}

    %{:error, error}
  """
  def user_by_reset_token(%{token: token}) do
    check = Repo.one(
      from(t in ResetToken,
        where: t.token == ^token and t.expires >= ^NaiveDateTime.utc_now(),
        preload: [:user],
        select: t
      )
    )
    case check do
      nil -> {:error, Errors.bad_token}
      tkn -> {:ok, tkn.user}
    end
  end

  @doc """
    user_by_email(%{email: email})

    %{:ok, User}

    %{:error, error}
  """
  def user_by_email(%{email: email}) do
    case Repo.get_by(User, email: String.downcase(email)) do
      nil -> {:error, Errors.not_found}
      user -> {:ok, user}
    end
  end

  @doc """
    user_by_id(%{user_id: user_id})

    %{:ok, User}

    %{:error, error}
  """
  def user_by_id(%{user_id: user_id}) do
    case Repo.get(User, user_id) do
      nil -> {:error, Errors.not_found}
      user -> {:ok, user}
    end
  end

  @doc """
    delete_group(%{group_id: group_id, user_id: user_id})

    {:ok, Group}

    {:noop, %{}}

    {:error, error}
  """
  def delete_group(%{group_id: group_id, user_id: user_id}) do
    case owns_group(%{user_id: user_id, group_id: group_id}) do
      :yes ->
        case Repo.get(Group, group_id) do
          nil -> {:noop, %{}} # Lack of existence is the same as removal from existence.
          g -> Repo.delete g
        end
      _ -> {:error, Errors.no_permission}
    end
  end
end
