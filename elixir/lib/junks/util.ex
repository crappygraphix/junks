defmodule Junks.Util do
  alias Ecto.Changeset
  alias Plug.Conn

  @doc false
  def validate_not_nil(changeset, fields) do
    Enum.reduce(fields, changeset, fn field, changeset ->
      if Changeset.get_field(changeset, field) == nil do
        Changeset.add_error(changeset, field, "_MISSING")
      else
        changeset
      end
    end)
  end

  @doc false
  def empty_json(conn) do
    conn
    |> Conn.put_status(:no_content)
    |> Conn.put_resp_header("content-type", "application/json")
    |> Conn.send_resp(204, "")
  end

  @doc false
  def strip_prefix(prefix, full) do
    base = byte_size(prefix)
    binary_part(full, base, byte_size(full) - base)
  end
end
