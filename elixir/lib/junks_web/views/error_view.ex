defmodule JunksWeb.ErrorView do
  use JunksWeb, :view

  import String

  # If you want to customize a particular status code
  # for a certain format, you may uncomment below.
  # def render("500.html", _assigns) do
  #   "Internal Server Error"
  # end

  # By default, Phoenix returns the status message from
  # the template name. For example, "404.html" becomes
  # "Not Found".

  @doc false
  def template_not_found(template, _assigns) do
    Phoenix.Controller.status_message_from_template(template)
  end

  @doc "Renders an error results from a failed Ecto.Changeset."
  def render("changeset_error.json", %{changeset: changeset}) do
    %{
      errors: Enum.map(changeset.errors, fn({k, {v, _}}) ->
        to_string(k) <> v
        |> upcase()
      end)
    }
  end

  @doc "Renders a custom error."
  def render("custom_error.json", %{errors: errors}) do
    %{
      errors: errors
    }
  end
end
