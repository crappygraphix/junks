defmodule Junks.Mailer do
  use Swoosh.Mailer, otp_app: :junks
  import Swoosh.Email

  def forgot_password(user, tkn) do
    Task.start(fn ->
      build_forgot_password(user, tkn)
      |> deliver
    end)
  end

  def build_forgot_password(user, tkn) do
    base = JunksWeb.Endpoint.url

    html = "<h1>Hello #{user.name}.</h1>" <>
            "<p>Click the link below to reset your password.</p>" <>
            "<p>" <>
            "<a href=\"" <> base <> "/reset/" <> tkn <> "\" target=\"_blank\">Reset Password</a>" <>
            "</p>" <>
            "<small>This link will expire in 24 hours.</small>"

    text = "Hello #{user.name}.\n" <>
            "Copy and paste the link below into a web browser of your choice to reset your password.\n" <>
            base <> "/reset/" <> tkn <> "\n" <>
            "This link will expire in 24 hours.\n"

    new()
    |> to({user.name, user.email})
    |> from({"Junks", "noreply@junks.info"})
    |> subject("Password Reset")
    |> html_body(html)
    |> text_body(text)
  end
end
