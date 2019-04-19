defmodule Junks.MailerTest do
  use Junks.DataCase
  import Swoosh.TestAssertions

  alias Junks.Auth

  def user_fixture(name, email, password) do
    {:ok, t} = Auth.create_user(%{name: name, email: email, password: password})
    t.user
  end

  test "send email forgot password" do
    user = user_fixture("Chris", "crappygraphix@gmail.com", "Password1")
    email = Junks.Mailer.build_forgot_password(user, "ABC123")
    Swoosh.Adapters.Test.deliver(email, [])
    assert_email_sent email
  end
end
