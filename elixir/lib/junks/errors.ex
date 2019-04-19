defmodule Junks.Errors do
  # Suffix Errors
  def missing do "_MISSING" end
  def exists do "_EXISTS" end
  def empty do "_EMPTY" end
  def short do "_SHORT" end
  def invalid do "_INVALID" end
  def no_fk do "_NO_FK" end
  # Whole Errors
  def server do "SERVER_ERROR" end
  def no_match do "PASSWORD_MISMATCH" end
  def bad_auth do "BAD_AUTH" end
  def bad_token do "BAD_TOKEN" end
  def not_found do "NOT_FOUND" end
  def no_permission do "INVALID_PERMISSIONS" end
end
