# Auth

## Group
  * name, string, not null
  * has_many users through user_groups
  * has_many notes
  * has_many tags
  * has_one user (as owner)
### Rules
  * Name can not be empty.
  * Last user to leave triggers a delete.
  * User Group relation is unique.
  * Owner can transfer ownership.
### Phoenix Gen
`mix phx.gen.context
Auth Group groups
name:string:unique`

## User
  * name, string, not null
  * email, string, unique, not null
  * password, text, not null
  * password_in, virtual text
  * has_many groups through user_groups
### Rules
  * name can not be empty.
  * email can not be empty.
  * on create, gets a group called "{{name}}'s Junks"
  * password is a hash
### Phoenix Gen
`mix phx.gen.context
Auth User users
email:unique
name
password:text`

## ResetToken
  * token, string, not null
  * user_id, unique, references users
  * expires, naive_datetime, not null
### Rules
  * token is not nullable
  * only one entry per user can ever exist at a time
### Phoenix Gen
`mix phx.gen.context
Auth ResetToken reset_tokens
user_id:references:users
token:string
expires:naive_datetime`

## User Group
  * user_id, refernces users
  * group_id, refernces groups
### Rules
  * User ID - Group ID combo must be unique.
    * create index(:user_groups, [:user_id, :group_id], unique: true)
### Phoenix Gen
`mix phx.gen.context
Auth UserGroup user_groups
user_id:references:users
group_id:references:groups
`

# Content

## Tag
  * name, citext, not null
  * many_to_many notes through note_tags
    * many_to_many :notes, Content.Note, join_through: "note_tags"
  * belongs_to group
### Rules
  * Name can not be empty.
  * Input case is preserved on insert, but uniqueness is case insensitive.
  * Do not allow duplicate Tag-Note relations.
  * Name is unique to group.
### Phoenix Gen
`mix phx.gen.context
Content Tag tags
name:unique
group_id:references:groups`

## Note
  * title, string, not null
  * body, text, not null, default ""
  * group_id references groups
  * many_to_many tags through note_tags
    * many_to_many :tags, Content.Tag, join_through: "note_tags"
### Rules
  * Title can not be empty.
  * Title is unique to group.
  * Body can be empty, but can not be null.
  * Body is GitHub flavoured Markdown
### Phoenix Gen
`mix phx.gen.context
Content Note notes
title
body:text
group_id:references:groups`

## Note Tag
  * tag_id, refernces tags
  * node_id, refernces notes
### Rules
  * Note ID - Tag ID combo must be unique.
    * create index(:note_tags, [:tag_id, :note_id], unique: true)
### Phoenix Gen
`mix phx.gen.context
Content NoteTag note_tags
note_id:references:notes
tag_id:references:tags
`
