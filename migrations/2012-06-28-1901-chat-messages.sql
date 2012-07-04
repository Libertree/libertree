CREATE TABLE chat_messages (
      id SERIAL PRIMARY KEY
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , from_member_id INTEGER NOT NULL REFERENCES members(id)
    , to_member_id INTEGER NOT NULL REFERENCES members(id)
    , seen BOOLEAN NOT NULL DEFAULT FALSE
    , text VARCHAR(512) NOT NULL CONSTRAINT text_not_empty CHECK (
        text !~ '^\s*$'
    )
);

COMMENT ON COLUMN chat_messages.seen IS
'The seen column is only used for local chat message recipients.  It has no meaning for remote recipients.';
