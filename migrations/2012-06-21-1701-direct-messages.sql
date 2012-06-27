CREATE TABLE messages (
      id SERIAL PRIMARY KEY
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , sender_member_id INTEGER REFERENCES members(id)
    , text VARCHAR(4096) NOT NULL CONSTRAINT text_not_empty CHECK (
        text !~ '^\s*$'
    )
);

COMMENT ON TABLE messages IS
'Direct messages, each visible only to sender and recipients.';

CREATE TABLE message_recipients (
      message_id INTEGER NOT NULL REFERENCES messages(id)
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , UNIQUE( message_id, member_id )
);

CREATE OR REPLACE VIEW view__messages_sent_and_received AS
    (
        SELECT
              m.sender_member_id AS member_id
            , m.id
            , m.time_created
            , m.sender_member_id
            , m.text
            , TRUE AS sent_by_member
            , NULL AS recipient_member_id
        FROM
            messages m
    ) UNION (
        SELECT
              mr.member_id
            , m.id
            , m.time_created
            , m.sender_member_id
            , m.text
            , FALSE AS sent_by_member
            , mr.member_id AS recipient_member_id
        FROM
              messages m
            , message_recipients mr
        WHERE
            mr.message_id = m.id
    )
;

COMMENT ON VIEW view__messages_sent_and_received IS
'SELECT from this view by member_id, the member to or by whom the messages were sent.';
