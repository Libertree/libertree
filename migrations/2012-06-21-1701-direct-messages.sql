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

CREATE TABLE messages_read (
      account_id INTEGER NOT NULL REFERENCES accounts(id)
    , message_id INTEGER NOT NULL REFERENCES messages(id)
    , UNIQUE( account_id, message_id )
);

-- ------------------


-- INSERT INTO messages ( sender_member_id, text ) VALUES ( 1, 'message 1' );
-- INSERT INTO messages ( sender_member_id, text ) VALUES ( 1, 'message 2' );
-- INSERT INTO message_recipients ( message_id, member_id ) VALUES ( 1, 2 );
-- INSERT INTO message_recipients ( message_id, member_id ) VALUES ( 2, 2 );
-- INSERT INTO messages ( sender_member_id, text ) VALUES ( 1, 'message 3' );
-- INSERT INTO message_recipients ( message_id, member_id ) VALUES ( 3, 2 );
-- INSERT INTO message_recipients ( message_id, member_id ) VALUES ( 3, 3 );
-- INSERT INTO messages ( sender_member_id, text ) VALUES ( 2, 'message 4' );
-- INSERT INTO message_recipients ( message_id, member_id ) VALUES ( 4, 3 );
