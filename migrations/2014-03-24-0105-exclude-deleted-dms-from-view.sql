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
        WHERE
            NOT m.deleted
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
            AND NOT mr.deleted
    )
;
