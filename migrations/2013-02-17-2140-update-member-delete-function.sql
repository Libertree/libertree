CREATE OR REPLACE FUNCTION delete_cascade_member(member_id INTEGER) RETURNS void AS $$
    SELECT delete_cascade_pool(p.id)
           FROM pools p WHERE p.member_id = $1;
    SELECT delete_cascade_post(p.id)
           FROM posts p WHERE p.member_id = $1;
    SELECT delete_cascade_comment(c.id)
           FROM comments c WHERE c.member_id = $1;

    DELETE FROM contact_lists_members WHERE member_id = $1;

    DELETE FROM
      message_recipients
    WHERE message_id IN
      (SELECT id FROM messages WHERE sender_member_id = $1);
    DELETE FROM messages WHERE sender_member_id = $1;
    DELETE FROM message_recipients WHERE member_id = $1;
    DELETE FROM
        messages m
    WHERE
        NOT EXISTS (
            SELECT 1
            FROM message_recipients mr
            WHERE mr.message_id = m.id
        )
    ;
    DELETE FROM post_likes    WHERE member_id = $1;
    DELETE FROM comment_likes WHERE member_id = $1;
    DELETE FROM profiles      WHERE member_id = $1;

    DELETE FROM members WHERE id = $1;
$$ LANGUAGE SQL;
