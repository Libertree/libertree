CREATE INDEX posts_by_member_id            ON posts(member_id);
CREATE INDEX post_by_post_id               ON post_revisions(post_id);
CREATE INDEX post_likes_by_post_id         ON post_likes(post_id);
CREATE INDEX posts_read_by_post_id         ON posts_read(post_id);
CREATE INDEX posts_hidden_by_post_id       ON posts_hidden(post_id);
CREATE INDEX post_subscriptions_by_post_id ON post_subscriptions(post_id);
CREATE INDEX pool_posts_by_post_id         ON pools_posts(post_id);
CREATE INDEX river_posts_by_river_id       ON river_posts(river_id);
CREATE INDEX river_posts_by_post_id        ON river_posts(post_id);
CREATE INDEX comments_by_member_id         ON comments(member_id);
CREATE INDEX comment_likes_by_comment_id   ON comment_likes(comment_id);

CREATE OR REPLACE FUNCTION delete_cascade_member(member_id INTEGER) RETURNS void AS $$
    SELECT delete_cascade_pool(p.id)
           FROM pools p WHERE p.member_id = $1;
    SELECT delete_cascade_post(p.id)
           FROM posts p WHERE p.member_id = $1;
    SELECT delete_cascade_post_like(l.id)
           FROM post_likes l WHERE l.member_id = $1;
    SELECT delete_cascade_comment(c.id)
           FROM comments c WHERE c.member_id = $1;
    SELECT delete_cascade_comment_like(l.id)
           FROM comment_likes l WHERE l.member_id = $1;

    DELETE FROM chat_messages WHERE to_member_id = $1 OR from_member_id = $1;
    DELETE FROM contact_lists_members WHERE member_id = $1;
    DELETE FROM message_recipients
           WHERE member_id = $1
           OR message_id IN (SELECT id FROM messages WHERE sender_member_id = $1);
    DELETE FROM messages WHERE sender_member_id = $1;
    DELETE FROM
        messages m
    WHERE
        NOT EXISTS (
            SELECT 1
            FROM message_recipients mr
            WHERE mr.message_id = m.id
        )
    ;
    DELETE FROM profiles WHERE member_id = $1;

    DELETE FROM members WHERE id = $1;
$$ LANGUAGE SQL;
