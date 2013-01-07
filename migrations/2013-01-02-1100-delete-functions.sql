CREATE FUNCTION delete_cascade_post_like(like_id INTEGER) RETURNS void AS $$
    DELETE FROM notifications WHERE data = '{"type":"post-like","post_like_id":'|| $1 ||'}';
    DELETE FROM post_likes WHERE id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_comment_like(like_id INTEGER) RETURNS void AS $$
    DELETE FROM notifications WHERE data = '{"type":"comment-like","comment_like_id":'|| $1 ||'}';
    DELETE FROM comment_likes WHERE id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_comment(comment_id INTEGER) RETURNS void AS $$
    SELECT delete_cascade_comment_like(l.id) FROM comment_likes l WHERE l.comment_id = $1;
    DELETE FROM notifications WHERE data = '{"type":"comment","comment_id":'|| $1 ||'}';
    DELETE FROM comments WHERE id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_post(post_id INTEGER) RETURNS void AS $$
    SELECT delete_cascade_comment(c.id)
           FROM comments c WHERE c.post_id = $1;
    SELECT delete_cascade_post_like(l.id)
           FROM post_likes l WHERE l.post_id = $1;
    DELETE FROM posts_read         WHERE post_id = $1;
    DELETE FROM posts_hidden       WHERE post_id = $1;
    DELETE FROM pools_posts        WHERE post_id = $1;
    DELETE FROM river_posts        WHERE post_id = $1;
    DELETE FROM post_subscriptions WHERE post_id = $1;
    DELETE FROM post_revisions     WHERE post_id = $1;
    DELETE FROM posts              WHERE      id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_pool(pool_id INTEGER) RETURNS void AS $$
    DELETE FROM pools_posts WHERE pool_id = $1;
    DELETE FROM pools WHERE id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_contact_list(list_id INTEGER) RETURNS void AS $$
    DELETE FROM contact_lists_members WHERE contact_list_id = $1;
    DELETE FROM contact_lists WHERE id = $1;
$$ LANGUAGE SQL;

CREATE FUNCTION delete_cascade_river(river_id INTEGER) RETURNS void AS $$
    DELETE FROM river_posts WHERE river_id = $1;
    DELETE FROM rivers WHERE id = $1;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION delete_cascade_member(member_id INTEGER) RETURNS void AS $$
    SELECT delete_cascade_pool(p.id)
           FROM pools p WHERE p.member_id = $1;
    SELECT delete_cascade_post(p.id)
           FROM posts p WHERE p.member_id = $1;
    SELECT delete_cascade_comment(c.id)
           FROM comments c WHERE c.member_id = $1;

    DELETE FROM contact_lists_members WHERE member_id = $1;
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
    DELETE FROM profiles WHERE member_id = $1;

    DELETE FROM members WHERE id = $1;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION delete_cascade_account(account_id INTEGER) RETURNS void AS $$
    UPDATE invitations SET inviter_account_id = NULL WHERE inviter_account_id = $1;
    DELETE FROM invitations        WHERE account_id = $1;
    DELETE FROM sessions_accounts  WHERE account_id = $1;
    DELETE FROM notifications      WHERE account_id = $1;
    DELETE FROM post_subscriptions WHERE account_id = $1;
    DELETE FROM posts_read         WHERE account_id = $1;
    DELETE FROM posts_hidden       WHERE account_id = $1;
    DELETE FROM
        contact_lists_members clm
    USING
        contact_lists cl
    WHERE
        cl.account_id = $1
        AND clm.contact_list_id = cl.id
    ;
    DELETE FROM contact_lists      WHERE account_id = $1;

    SELECT delete_cascade_river(r.id)
           FROM rivers r WHERE r.account_id = $1;
    SELECT delete_cascade_member(m.id)
           FROM members m WHERE m.account_id = $1;

    DELETE FROM accounts WHERE id = $1;
$$ LANGUAGE SQL;
