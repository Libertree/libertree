CREATE TABLE post_subscriptions (
      id SERIAL PRIMARY KEY
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
);

COMMENT ON TABLE post_subscriptions IS
'Accounts subscribed to posts for notifications about new comments.';

-- Backfill subscriptions

INSERT INTO post_subscriptions(
      account_id
    , post_id
) SELECT
      m.account_id
    , p.id
FROM
      posts p
    , members m
WHERE
    m.id = p.member_id
    AND m.account_id IS NOT NULL
;

INSERT INTO post_subscriptions(
      account_id
    , post_id
) SELECT
      m.account_id
    , c.post_id
FROM
      comments c
    , members m
WHERE
    m.id = c.member_id
    AND m.account_id IS NOT NULL
    AND NOT EXISTS(
        SELECT 1
        FROM post_subscriptions ps
        WHERE
            ps.account_id = m.account_id
            AND ps.post_id = c.post_id
    )
;
