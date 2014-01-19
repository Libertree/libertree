CREATE OR REPLACE FUNCTION subscribe_account_to_post(account_id INTEGER, post_id INTEGER) RETURNS void AS $$
       INSERT INTO post_subscriptions (
           account_id
         , post_id
       ) SELECT $1, $2
       WHERE NOT EXISTS(
         SELECT 1
         FROM post_subscriptions ps
         WHERE
           ps.account_id = $1
           AND ps.post_id = $2
       )
$$ LANGUAGE SQL;
