CREATE OR REPLACE FUNCTION account_collected_post(account_id INTEGER, post_id INTEGER) RETURNS BOOLEAN AS $$
    SELECT EXISTS (
        SELECT 1
        FROM
              pools_posts pp
            , pools p
            , members m
        WHERE
            m.account_id = $1
            AND p.member_id = m.id
            AND pp.pool_id = p.id
            AND post_id = $2
    );
$$ LANGUAGE SQL;
