CREATE FUNCTION river_contains_post(river_id INTEGER, post_id INTEGER) RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT 1 FROM river_posts WHERE river_id = $1 AND post_id = $2
    );
$$ LANGUAGE SQL;

CREATE FUNCTION post_hidden_by_account(post_id INTEGER, account_id INTEGER) RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT 1 FROM posts_hidden WHERE post_id = $1 AND account_id = $2
    );
$$ LANGUAGE SQL;
