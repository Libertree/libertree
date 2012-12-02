CREATE FUNCTION mark_post_as_read_by(post_id INTEGER, account_id INTEGER) RETURNS void AS $$
    INSERT INTO posts_read ( post_id, account_id )
    SELECT $1, $2
    WHERE NOT EXISTS (
      SELECT 1
      FROM posts_read
      WHERE
        post_id = $1
        AND account_id = $2
    );

    DELETE FROM river_posts rp
    USING rivers r
    WHERE
      rp.river_id = r.id
      AND r.account_id = $2
      AND r.query LIKE '%:unread%'
      AND rp.post_id = $1;
$$ LANGUAGE SQL;
