CREATE OR REPLACE FUNCTION member_collects_post(member_id INTEGER, post_id INTEGER) RETURNS BOOLEAN AS $$
    SELECT EXISTS (
      SELECT 1 FROM pools_posts WHERE post_id = $2
      AND pool_id IN (
        SELECT id FROM pools WHERE member_id = $1
      )
    );
$$ LANGUAGE SQL;
