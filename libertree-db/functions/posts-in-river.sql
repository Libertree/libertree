CREATE OR REPLACE FUNCTION posts_in_river(river_id INTEGER, account_id INTEGER, compare_time TIMESTAMP WITH TIME ZONE, newer BOOLEAN, comment_order BOOLEAN, nlimit INTEGER) RETURNS SETOF posts AS $$
       SELECT * FROM (
         SELECT
           p.*
         FROM
             river_posts rp
           , posts p
         WHERE
           p.id = rp.post_id
           AND rp.river_id = $1
           AND CASE
             WHEN ($5 AND $4) THEN
               GREATEST(p.time_commented, p.time_updated) > $3
             WHEN ($5 AND NOT $4) THEN
               GREATEST(p.time_commented, p.time_updated) < $3
             WHEN (NOT $5 AND $4) THEN
               p.time_created > $3
             ELSE
               p.time_created < $3
             END
           AND NOT post_hidden_by_account( rp.post_id, $2 )
         ORDER BY CASE
           WHEN $5 THEN
             GREATEST(p.time_commented, p.time_updated)
           ELSE
             p.time_created
           END
         DESC LIMIT $6
       ) AS x
       ORDER BY CASE
         WHEN $5 THEN
           GREATEST(time_commented, time_updated)
         ELSE
           time_created
         END;
$$ LANGUAGE SQL IMMUTABLE;
