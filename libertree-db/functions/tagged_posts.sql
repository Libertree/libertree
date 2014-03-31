CREATE OR REPLACE FUNCTION tagged_posts(tag VARCHAR, compare_time TIMESTAMP WITH TIME ZONE, newer BOOLEAN, comment_order BOOLEAN, nlimit INTEGER) RETURNS SETOF posts AS $$
         SELECT
           *
         FROM
           posts
         WHERE
           text ~* (E'(^|\\s|\\()#' || $1 || E'(\\M|\\s|$|[[:punct:]])')
           AND CASE
             WHEN ($4 AND $3) THEN
               GREATEST(time_commented, time_updated) > $2
             WHEN ($4 AND NOT $3) THEN
               GREATEST(time_commented, time_updated) < $2
             WHEN (NOT $4 AND $3) THEN
               time_created > $2
             ELSE
               time_created < $2
           END
         ORDER BY CASE
           WHEN $4 THEN
             GREATEST(time_commented, time_updated)
           ELSE
             time_created
           END
         DESC LIMIT $5;
$$ LANGUAGE SQL;
