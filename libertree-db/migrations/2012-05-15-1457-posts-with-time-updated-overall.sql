CREATE VIEW view__posts_with_time_updated_overall AS
SELECT
      p.*
    , COALESCE(
        (
            SELECT MAX(time_updated)
            FROM comments c
            WHERE c.post_id = p.id
        ),
        p.time_updated
    ) AS time_updated_overall
FROM
    posts p
;
