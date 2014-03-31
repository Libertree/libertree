DROP VIEW view__posts_with_time_updated_overall;

ALTER TABLE posts DROP COLUMN public;
ALTER TABLE posts ADD COLUMN visibility VARCHAR(32) NOT NULL DEFAULT 'forest';
ALTER TABLE posts ADD CONSTRAINT valid_visibility CHECK (
    visibility = 'internet'
    OR visibility = 'forest'
);

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
