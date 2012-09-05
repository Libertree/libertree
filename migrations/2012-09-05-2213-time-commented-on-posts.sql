ALTER TABLE posts ADD COLUMN time_commented TIMESTAMP WITH TIME ZONE;
UPDATE posts p SET time_commented = (SELECT MAX(c.time_updated) FROM comments c WHERE c.post_id = p.id);
DROP VIEW view__posts_with_time_updated_overall;
