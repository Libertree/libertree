ALTER TABLE accounts ADD COLUMN watched_post_id INTEGER REFERENCES posts(id);
ALTER TABLE accounts ADD COLUMN watched_post_last_comment_id INTEGER REFERENCES comments(id);
