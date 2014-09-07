ALTER TABLE posts ADD COLUMN hashtags VARCHAR(256) ARRAY;
COMMENT ON COLUMN posts.hashtags IS
'An array holding all valid hashtags in this post.';
