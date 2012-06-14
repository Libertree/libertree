ALTER TABLE accounts
ADD COLUMN excerpt_max_height INTEGER
DEFAULT 200
CONSTRAINT valid_excerpt_max_height CHECK (
    excerpt_max_height >= 200
);

COMMENT ON COLUMN accounts.excerpt_height IS
'The maximum height in pixels of excerpts on the home page.';
