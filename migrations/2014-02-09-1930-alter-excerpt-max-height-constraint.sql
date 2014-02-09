ALTER TABLE accounts DROP CONSTRAINT valid_excerpt_max_height;
ALTER TABLE accounts ADD CONSTRAINT valid_excerpt_max_height CHECK (
    excerpt_max_height >= 0
);
