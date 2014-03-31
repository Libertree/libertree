ALTER TABLE posts DROP CONSTRAINT valid_visibility;
ALTER TABLE posts ADD CONSTRAINT valid_visibility CHECK (
    visibility = 'internet'
    OR visibility = 'forest'
    OR visibility = 'tree'
);
