CREATE TABLE post_revisions (
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , text VARCHAR(16384)
);
