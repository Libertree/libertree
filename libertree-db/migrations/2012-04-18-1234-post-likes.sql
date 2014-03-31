CREATE TABLE post_likes(
      id SERIAL
    , remote_id INTEGER
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , PRIMARY KEY(id)
);
