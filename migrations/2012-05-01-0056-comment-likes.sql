CREATE TABLE comment_likes(
      id SERIAL
    , remote_id INTEGER
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , comment_id INTEGER NOT NULL REFERENCES comments(id)
    , PRIMARY KEY(id)
);
