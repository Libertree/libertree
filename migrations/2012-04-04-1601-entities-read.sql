CREATE TABLE posts_read(
      post_id INTEGER NOT NULL REFERENCES posts(id)
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , PRIMARY KEY( post_id, account_id )
);

CREATE TABLE comments_read(
      comment_id INTEGER NOT NULL REFERENCES comments(id)
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , PRIMARY KEY( comment_id, account_id )
);
