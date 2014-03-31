-- DROP and reCREATE because indices and sequences are named based on the table name

DROP TABLE post_ignores;

CREATE TABLE posts_hidden(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , PRIMARY KEY(id)
    , UNIQUE( account_id, post_id )
);
