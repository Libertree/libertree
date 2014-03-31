CREATE TABLE rivers (
      id SERIAL
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , label VARCHAR(128) NOT NULL CHECK ( LENGTH(label) > 0 )
    , query VARCHAR(1024) NOT NULL CONSTRAINT valid_query CHECK (
        query ~ '^[a-z0-9:@# -]*$'
    )
    , UNIQUE( account_id, query )
    , PRIMARY KEY(id)
);

CREATE TABLE river_posts (
      river_id INTEGER NOT NULL REFERENCES rivers(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , UNIQUE( river_id, post_id )
);
COMMENT ON TABLE river_posts IS
'A table holding which posts match which rivers. i.e. meet the river''s query.';
