CREATE TABLE pools (
      id SERIAL PRIMARY KEY
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , name VARCHAR(64) NOT NULL
    , UNIQUE( account_id, name )
);

CREATE TABLE pools_posts (
      pool_id INTEGER NOT NULL REFERENCES pools(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , UNIQUE( pool_id, post_id )
);
