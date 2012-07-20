CREATE TABLE embed_cache (
    id SERIAL PRIMARY KEY
  , url VARCHAR(512) UNIQUE
  , object VARCHAR(1024)
);
