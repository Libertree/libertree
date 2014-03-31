CREATE TABLE url_expansions (
    id SERIAL PRIMARY KEY
  , url_short VARCHAR(1024) UNIQUE
  , url_expanded VARCHAR(1024)
);
