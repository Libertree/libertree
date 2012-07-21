CREATE TABLE embed_cache (
    id SERIAL PRIMARY KEY
  , url VARCHAR(512) UNIQUE
  , object VARCHAR(1024)
);
COMMENT ON TABLE embed_cache IS
'URLs of embeddable resources and their matching objects.';
