CREATE TABLE remote_storage_connections(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , access_token VARCHAR(256)
    , handle VARCHAR(128)
    , storage_url VARCHAR(512)
    , PRIMARY KEY(id)
    , CONSTRAINT remote_storage_connections_account_id_key UNIQUE (account_id)
);
