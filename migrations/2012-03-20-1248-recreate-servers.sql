DROP TABLE servers;

CREATE TABLE servers (
      id SERIAL
    , ip INET NOT NULL
    , public_key VARCHAR(4096) NOT NULL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , PRIMARY KEY(id)
);

CREATE INDEX servers_public_key ON servers(public_key);
