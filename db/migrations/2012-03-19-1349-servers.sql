CREATE TABLE servers (
      id SERIAL
    , ip INET
    , key_public VARCHAR(4096)
    , time_created TIMESTAMP WITH TIME ZONE
    , PRIMARY KEY(id)
);
