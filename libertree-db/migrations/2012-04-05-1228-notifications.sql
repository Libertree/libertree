CREATE TABLE notifications(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , seen BOOLEAN NOT NULL DEFAULT FALSE
    , data VARCHAR(1024) NOT NULL
    , PRIMARY KEY(id)
);
