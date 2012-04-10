CREATE TABLE sessions_accounts (
      sid VARCHAR(128) NOT NULL PRIMARY KEY
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
);
