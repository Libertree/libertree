CREATE TABLE invitations (
      id SERIAL
    , email VARCHAR(256)
    , code VARCHAR(32) NOT NULL DEFAULT MD5(RANDOM()::TEXT)
    , account_id INTEGER REFERENCES accounts(id)
    , inviter_account_id INTEGER REFERENCES accounts(id)
    , PRIMARY KEY(id)
);

COMMENT ON COLUMN invitations.account_id IS
'The account created after this invitation was accepted.  NULL if the invitation has not been accepted yet.';
