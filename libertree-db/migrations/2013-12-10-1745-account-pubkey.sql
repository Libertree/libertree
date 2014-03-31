ALTER TABLE accounts ADD COLUMN pubkey VARCHAR(4096) DEFAULT NULL;

COMMENT ON COLUMN accounts.pubkey IS
'An optional public key in ASCII armour format.';
