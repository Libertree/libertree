ALTER TABLE accounts ADD COLUMN gateway_jid VARCHAR(3071) UNIQUE;

COMMENT ON COLUMN accounts.gateway_jid IS
'The JID that has been registered with the account through the Libertree gateway service.';
