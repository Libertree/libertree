ALTER TABLE members ADD COLUMN gateway_jid VARCHAR(3071) UNIQUE;

COMMENT ON COLUMN members.gateway_jid IS
'The JID that has been registered with the remote member through the Libertree gateway service.  Local accounts have their own gateway_jid field.';
