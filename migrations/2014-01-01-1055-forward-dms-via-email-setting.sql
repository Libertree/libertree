ALTER TABLE accounts ADD COLUMN forward_dms_via_email BOOLEAN DEFAULT FALSE;
COMMENT ON COLUMN accounts.forward_dms_via_email IS 'A setting to forward received direct messages to the email address stored for the account';
