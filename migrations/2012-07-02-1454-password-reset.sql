ALTER TABLE accounts ADD COLUMN password_reset_code VARCHAR(64);
ALTER TABLE accounts ADD COLUMN password_reset_expiry TIMESTAMP WITH TIME ZONE;
