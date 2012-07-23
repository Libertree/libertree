ALTER TABLE accounts ADD COLUMN autoembed BOOLEAN DEFAULT FALSE;
COMMENT ON COLUMN accounts.autoembed IS 'A setting to enable automatic resource embedding';
