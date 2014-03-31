ALTER TABLE accounts ADD COLUMN custom_link VARCHAR(512);
COMMENT ON COLUMN accounts.custom_link IS 'A custom URL that the member can provide for the header menu';
