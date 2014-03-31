ALTER TABLE accounts ADD COLUMN hide_markdown_bar BOOLEAN DEFAULT FALSE;
COMMENT ON COLUMN accounts.hide_markdown_bar IS 'A setting to hide the markdown injector in the frontend';
