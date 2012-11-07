ALTER TABLE accounts ADD COLUMN filter_images BOOLEAN DEFAULT FALSE;
COMMENT ON COLUMN accounts.filter_images IS 'A setting to disable markdown rendering of image links';
