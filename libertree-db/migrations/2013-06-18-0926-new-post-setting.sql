ALTER TABLE accounts ALTER COLUMN autoembed SET NOT NULL;
ALTER TABLE accounts ALTER COLUMN filter_images SET NOT NULL;
ALTER TABLE accounts ALTER COLUMN icons SET NOT NULL;

COMMENT ON COLUMN accounts.icons IS
'A setting to choose whether tools are presented as text or as icons';

ALTER TABLE accounts ADD COLUMN new_post_in_river BOOLEAN NOT NULL DEFAULT FALSE;

COMMENT ON COLUMN accounts.new_post_in_river IS
'A setting to choose whether a new post can be made from the river view (in addition to the standalone New Post page).';
