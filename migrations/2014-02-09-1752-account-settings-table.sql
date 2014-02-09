CREATE TABLE account_settings (
      account_id INTEGER NOT NULL REFERENCES accounts(id)
    , excerpt_max_height INTEGER DEFAULT 200
    , custom_link VARCHAR(512)
    , custom_css VARCHAR(4096)
    , custom_js VARCHAR(4096)
    , autoembed BOOLEAN NOT NULL DEFAULT FALSE
    , thumbnail BOOLEAN NOT NULL DEFAULT FALSE
    , theme VARCHAR(128)
    , filter_images BOOLEAN NOT NULL DEFAULT FALSE
    , icons BOOLEAN NOT NULL DEFAULT FALSE
    , new_post_in_river BOOLEAN NOT NULL DEFAULT FALSE
    , hide_markdown_bar BOOLEAN NOT NULL DEFAULT FALSE
    , forward_dms_via_email BOOLEAN NOT NULL DEFAULT FALSE
    , PRIMARY KEY (account_id)
);

COMMENT ON COLUMN account_settings.excerpt_max_height IS
'The maximum height in pixels of excerpts on the home page';

COMMENT ON COLUMN account_settings.custom_link IS
'A custom URL that the member can provide for the header menu';

COMMENT ON COLUMN account_settings.autoembed IS
'Enable automatic resource embedding';

COMMENT ON COLUMN account_settings.thumbnail IS
'Whether display images as thumbnails or full size in excerpts';

COMMENT ON COLUMN account_settings.theme IS
'The frontend theme to use';

COMMENT ON COLUMN account_settings.filter_images IS
'disable markdown rendering of image links';

COMMENT ON COLUMN account_settings.icons IS
'whether tools are presented as text or as icons';

COMMENT ON COLUMN account_settings.new_post_in_river IS
'whether a new post can be made from the river view (in addition to the standalone New Post page)';

COMMENT ON COLUMN account_settings.hide_markdown_bar IS
'hide the markdown injector in the frontend';

COMMENT ON COLUMN account_settings.forward_dms_via_email IS
'forward received direct messages to the email address stored for the account';

INSERT INTO account_settings (
      account_id
    , excerpt_max_height
    , custom_link
    , custom_css
    , custom_js
    , autoembed
    , thumbnail
    , theme
    , filter_images
    , icons
    , new_post_in_river
    , hide_markdown_bar
    , forward_dms_via_email
) SELECT
      id
    , excerpt_max_height
    , custom_link
    , custom_css
    , custom_js
    , autoembed
    , thumbnail
    , theme
    , filter_images
    , icons
    , new_post_in_river
    , hide_markdown_bar
    , forward_dms_via_email
FROM
    accounts
;

ALTER TABLE accounts DROP COLUMN excerpt_max_height;
ALTER TABLE accounts DROP COLUMN custom_link;
ALTER TABLE accounts DROP COLUMN custom_css;
ALTER TABLE accounts DROP COLUMN custom_js;
ALTER TABLE accounts DROP COLUMN autoembed;
ALTER TABLE accounts DROP COLUMN thumbnail;
ALTER TABLE accounts DROP COLUMN theme;
ALTER TABLE accounts DROP COLUMN filter_images;
ALTER TABLE accounts DROP COLUMN icons;
ALTER TABLE accounts DROP COLUMN new_post_in_river;
ALTER TABLE accounts DROP COLUMN hide_markdown_bar;
ALTER TABLE accounts DROP COLUMN forward_dms_via_email;
