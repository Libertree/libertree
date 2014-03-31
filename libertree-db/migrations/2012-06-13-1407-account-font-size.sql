ALTER TABLE accounts
ADD COLUMN font_css VARCHAR(32)
CONSTRAINT valid_font_css CHECK (
    font_css ~ '^[a-zA-Z0-9_-]+$'
);

COMMENT ON COLUMN accounts.font_css IS
'The base part of the CSS filename.  e.g. "foo" in "foo.css".  This is not a path.  No directory indirection allowed.  NULL means default.';
