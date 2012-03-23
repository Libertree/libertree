CREATE TABLE accounts(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , username VARCHAR(64) NOT NULL UNIQUE
    , password_encrypted VARCHAR(512) NOT NULL
    , email VARCHAR(512)
    , PRIMARY KEY(id)
);
COMMENT ON TABLE accounts IS
'Local user accounts.  For logging in, changing settings, etc.';

CREATE TABLE members(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , guid VARCHAR(64) NOT NULL UNIQUE
    , username VARCHAR(64) NOT NULL
    , host VARCHAR(256) NOT NULL
    , account_id INTEGER REFERENCES accounts(id)
    , PRIMARY KEY(id)
    , UNIQUE( username, host )
);
COMMENT ON TABLE members IS
'Representation of both local and remote users.  For public profiles, associating contacts, etc.';
COMMENT ON COLUMN members.account_id IS
'If NULL, the member is remote.  If not NULL, the member is local.';

CREATE TABLE profiles(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , name_display VARCHAR(128)
    , description VARCHAR(2048)
    , PRIMARY KEY(id)
);

CREATE TABLE posts(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , public BOOLEAN NOT NULL DEFAULT FALSE
    , guid VARCHAR(64) NOT NULL UNIQUE
    , text VARCHAR(16384) NOT NULL
    , PRIMARY KEY(id)
);

CREATE TABLE comments(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , guid VARCHAR(64) NOT NULL UNIQUE
    , text VARCHAR(16384) NOT NULL
    , PRIMARY KEY(id)
);

CREATE TABLE sharings(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , from_member_id INTEGER NOT NULL REFERENCES members(id)
    , to_member_id INTEGER NOT NULL REFERENCES members(id)
    , accepted BOOLEAN NOT NULL DEFAULT FALSE
    , PRIMARY KEY(id)
);
COMMENT ON TABLE sharings IS
'Sharing of public posts, to be read in stream pages.';
COMMENT ON COLUMN sharings.accepted IS
'This is only meaningful for local members.';

CREATE TABLE contact_categories(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , name VARCHAR(64) NOT NULL
    , description VARCHAR(256) NOT NULL
    , PRIMARY KEY(id)
    , UNIQUE( account_id, name )
);

CREATE TABLE contacts(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , contact_category_id INTEGER NOT NULL REFERENCES contact_categories(id)
    , notes VARCHAR(1024)
    , PRIMARY KEY(id)
);
