CREATE TABLE servers (
      id SERIAL
    , ip INET NOT NULL
    , public_key VARCHAR(4096) NOT NULL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , challenge VARCHAR(2048)
    , PRIMARY KEY(id)
);
COMMENT ON TABLE servers IS
'Records of remote servers.  The local server does not have a record in this table.';

CREATE INDEX servers_public_key ON servers(public_key);

CREATE TABLE accounts(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , username VARCHAR(64) NOT NULL UNIQUE
    , password_encrypted VARCHAR(512) NOT NULL
    , email VARCHAR(512)
    , PRIMARY KEY(id)
    , CONSTRAINT username_valid CHECK ( username ~ '^[a-z0-9_-]{2,}$' )
);
COMMENT ON TABLE accounts IS
'Local user accounts.  For server-local concerns, like logging in, changing settings, etc.  See also members and profiles tables.';

CREATE TABLE members(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , username VARCHAR(64) CHECK( length(username) > 0 )
    , server_id INTEGER REFERENCES servers(id)
    , account_id INTEGER REFERENCES accounts(id)
    , avatar_path VARCHAR(256)
    , PRIMARY KEY(id)
    , UNIQUE( username, server_id )
    , CONSTRAINT either_local_or_remote CHECK (
        username IS NULL AND server_id IS NULL AND account_id IS NOT NULL
        OR username IS NOT NULL AND server_id IS NOT NULL AND account_id IS NULL
    )
);
COMMENT ON TABLE members IS
'Representation of both local and remote users.  For inter-server concerns, like public profiles, associating contacts, etc.';
COMMENT ON COLUMN members.username IS
'A duplicate of remote data (remote accounts.username column).';
COMMENT ON COLUMN members.server_id IS
'If not NULL, the member is remote.  If NULL, the member is local.';
COMMENT ON COLUMN members.account_id IS
'If NULL, the member is remote.  If not NULL, the member is local.';
COMMENT ON COLUMN members.avatar_path IS
'A relative URL, not a filesystem path.';

CREATE TABLE posts(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , remote_id INTEGER
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , public BOOLEAN NOT NULL DEFAULT FALSE
    , text VARCHAR(16384) NOT NULL
    , PRIMARY KEY(id)
);

CREATE TABLE comments(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
    , text VARCHAR(16384) NOT NULL
    , PRIMARY KEY(id)
);
