CREATE TABLE forests (
      id SERIAL PRIMARY KEY
    , name VARCHAR(256) NOT NULL
    , origin_server_id INTEGER REFERENCES servers(id)
    , remote_id INTEGER
    , local_is_member BOOLEAN NOT NULL DEFAULT FALSE
    , CONSTRAINT either_local_or_remote CHECK (
        origin_server_id IS NULL AND remote_id IS NULL
        OR origin_server_id IS NOT NULL AND remote_id IS NOT NULL
    )
);
COMMENT ON COLUMN forests.local_is_member IS
'Whether the arborista of the local server has consented to the server being a part of this forest';

CREATE TABLE forests_servers (
      forest_id INTEGER NOT NULL REFERENCES forests(id)
    , server_id INTEGER NOT NULL REFERENCES servers(id)
    , UNIQUE( forest_id, server_id )
);

ALTER TABLE servers ALTER COLUMN public_key DROP NOT NULL;
ALTER TABLE accounts ADD COLUMN admin BOOLEAN NOT NULL DEFAULT FALSE;
