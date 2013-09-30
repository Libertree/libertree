CREATE TABLE remote_storage_connections(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , access_token VARCHAR(256)
    , handle VARCHAR(128)
    , storage_url VARCHAR(512)
    , PRIMARY KEY(id)
    , CONSTRAINT remote_storage_connections_account_id_key UNIQUE (account_id)
);

CREATE OR REPLACE FUNCTION delete_cascade_account(account_id INTEGER) RETURNS void AS $$
    UPDATE invitations SET inviter_account_id = NULL WHERE inviter_account_id = $1;
    DELETE FROM invitations                WHERE account_id = $1;
    DELETE FROM sessions_accounts          WHERE account_id = $1;
    DELETE FROM notifications              WHERE account_id = $1;
    DELETE FROM post_subscriptions         WHERE account_id = $1;
    DELETE FROM posts_read                 WHERE account_id = $1;
    DELETE FROM posts_hidden               WHERE account_id = $1;
    DELETE FROM remote_storage_connections WHERE account_id = $1;
    DELETE FROM
        contact_lists_members clm
    USING
        contact_lists cl
    WHERE
        cl.account_id = $1
        AND clm.contact_list_id = cl.id
    ;
    DELETE FROM contact_lists WHERE account_id = $1;

    SELECT delete_cascade_river(r.id)
           FROM rivers r WHERE r.account_id = $1;
    SELECT delete_cascade_member(m.id)
           FROM members m WHERE m.account_id = $1;

    DELETE FROM accounts WHERE id = $1;
$$ LANGUAGE SQL;
