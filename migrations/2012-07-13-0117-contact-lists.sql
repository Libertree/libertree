CREATE TABLE contact_lists (
      id SERIAL PRIMARY KEY
    , name VARCHAR(64) NOT NULL
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
);

CREATE TABLE contact_lists_members (
      contact_list_id INTEGER NOT NULL REFERENCES contact_lists(id)
    , member_id INTEGER NOT NULL REFERENCES members(id)
);
