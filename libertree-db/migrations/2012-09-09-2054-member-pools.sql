ALTER TABLE pools ADD COLUMN member_id INTEGER REFERENCES members(id);
UPDATE
    pools
SET
    member_id = m.id
FROM
    members m
WHERE
    m.account_id = pools.account_id
;
ALTER TABLE pools ALTER COLUMN member_id SET NOT NULL;
ALTER TABLE pools DROP COLUMN account_id;
ALTER TABLE pools ADD CONSTRAINT pools_member_id_name_key UNIQUE (member_id, name);
