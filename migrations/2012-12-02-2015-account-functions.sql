CREATE FUNCTION account_subscribed_to_post(account_id INTEGER, post_id INTEGER) RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT 1 FROM post_subscriptions WHERE account_id = $1 AND post_id = $2
    );
$$ LANGUAGE SQL STABLE;

CREATE FUNCTION account_has_contact_list_by_name_containing_member(
  account_id INTEGER,
  contact_list_name VARCHAR(64),
  member_id INTEGER
) RETURNS BOOLEAN AS $$
    SELECT EXISTS(
      SELECT 1
      FROM
          contact_lists cl
        , contact_lists_members clm
      WHERE
        cl.account_id = $1
        AND cl.name = $2
        AND clm.contact_list_id = cl.id
        AND clm.member_id = $3
    );
$$ LANGUAGE SQL STABLE;
