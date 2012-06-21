DELETE FROM
    post_subscriptions ps1
WHERE EXISTS (
    SELECT 1
    FROM post_subscriptions ps2
    WHERE
        ps2.account_id = ps1.account_id
        AND ps2.post_id = ps1.post_id
        AND ps2.id <> ps1.id
    )
;

ALTER TABLE post_subscriptions ADD CONSTRAINT post_subscriptions_account_id_post_id_key UNIQUE (account_id, post_id);
