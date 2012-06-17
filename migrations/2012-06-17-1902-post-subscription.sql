CREATE TABLE post_subscriptions (
      id SERIAL PRIMARY KEY
    , account_id INTEGER NOT NULL REFERENCES accounts(id)
    , post_id INTEGER NOT NULL REFERENCES posts(id)
);

COMMENT ON TABLE post_subscriptions IS
'Accounts subscribed to posts for notifications about new comments.';
