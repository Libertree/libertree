CREATE TABLE nodes (
    id            SERIAL
  , address       VARCHAR(256) NOT NULL
  , title         VARCHAR(1024)
  , description   VARCHAR(1024)
  , access_model  VARCHAR(32)
  , server_id     INTEGER
  , PRIMARY KEY(id)
  , CONSTRAINT nodes_valid_access_model CHECK (
      server_id IS NULL AND (
           access_model = 'open'
        OR access_model = 'presence'
        OR access_model = 'roster'
        OR access_model = 'authorize'
        OR access_model = 'whitelist'
      ) OR server_id IS NOT NULL AND access_model IS NULL
    )
);
COMMENT ON TABLE nodes IS
'A publish/subscribe endpoint, the core element of a pubsub system.  Messages can be published to a node by publishers (i.e. users with a publisher affiliation) and will be received by subscribers.  Exactly how a node behaves when an attempt is made to publish to it or subscribe to updates depends on the affiliation the actor has with the node.';
COMMENT ON COLUMN nodes.address IS
'The NodeID part of the JID identifying the node.';
COMMENT ON COLUMN nodes.title IS
'The name of the node (optional).';
COMMENT ON COLUMN nodes.description IS
'A description of the node (optional).';
COMMENT ON COLUMN nodes.access_model IS
'One of the access models described in XEP-0060#4.5.  Only stored for local nodes, i.e. those without a server_id.';
COMMENT ON COLUMN nodes.server_id IS
'The id of the server hosting the node, if not local.';


CREATE TABLE node_subscriptions (
    id            SERIAL
  , sub_id        VARCHAR(1024)
  , jid           VARCHAR(1024)
  , account_id    INTEGER REFERENCES accounts
  , node_id       INTEGER REFERENCES nodes
  , state         VARCHAR(16)
  , CONSTRAINT node_subscriptions_valid_status CHECK (
         state = 'none'
      OR state = 'pending'
      OR state = 'unconfigured'
      OR state = 'subscribed'
      OR state IS NULL
    )
  , CONSTRAINT node_subscriptions_valid_subscriber CHECK (
      (account_id IS NOT NULL AND jid IS NULL)
      OR (account_id IS NULL AND jid IS NOT NULL)
    )
  , UNIQUE (node_id, jid)
);
COMMENT ON TABLE node_subscriptions IS
'A subscription of a local or remote user (identified by a JID) to a local node.';
COMMENT ON COLUMN node_subscriptions.sub_id IS
'A subscription identifier given to an approved subscriber.';
COMMENT ON COLUMN node_subscriptions.jid IS
'The JID of the subscriber.  For local accounts this will be NULL.';
COMMENT ON COLUMN node_subscriptions.account_id IS
'The account id of the local subscriber account.  For local accounts only, NULL for remote subscriptions.';
COMMENT ON COLUMN node_subscriptions.node_id IS
'The id of the node to which the subscription applies.';
COMMENT ON COLUMN node_subscriptions.state IS
'One of the subscription states defined in XEP-0060#4.2 (for subscriptions to local nodes) or NULL (for subscriptions to remote nodes).';


CREATE TABLE affiliations (
    id            SERIAL
  , jid           VARCHAR(1024)
  , account_id    INTEGER REFERENCES accounts
  , affiliation   VARCHAR(32)
  , node_id       INTEGER REFERENCES nodes
  , CONSTRAINT node_affiliations_valid_affiliation CHECK (
         affiliation = 'owner'
      OR affiliation = 'publisher'
      OR affiliation = 'publish-only'
      OR affiliation = 'member'
      OR affiliation = 'none'
      OR affiliation = 'outcast'
    )
  , CONSTRAINT affiliations_valid_actor CHECK (
      (account_id IS NOT NULL AND jid IS NULL)
      OR (account_id IS NULL AND jid IS NOT NULL)
    )
  , UNIQUE (node_id, jid)
);
COMMENT ON TABLE affiliations IS
'An affiliation defines the role a JID has on a local or remote node.';
COMMENT ON COLUMN affiliations.jid IS
'The JID of the person affiliated with the node.  For local accounts this will be NULL.';
COMMENT ON COLUMN affiliations.account_id IS
'The account id of the local account for which this affiliation is valid.  For local accounts only.';
COMMENT ON COLUMN affiliations.node_id IS
'The id of the node to which the subscription applies.';
COMMENT ON COLUMN affiliations.affiliation IS
'The affiliation type of the JID with the node as defined in XEP-0060#4.1';


CREATE TABLE nodes_posts (
    node_id        INTEGER
  , post_id        INTEGER
  , time_published TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
  , UNIQUE (node_id, post_id)
);
COMMENT ON TABLE nodes_posts IS
'A join table that associates posts with nodes.';
COMMENT ON COLUMN nodes_posts.time_published IS
'The time at which the post was added to the node, not the time the post was first created.';
