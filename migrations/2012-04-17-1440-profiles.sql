CREATE TABLE profiles(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , time_updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , member_id INTEGER NOT NULL REFERENCES members(id)
    , name_display VARCHAR(128)
    , description VARCHAR(2048)
    , PRIMARY KEY(id)
);

