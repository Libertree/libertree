ALTER TABLE profiles ADD CONSTRAINT valid_name_display CHECK (
    name_display ~ '^[a-zA-Z0-9,_.&():;~"'' -]+$'
);
