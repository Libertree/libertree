ALTER TABLE pools ADD COLUMN spring_url_name VARCHAR(64);
ALTER TABLE pools ADD CONSTRAINT unique_spring_url_name UNIQUE (spring_url_name, member_id);
ALTER TABLE pools ADD CONSTRAINT valid_spring_url_name CHECK ( spring_url_name ~ '[a-z_-]+' );
