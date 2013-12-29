ALTER TABLE pools DROP CONSTRAINT valid_spring_url_name;
ALTER TABLE pools ADD CONSTRAINT valid_spring_url_name CHECK ( spring_url_name ~ '^[a-z_-]+$' );
