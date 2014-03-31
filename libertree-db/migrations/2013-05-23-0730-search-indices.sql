CREATE INDEX posts_search_index    ON posts    USING gin(to_tsvector('simple', text));
CREATE INDEX comments_search_index ON comments USING gin(to_tsvector('simple', text));
CREATE INDEX profiles_search_index ON profiles USING gin(to_tsvector('simple', description));
