DROP INDEX IF EXISTS posts_search_index;
DROP INDEX IF EXISTS comments_search_index;
DROP INDEX IF EXISTS profiles_search_index;

CREATE INDEX posts_search_index    ON posts    USING
  gin((to_tsvector('simple', text) || to_tsvector('english', text)));
CREATE INDEX comments_search_index ON comments USING
  gin((to_tsvector('simple', text) || to_tsvector('english', text)));
CREATE INDEX profiles_search_index ON profiles USING
  gin((to_tsvector('simple', description) || to_tsvector('english', description)));
