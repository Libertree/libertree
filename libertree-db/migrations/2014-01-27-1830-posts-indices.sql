CREATE INDEX posts_by_time_created ON posts(time_created);
CREATE INDEX posts_by_last_activity_time ON posts(GREATEST(time_commented,time_updated));
