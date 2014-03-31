DROP TRIGGER IF EXISTS after_insert ON chat_messages;
DROP TRIGGER IF EXISTS after_insert ON comments;
DROP TRIGGER IF EXISTS after_insert ON notifications;


CREATE OR REPLACE FUNCTION notify_new_chat_messages() RETURNS trigger AS $$
BEGIN
       NOTIFY chat_messages;
       RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION notify_new_comments() RETURNS trigger AS $$
BEGIN
       NOTIFY comments;
       RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION notify_new_notifications() RETURNS trigger AS $$
BEGIN
       NOTIFY notifications;
       RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER after_insert AFTER INSERT ON chat_messages FOR EACH STATEMENT EXECUTE PROCEDURE notify_new_chat_messages();
CREATE TRIGGER after_insert AFTER INSERT ON comments      FOR EACH STATEMENT EXECUTE PROCEDURE notify_new_comments();
CREATE TRIGGER after_insert AFTER INSERT ON notifications FOR EACH STATEMENT EXECUTE PROCEDURE notify_new_notifications();
