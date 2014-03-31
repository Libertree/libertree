ALTER TABLE messages ADD COLUMN deleted BOOLEAN DEFAULT false;

COMMENT ON COLUMN messages.deleted IS
'A flag that is set if the sender is a local member and has marked the message for deletion.';

ALTER TABLE message_recipients ADD COLUMN deleted BOOLEAN DEFAULT false;

COMMENT ON COLUMN message_recipients.deleted IS
'A flag that is set if the recipient is a local member and has marked the message for deletion.';
