ALTER TABLE accounts ALTER COLUMN locale SET DEFAULT 'en_GB';
UPDATE accounts SET locale = DEFAULT WHERE locale IS NULL;
ALTER TABLE accounts ALTER COLUMN locale SET NOT NULL;
