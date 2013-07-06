ALTER TABLE jobs ADD COLUMN retry_reason VARCHAR(4096);
COMMENT ON COLUMN jobs.retry_reason IS
'When the job failed and is being retried this field should hold the reason for scheduling a retry.';
