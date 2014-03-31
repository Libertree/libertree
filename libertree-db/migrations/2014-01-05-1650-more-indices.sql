CREATE INDEX jobs_by_task ON jobs(task);
CREATE INDEX chat_messages_by_receiving_member ON chat_messages(to_member_id);
CREATE INDEX notifications_by_account ON notifications(account_id);
CREATE INDEX url_expansions_by_url_short ON url_expansions(url_short);
