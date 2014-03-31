CREATE TABLE jobs(
      id SERIAL
    , time_created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , queue VARCHAR(64) NOT NULL DEFAULT 'default'
    , task VARCHAR(64) NOT NULL
    , params VARCHAR(4096)
    , time_to_start TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    , pid INTEGER
    , time_started TIMESTAMP WITH TIME ZONE
    , time_finished TIMESTAMP WITH TIME ZONE
    , tries INTEGER NOT NULL DEFAULT 0
    , PRIMARY KEY(id)
);

COMMENT ON TABLE jobs IS
'For delayed or background jobs that would be queued up for processing.';
COMMENT ON COLUMN jobs.pid IS
'The ID of the process which has reserved the job for processing.  A sort of mutex.';
