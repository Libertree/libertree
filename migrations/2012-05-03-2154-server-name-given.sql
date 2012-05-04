ALTER TABLE servers ADD COLUMN name_given VARCHAR(32) CONSTRAINT valid_name_given CHECK ( name_given ~ '^[^\s!"#$%&()*+,/:;<=>?\[\\\]^_`~]+$' );
