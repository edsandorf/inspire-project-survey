-- !preview conn=DBI::dbConnect(RSQLite::SQLite())
CREATE TABLE data_table (
choice_data	BLOB,
search_data	BLOB,
consideration_data	BLOB,
time_delay_data	BLOB,
time_data	BLOB,
question_data	BLOB,
survey_id TEXT /* End last line without a comma */
);