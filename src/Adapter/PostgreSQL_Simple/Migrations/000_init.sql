/*

I'm not using a migration system for now.

citext extension allows to match email in a case insensitive way.

 */

CREATE EXTENSION IF NOT EXISTS citext;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

DROP TABLE IF EXISTS users;

CREATE TABLE users (
  user_id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  email CITEXT NOT NULL UNIQUE CHECK (TRIM(email) <> '' AND LENGTH(email) < 100) UNIQUE,
  pw_hash CHAR(60) NOT NULL UNIQUE,
  verification_code VARCHAR(255) NOT NULL UNIQUE,
  verified_at TIMESTAMP WITH TIME ZONE NULL CHECK (verified_at < current_timestamp), -- TODO: test with `<=` and `<`
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
);