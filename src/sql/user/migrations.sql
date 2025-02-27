#lang cl-yesql/postmodern

-- name: 0001-users-initial-migration @execute
-- Returns nothing
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE
)

-- name: 0002-users-add-username @execute
-- Returns nothing
ALTER TABLE users ADD COLUMN IF NOT EXISTS username TEXT NOT NULL UNIQUE DEFAULT '<empty>'

-- name: 0003-users-add-password @execute
-- Returns nothing
ALTER TABLE users ADD COLUMN IF NOT EXISTS password TEXT DEFAULT ''
