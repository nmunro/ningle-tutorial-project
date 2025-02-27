#lang cl-yesql/postmodern

-- name: create-user @execute
-- Returns nothing
INSERT INTO users (first_name, last_name, email, username, password) VALUES (?, ?, ?, ?, ?)

-- name: all-users @rows
-- Returns all users
SELECT id, first_name, last_name, email, username, password FROM users

-- name: get-user-by-id @values
-- Returns a single user
SELECT * FROM users WHERE id = ?

-- name: get-user-by-email @values
-- Returns a single user
SELECT * FROM users WHERE email = ?

-- name: update-user @execute
-- Returns nothing
UPDATE users SET first_name = ?, last_name = ?, email = ?, username = ?, password = ? WHERE id = ?
