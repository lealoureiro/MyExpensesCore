
CREATE KEYSPACE finances_core WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

use finances_core;

CREATE TABLE users (
    user_id uuid PRIMARY KEY,
    username varchar,
    password varchar,
    name varchar
);

CREATE TABLE users_by_username (
    username varchar PRIMARY KEY,
    user_id uuid
);

CREATE TABLE accounts (
    account_id uuid PRIMARY KEY,
    name varchar,
    account_type text,
    start_balance bigint,
    currency varchar,
    user_id uuid,
);

CREATE TABLE user_by_account (
    account_id uuid PRIMARY KEY,
    user_id uuid
);

CREATE TABLE accounts_by_user (
    user_id uuid,
    account_id uuid,
    PRIMARY KEY (user_id, account_id)
);

CREATE TABLE transactions (
    transaction_id uuid,
    description text,
    account_id uuid,
    category text,
    sub_category text,
    date timestamp,
    amount bigint,
    external_reference text,
    PRIMARY KEY (account_id, date, transaction_id)
) WITH CLUSTERING ORDER BY (date DESC);

CREATE TABLE tags_by_transaction (
      transaction_id uuid,
      tag text,
      PRIMARY KEY (transaction_id, tag)
);

CREATE TABLE transactions_by_tag (
    tag text,
    transaction_id uuid,
    PRIMARY KEY (tag,transaction_id)
);

CREATE INDEX search_by_category ON transactions (category);
CREATE INDEX search_by_sub_category ON transactions (sub_category);

CREATE TABLE category (
    user_id uuid,
    name text,
    PRIMARY KEY (user_id, name)
);

CREATE TABLE sub_category (
    user_id uuid,
    category_name text,
    name text,
    PRIMARY KEY (user_id, category_name, name)
);

CREATE TABLE transactions_by_external (
	external_reference text PRIMARY KEY,
	transaction_id uuid
);
