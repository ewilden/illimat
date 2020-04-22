create extension citext;
create extension pgcrypto;
create table auths (
  id bigserial primary key not null,
  pass text not null,
  username citext not null unique,
  username_verification_code text not null,
  is_username_verified boolean not null
);
