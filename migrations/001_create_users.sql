CREATE TABLE users (
       id serial primary key,
       name text not null,
       avatar_url text not null,
       email text,
       github_login text not null,
       hireable boolean not null
);
