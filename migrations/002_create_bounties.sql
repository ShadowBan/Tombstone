CREATE TABLE bounties (
       id serial primary key,
       description text not null,
       claimed boolean not null,
       compensation_schedule text not null,
       compensation_magnitude integer not null CHECK (compensation_magnitude >= 0),
       user_id integer not null UNIQUE REFERENCES users (id)
);
