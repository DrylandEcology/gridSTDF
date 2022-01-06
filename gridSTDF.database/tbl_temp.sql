CREATE TABLE tbl_temp
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY ( INCREMENT 1 START 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1 ),
    run_fk integer,
    site_fk integer NOT NULL,
    year integer NOT NULL,
    day integer,
    temp numeric,
    CONSTRAINT tbl_temp_pkey PRIMARY KEY (id)
);