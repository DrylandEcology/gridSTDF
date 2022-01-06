CREATE TABLE tbl_ppt
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY ( INCREMENT 1 START 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1 ),
    run_fk integer,
    site_fk integer NOT NULL,
    year integer NOT NULL,
    day integer,
    ppt numeric,
    CONSTRAINT tbl_ppt_pkey PRIMARY KEY (id)
);
