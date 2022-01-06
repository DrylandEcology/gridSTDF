CREATE TABLE tbl_sites
(
    id integer NOT NULL,
    latitude numeric,
    longitude numeric,
    weather_label character(30) COLLATE pg_catalog."default",
    CONSTRAINT "tbl_sites_pkey" PRIMARY KEY (id)
);