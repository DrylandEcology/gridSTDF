CREATE TABLE tbl_vwc
(
    "Id" integer NOT NULL GENERATED ALWAYS AS IDENTITY ( INCREMENT 1 START 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1 ),
    site_fk integer NOT NULL,
    run_fk integer,
    year integer NOT NULL,
    day integer NOT NULL,
    shallow numeric,
    intermediate numeric,
    deep numeric,
    CONSTRAINT "tblSoilMoisture_pkey" PRIMARY KEY ("Id"),
    CONSTRAINT runs_soilmoisture_fk FOREIGN KEY (run_fk)
        REFERENCES public."tbl_runs" ("Id") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT sites_soilmoisture_key FOREIGN KEY (site_fk)
        REFERENCES public.tbl_sites (id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);