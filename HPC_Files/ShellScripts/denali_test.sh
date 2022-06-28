#!/bin/bash

# Modules are already loaded in the submission script
module list

# start postgresql
pg_ctl start -D test_db

# Run the test
date
Rscript denali_test.R
date

# Clean up after ourselves
pg_ctl stop -D test_db
