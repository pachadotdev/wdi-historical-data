# Create SQL dump and restore

pg_dump -Fc wdi -f wdi.dump
zip -s 1g wdi.zip wdi.dump
rm wdi.dump
