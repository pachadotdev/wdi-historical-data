# Create SQL dump and restore

pg_dump wdi > wdi.dump
exit
sudo mv /var/lib/postgres/wdi.dump ./wdi.dump
sudo chmod 777 ./wdi.dump

zip -s 2g wdi.zip wdi.dump
rm wdi.dump
