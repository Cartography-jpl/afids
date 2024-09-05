#!/bin/csh

mysqladmin -hlocalhost -uroot create afidsdb
mysql -hlocalhost -uroot < grant.sql
mysql -hlocalhost -uafidsuser -pafidspwd afidsdb < makeUsers.sql
