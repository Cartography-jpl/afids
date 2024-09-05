#!/usr/bin/tclsh

set usersDir /usr/share/tomcat5/webapps/ROOT/users
for {set i 0} {$i < 10} {incr i} {
    exec -- mkdir -p ${usersDir}/user${i}/upload
    exec -- mkdir -p ${usersDir}/user${i}/wpsoutput
    exec chmod 777 ${usersDir}/user${i}
    exec chmod 777 ${usersDir}/user${i}/upload
    exec chmod 777 ${usersDir}/user${i}/wpsoutput
#    exec -- ln -s /usr/share/tomcat5/webapps/ROOT/users/user${i}/upload /home/user${i}/PreviousInput
#    exec chown user${i} /home/user${i}/PreviousInput
#    exec -- ln -s /usr/share/tomcat5/webapps/ROOT/users/user${i}/wpsoutput /home/user${i}/PreviousOutput
#    exec chown user${i} /home/user${i}/PreviousOutput
}
