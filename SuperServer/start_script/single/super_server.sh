#!/bin/sh

echo "Start to install Super server environment,TWO steps in total...."
echo "First step ==>"
echo
cd ../../tower_dispatch/tower_dispatch_server/
chmod 777 emake bin/*
dos2unix bin/*
./emake clean
./emake get-deps
./emake build
echo
cd bin/
echo "master_sysinit"
./masterctl sysinit 
sleep 10
echo "exit sysinit"
echo "master_start"
./masterctl start
sleep 2
./masterctl status

echo "database_sysinit"
./databasectl sysinit 
sleep 10
echo "exit sysinit"
echo "database_start"
./databasectl start
sleep 2
./databasectl status

echo "login_sysinit"
./loginctl sysinit  
sleep 10
echo "exit sysinit"
echo "login_start"
./loginctl start
sleep 2
./loginctl status

echo "dispatch_sysinit"
./dispatchctl sysinit   
sleep 10
echo "exit sysinit"
echo "dispatch_start"
./dispatchctl start
sleep 2
./dispatchctl status

echo "token_sysinit"
./tokenctl sysinit   
sleep 10
echo "exit sysinit"
echo "token_start"
./tokenctl start
sleep 2
./tokenctl status
echo "Step 1 Finished"
echo


echo "Second step ==>"

cd ../../../tower/tower_server
chmod 777 emake bin/*
dos2unix bin/*
./emake clean
./emake get-deps
./emake build
echo
cd bin/
echo "master_sysinit"
./masterctl sysinit    
sleep 10
echo "exit sysinit"
echo "master_start"
./masterctl start
sleep 2
./masterctl status

echo "database_sysinit"
./databasectl sysinit    
sleep 10
echo "exit sysinit"
echo "database_start"
./databasectl start
sleep 10
./databasectl status

echo "cache_sysinit"
./cachectl sysinit    
sleep 10
echo "exit sysinit"
echo "cache_start"
./cachectl start
sleep 5
./cachectl status

echo "log_sysinit"
./logctl sysinit     
sleep 10
echo "exit sysinit"
echo "log_start"
./logctl start
sleep 2
./logctl status

echo "mail_sysinit"
./mailctl sysinit     
sleep 10
echo "exit sysinit"
echo "mail_start"
./mailctl start
sleep 2
./mailctl status

echo "ranking_sysinit"
./rankingctl sysinit     
sleep 10
echo "exit sysinit"
echo "ranking_start"
./rankingctl start
sleep 2
./rankingctl status

./gatewayctl sysinit     
sleep 10
echo "exit sysinit"
echo "gateway_start"
./gatewayctl start
sleep 2
./gatewayctl status

echo "adminserver_sysinit"
./adminserverctl sysinit     
sleep 10
echo "exit sysinit"
echo "adminserver_start"
./adminserverctl start
sleep 2
./adminserverctl status

echo "http_proc_sysinit"
./httpprocctl sysinit     
sleep 10
echo "exit sysinit"
echo "http_proc_start"
./httpprocctl start
sleep 2
./httpprocctl status

echo "session_sysinit"
./sessionctl sysinit     
sleep 10
echo "exit sysinit"
echo "session_start"
./sessionctl start
sleep 2
./sessionctl status

echo "Step 2 Finished"
echo "DONE"


