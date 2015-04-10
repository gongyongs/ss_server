#!/bin/sh

###########################
# Configuration
###########################

echo "Start to stop all the nodes in single server ==> "
echo
cd ../../tower_dispatch/tower_dispatch_server/bin
echo "Step 1 ==>"
echo "stoping masterctl ==>"
./masterctl stop
sleep 3

echo "stoping databasectl ==>"
./databasectl stop
sleep 3

echo "stoping loginctl ==>"
./loginctl stop
sleep 3

echo "stoping dispatchctl ==>"
./dispatchctl stop
sleep 3

echo "stoping tokenctl ==>"
./tokenctl stop
sleep 3

echo "Step 1 Finished"
echo



echo "step 2 ==>"
cd ../../../tower/tower_server/bin
echo "stoping masterctl ==>"
./masterctl stop
sleep 3

echo "stoping databasectl ==>"
./databasectl stop
sleep 3

echo "stoping cachectl ==>"
./cachectl stop
sleep 3

echo "stoping logctl ==>"
./logctl stop
sleep 3

echo "stoping mailctl ==>"
./mailctl stop
sleep 3

echo "stoping rankingctl ==>"
./rankingctl stop
sleep 3

echo "stoping gatewayctl ==>"
./gatewayctl stop
sleep 3

echo "stoping adminserverctl ==>"
./adminserverctl stop
sleep 3

echo "stoping httpprocctl ==>"
./httpprocctl stop
sleep 3

echo "stoping sessionctl ==>"
./sessionctl stop
sleep 3

echo "Step 2 Finished"
echo "DONE"