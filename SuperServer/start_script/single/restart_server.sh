#!/bin/sh

###########################
# This the script to restart
# the super star server 
# including logic and dispatch
# server
###########################

##restart logic server
restart_logic_server(){
	echo "start to restart the logic nodes =====>"
	cd ../../tower/tower_server/bin
	echo "restarting masterctl ==>"
	./masterctl restart
	sleep 3

	echo "restarting databasectl ==>"
	./databasectl restart
	sleep 3

	echo "restarting cachectl ==>"
	./cachectl restart
	sleep 3

	echo "restarting logctl ==>"
	./logctl restart
	sleep 3

	echo "restarting mailctl ==>"
	./mailctl restart
	sleep 3

	echo "restarting rankingctl ==>"
	./rankingctl restart
	sleep 3

	echo "restarting gatewayctl ==>"
	./gatewayctl restart
	sleep 3

	echo "restarting adminserverctl ==>"
	./adminserverctl restart
	sleep 3

	echo "restarting httpprocctl ==>"
	./httpprocctl restart
	sleep 3

	echo "restarting sessionctl ==>"
	./sessionctl restart
	sleep 3
	echo "DONE"
}

##restart dispatch server
restart_dispatch_server(){
	echo "Start to restart the dispatch nodes ==> "
	cd ../../tower_dispatch/tower_dispatch_server/bin
	echo "restarting masterctl ==>"
	./masterctl restart
	sleep 3

	echo "restarting databasectl ==>"
	./databasectl restart
	sleep 3

	echo "restarting loginctl ==>"
	./loginctl restart
	sleep 3

	echo "restarting dispatchctl ==>"
	./dispatchctl restart
	sleep 3

	echo "restarting tokenctl ==>"
	./tokenctl restart
	sleep 3
	echo "DONE"
}

##print_usage 
print_usage(){
	echo "usage=====================>
			./restart_server dispatch
			./restart_server logic
		 "
}

case "$1" in
	logic)
		restart_logic_server
		exit 1
		;;
	dispatch)
		restart_dispatch_server
		exit 1
		;;
	*)
		print_usage
		exit 1
		;;
esac
