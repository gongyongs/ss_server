#!/bin/sh

###########################
# Configuration
###########################
SHOW_VERIFY=true
echo "Start Killing the nodes ==>"
show_verify(){
	if [ "$SHOW_VERIFY" = "true" ];then
		echo "you sure to $1 the nodes?:yes/no"
		read option
		case $option in
			"Yes");;
			"yes");;
			*)echo "operation cancel";exit 1;;
		esac
	fi
}
show_verify "kill"
#|xargs kill -9
ps -A|grep beam|awk '{print $1}'|xargs kill -9
echo "DONE"
echo 