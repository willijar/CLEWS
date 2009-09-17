#! /bin/sh
#
# skeleton	example file to build /etc/init.d/ scripts.
#		This file should be used to construct scripts for /etc/init.d.
#
#		Written by Miquel van Smoorenburg <miquels@cistron.nl>.
#		Modified for Debian GNU/Linux
#		by Ian Murdock <imurdock@gnu.ai.mit.edu>.
#
# Version:	@(#)skeleton  1.9.1  08-Apr-2002  miquels@cistron.nl
#

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/home/willijar/dev/lisp/src/www/aston/start
NAME=EWAS
DESC="Educational Web Application Server"

test -x $DAEMON || exit 0

set -e

case "$1" in
  start)
	echo -n "Starting $DESC: $NAME"
	start-stop-daemon --start --quiet --pidfile /var/run/$NAME.pid \
		--exec $DAEMON
	echo "."
	;;
  stop)
	echo -n "Stopping $DESC: $NAME "
	start-stop-daemon --stop --quiet --pidfile /var/run/$NAME.pid \
		--exec $DAEMON
	echo "."
	;;
  #reload|force-reload)
#	echo -n "Reloading $DESC configuration..."
#	start-stop-daemon --stop --signal 1 --quiet --pidfile \
#		/var/run/$NAME.pid --exec $DAEMON
#	 echo "done."
  restart|force-reload)
	echo -n "Restarting $DESC: $NAME"
	start-stop-daemon --stop --quiet --pidfile \
		/var/run/$NAME.pid --exec $DAEMON
	sleep 1
	start-stop-daemon --start --quiet --pidfile \
		/var/run/$NAME.pid --exec $DAEMON
	echo "."
	;;
  *)
	N=/etc/init.d/$NAME
	# echo "Usage: $N {start|stop|restart|reload|force-reload}" >&2
	echo "Usage: $N {start|stop|restart|force-reload}" >&2
	exit 1
	;;
esac

exit 0
