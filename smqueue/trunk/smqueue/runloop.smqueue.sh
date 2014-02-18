#!/bin/sh

# A script to restart and just keep smqueue running.
while true; do killall smqueue; sleep 2; ./smqueue; done
