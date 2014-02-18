#!/bin/sh

# A script to restart and just keep sipauthserve running.
while true; do killall sipauthserve; sleep 2; ./sipauthserve; done
