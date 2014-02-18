#!/bin/bash
# $1 is IP number
# $2 is port

cat MT-SMS-Example.txt | nc -u $1 $2
