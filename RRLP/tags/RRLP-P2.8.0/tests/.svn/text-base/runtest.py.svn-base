#!/usr/bin/python

from __future__ import with_statement

import sys
import os
import struct

executable='rrlpconverter'

def binary(x):
    n = bin(x)[2:]
    if len(n)%8 != 0:
        n = '0'*(8 - len(n)%8) + n
    return n

def hex2str(h):
    """ h is a string of hexadecimal chars
    returns a string whose hexadecimal representation is h """
    assert(len(h)%2==0)
    return ''.join(chr(int(h[i:i+2],16)) for i in xrange(0,len(h),2))

def bin2str(b):
    assert(len(b)%8==0)
    return ''.join(chr(int(b[i:i+8],2)) for i in xrange(0,len(b),8))

def str2hex(s):
    return ''.join('%02X' % ord(c) for c in s)

def main():
    s = ''.join(sys.argv[1:])
    if set(s) == set(['0','1']):
        d = bin2str(s)
        b = s
    else:
        d = hex2str(s)
        b = binary(int(sys.argv[-1], 16))
    with open('temp.per', 'wb+') as fd:
        fd.write(d)
    print "rrreccceoooooOmmaaaaaaappmmmu___"
    print b
    print str2hex(d)
    os.system('%s -oxer -iper temp.per' % executable)

def findnull():
    for i in [1,2,3]:
        pass

if __name__ == '__main__':
    main()

