#!/usr/bin/env python

# yet to know how to do this fast in erlang.
import os

def bin2str(b):
    assert(len(b)%8==0)
    return ''.join(chr(int(b[i:i+8],2)) for i in xrange(0,len(b),8))

def str2hex(s):
    return ''.join('%02X' % ord(c) for c in s)

lines = os.popen("cat ../../apps/test.out | grep 'RRLP.*0000' | gawk '{print $8}' | sort | uniq").readlines()
for i, l in enumerate(lines):
    s = bin2str(l.split('(')[1].split(')')[0])
    PD = ord(s[0]) & 0xf
    if PD != 6: continue # Not an RR packet, can't be an RRLP packet.
    if len(s) < 5: continue # Not large enough
    s = s[4:] # remove L3 header, leave RRLP
    outfile = '../tests/log_%03d.per' % i
    print "%s = %s" % (outfile, str2hex(s))
    fd = open(outfile, 'wb+')
    fd.write(s)
    fd.close()
