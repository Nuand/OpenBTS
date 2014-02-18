#
# Copyright 2008 Free Software Foundation, Inc.
#
# This software is distributed under the terms of the GNU Affero Public License.
# See the COPYING file in the main directory for details.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

include $(top_srcdir)/Makefile.common

DESTDIR := 

AM_CPPFLAGS = $(STD_DEFINES_AND_INCLUDES)
AM_CXXFLAGS = -O3 -g -lpthread

EXTRA_DIST = \
	README.smqueue \
	smqueue.config.example

noinst_PROGRAMS = \
	smqueue

noinst_HEADERS = \
	poll.h \
	smnet.h \
	smqueue.h \
	smsc.h \
	diskbackup.h

smqueue_SOURCES = \
	poll.c \
	smcommands.cpp \
	smnet.cpp \
	smqueue.cpp \
	smsc.cpp \
	diskbackup.cpp

smqueue_LDADD = \
	$(GLOBALS_LA) \
	$(SMS_LA) \
	$(GSM_LA) \
	$(COMMON_LA) \
	$(SQLITE_LA) \
	$(SR_LA) \
	$(SIP_LA) \
	$(OSIP_LIBS)

install: smqueue
	mkdir -p "$(DESTDIR)/OpenBTS/"
	install smqueue "$(DESTDIR)/OpenBTS/"
	install runloop.smqueue.sh "$(DESTDIR)/OpenBTS/"
	mkdir -p "$(DESTDIR)/etc/OpenBTS/"
	install smqueue.example.sql "$(DESTDIR)/etc/OpenBTS/"

