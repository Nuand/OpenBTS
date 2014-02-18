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

AM_CPPFLAGS = $(STD_DEFINES_AND_INCLUDES)
AM_CXXFLAGS = -Wall -g -pthread

noinst_PROGRAMS = \
	OpenBTS 

OpenBTS_SOURCES = OpenBTS.cpp
OpenBTS_LDADD = \
	$(GLOBALS_LA) \
	$(COMMON_LA) \
	$(HLR_LA) \
	$(SIP_LA) \
	$(GSM_LA) \
	$(TRX_LA) \
	$(CONTROL_LA) \
	$(SMS_LA) \
	$(CLI_LA) \
	$(OSIP_LIBS) \
	$(ORTP_LIBS)


EXTRA_DIST = \
	OpenBTS.config.example \
	runloop.sh

