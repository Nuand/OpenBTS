#
# Copyright 2009 Free Software Foundation, Inc.
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
AM_CXXFLAGS = -Wall -lpthread

EXTRA_DIST = \
	README.HLR \
	maketables.sql

noinst_LTLIBRARIES = libHLR.la

libHLR_la_SOURCES = \
	HLR.cpp

noinst_PROGRAMS = \
	HLRTest

noinst_HEADERS = \
	HLR.h

HLRTest_LDADD =  $(HLR_LA) $(COMMON_LA)
HLRTest_SOURCES = HLRTest.cpp

