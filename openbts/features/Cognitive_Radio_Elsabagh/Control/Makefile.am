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
AM_CXXFLAGS = -Wall -O3

EXTRA_DIST = README.Control

noinst_LTLIBRARIES = libcontrol.la

libcontrol_la_SOURCES = \
	CallControl.cpp \
	SMSControl.cpp \
	ControlCommon.cpp \
	MobilityManagement.cpp \
	RadioResource.cpp \
	DCCHDispatch.cpp \
	CollectMSInfo.cpp \
    RRLPQueryController.cpp

# TODO - move CollectMSInfo.cpp and RRLPQueryController.cpp to RRLP directory.

noinst_HEADERS = \
	ControlCommon.h \
	CollectMSInfo.h \
	RRLPQueryController.h
