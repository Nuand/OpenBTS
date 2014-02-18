/**@file Global system parameters. */
/*
* Copyright 2008, 2009, 2010 Free Software Foundation, Inc.
*
* This software is distributed under the terms of the GNU Affero Public License.
* See the COPYING file in the main directory for details.
*
* This use of this software may be subject to additional restrictions.
* See the LEGAL file in the main directory for details.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "config.h"
#include <Globals.h>
#include <CLI.h>


const char* gOpenBTSWelcome =
	//23456789123456789223456789323456789423456789523456789623456789723456789
	"OpenBTS, Copyright 2008-2010 Free Software Foundation, Inc.\n"
	"Release " VERSION " built " __DATE__ "\n"
	"\"OpenBTS\" is a trademark of Kestrel Signal Processing, Inc.,\n"
	"regsitered with the US Patent and Trademark Office.\n"
	"\nContributors:\n"
	"  Kestrel Signal Processing, Inc.:\n"
	"    David Burgess, Harvind Samra, Raffi Sevlian, Roshan Baliga\n"
	"  GNU Radio:\n"
	"    Johnathan Corgan\n"
	"  Others:\n"
	"    Anne Kwong, Jacob Appelbaum, Joshua Lackey, Alon Levy\n"
	"    Alexander Chemeris, Alberto Escudero-Pascual\n"
	"Incorporated GPL libraries and components:\n"
	"  libosip2, liportp2"
#ifdef HAVE_LIBREADLINE
	", readline"
#endif
	"\n"
	"\nThis program comes with ABSOLUTELY NO WARRANTY.\n"
	"\nThis is free software; you are welcome to redistribute it\n"
	"under the terms of AGPLv3.\n"
	"Please see the COPYING file in the source code for information\n"
	"about the AGPLv3 license and recommended procedures for compliance\n"
	"with the Affero requirements of that license.\n"
	"\nUse of this software may be subject to other legal restrictions,\n"
	"including patent licsensing and radio spectrum licensing.\n"
	"All users of this software are expected to comply with applicable\n"
	"regulations and laws.\n"
;


CommandLine::Parser gParser;
