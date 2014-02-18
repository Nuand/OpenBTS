/*
 * SMSC.h - SMS Center implementation for OpenBTS.
 * Written by Alexander Chemeris, 2010.
 *
 * Copyright 2010 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * See the COPYING file in the main directory for details.
 */

#ifndef SMSC_H
#define SMSC_H

#include "smqueue.h"

#undef WARNING

using namespace SMqueue;

/** Unhex and parse msgtext to RPData struct.
  @param msgttext RPData encoded into hex-string.
  @return Pointer to parsed RPData or NULL on failure.
*/
RPData *hex2rpdata(const char *msgtext);

enum short_code_action shortcode_smsc(const char *imsi, const char *msgtext,
                                      short_code_params *scp);
bool pack_sms_for_delivery(short_msg_p_list::iterator &smsg);

#endif
