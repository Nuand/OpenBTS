/*
* Copyright 2011 Free Software Foundation, Inc.
*
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

/*
 * defines for the VRQ_SPI_READ and VRQ_SPI_WRITE commands
 *
 * SPI == "Serial Port Interface".  SPI is a 3 wire bus plus a
 * separate enable for each peripheral.  The common lines are SCLK,
 * SDI and SDO.  The FX2 always drives SCLK and SDI, the clock and
 * data lines from the FX2 to the peripheral.  When enabled, a
 * peripheral may drive SDO, the data line from the peripheral to the
 * FX2.
 *
 * The SPI_READ and SPI_WRITE commands are formatted identically.
 * Each specifies which peripherals to enable, whether the bits should
 * be transmistted Most Significant Bit first or Least Significant Bit
 * first, the number of bytes in the optional header, and the number
 * of bytes to read or write in the body.
 *
 * The body is limited to 64 bytes.  The optional header may contain
 * 0, 1 or 2 bytes.  For an SPI_WRITE, the header bytes are
 * transmitted to the peripheral followed by the the body bytes.  For
 * an SPI_READ, the header bytes are transmitted to the peripheral,
 * then len bytes are read back from the peripheral.
 */

/*
 * SPI_FMT_* goes in wIndexL
 */
#define SPI_FMT_xSB_MASK        (1 << 7)
#  define       SPI_FMT_LSB     (1 << 7)        // least signficant bit first
#  define       SPI_FMT_MSB     (0 << 7)        // most significant bit first
#define SPI_FMT_HDR_MASK        (3 << 5)
#  define       SPI_FMT_HDR_0   (0 << 5)        // 0 header bytes
#  define       SPI_FMT_HDR_1   (1 << 5)        // 1 header byte
#  define       SPI_FMT_HDR_2   (2 << 5)        // 2 header bytes

/*
 * SPI_ENABLE_*  goes in wIndexH
 *
 * For the software interface, the enables are active high.
 * For reads, it's an error to have more than one enable set.
 *
 * [FWIW, the hardware implements them as active low.  Don't change the
 * definitions of these.  They are related to usrp_rev1_regs.h]
 */
#define SPI_ENABLE_FPGA         0x01    // select FPGA
#define SPI_ENABLE_CODEC_A      0x02    // select AD9862 A
#define SPI_ENABLE_CODEC_B      0x04    // select AD9862 B
#define SPI_ENABLE_reserved     0x08
#define SPI_ENABLE_TX_A         0x10    // select d'board TX A
#define SPI_ENABLE_RX_A         0x20    // select d'board RX A
#define SPI_ENABLE_TX_B         0x40    // select d'board TX B
#define SPI_ENABLE_RX_B         0x80    // select d'board RX B

/*
 * If there's one header byte, it goes in wValueL.
 *
 * If there are two header bytes, they go in wValueH | wValueL.
 * The transmit order of the bytes (and bits within them) is 
 * determined by SPI_FMT_*SB
 */

