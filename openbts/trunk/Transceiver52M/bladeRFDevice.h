/*
 * Copyright 2014 Free Software Foundation, Inc.
 *
 * This software is distributed under multiple licenses; see the COPYING file in
 * the main directory for licensing information for this specific distribuion.
 *
 * This use of this software may be subject to additional restrictions.
 * See the LEGAL file in the main directory for details.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 **/

#ifndef _BLADERF_DEVICE_H_
#define _BLADERF_DEVICE_H_

#include <libbladeRF.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "radioDevice.h"

#include <sys/time.h>
#include <math.h>
#include <string>
#include <iostream>

class bladeRFDevice: public RadioDevice {

private:

  double desiredSampleRate; 	///< the desired sampling rate

  int sps;
  double actualSampleRate;	///< the actual bladeRF sampling rate
  unsigned int decimRate;	///< the bladeRF decimation rate

  unsigned long long samplesRead;	///< number of samples read from bladeRF
  unsigned long long samplesWritten;	///< number of samples sent to bladeRF

  bool started;			///< flag indicates bladeRF has started
  bool skipRx;			///< set if bladeRF is transmit-only.

  static const unsigned int currDataSize_log2 = 21;
  static const unsigned long currDataSize = (1 << currDataSize_log2);
  short *data;
  unsigned long dataStart;
  unsigned long dataEnd;
  TIMESTAMP timeStart;
  TIMESTAMP timeEnd;
  bool isAligned;
  struct bladerf *bdev;

  Mutex writeLock;

  short *currData;		///< internal data buffer when reading from bladeRF
  TIMESTAMP currTimestamp;	///< timestamp of internal data buffer
  unsigned currLen;		///< size of internal data buffer

  TIMESTAMP timestampOffset;       ///< timestamp offset b/w Tx and Rx blocks
  TIMESTAMP latestWriteTimestamp;  ///< timestamp of most recent ping command
  TIMESTAMP pingTimestamp;	   ///< timestamp of most recent ping response

  unsigned long hi32Timestamp;
  unsigned long lastPktTimestamp;

  double rxGain;

  /** Set the transmission frequency */
  bool tx_setFreq(double freq, double *actual_freq);

  /** Set the receiver frequency */
  bool rx_setFreq(double freq, double *actual_freq);

 public:

  /** Object constructor */
  bladeRFDevice(int sps, bool skipRx);

  /** Instantiate the bladeRF */
  int open(const std::string &, bool);

  /** Start the bladeRF */
  bool start();

  /** Stop the bladeRF */
  bool stop();

  /** Set priority not supported */
  void setPriority() { return; }

  enum TxWindowType getWindowType() { return TX_WINDOW_FIXED; }

  /**
	Read samples from the bladeRF.
	@param buf preallocated buf to contain read result
	@param len number of samples desired
	@param overrun Set if read buffer has been overrun, e.g. data not being read fast enough
	@param timestamp The timestamp of the first samples to be read
	@param underrun Set if bladeRF does not have data to transmit, e.g. data not being sent fast enough
	@param RSSI The received signal strength of the read result
	@return The number of samples actually read
  */
  int  readSamples(short *buf, int len, bool *overrun,
		   TIMESTAMP timestamp = 0xffffffff,
		   bool *underrun = NULL,
		   unsigned *RSSI = NULL);
  /**
        Write samples to the bladeRF.
        @param buf Contains the data to be written.
        @param len number of samples to write.
        @param underrun Set if bladeRF does not have data to transmit, e.g. data not being sent fast enough
        @param timestamp The timestamp of the first sample of the data buffer.
        @param isControl Set if data is a control packet, e.g. a ping command
        @return The number of samples actually written
  */
  int  writeSamples(short *buf, int len, bool *underrun,
		    TIMESTAMP timestamp = 0xffffffff,
		    bool isControl = false);

  /** Update the alignment between the read and write timestamps */
  bool updateAlignment(TIMESTAMP timestamp);

  /** Set the transmitter frequency */
  bool setTxFreq(double wFreq);

  /** Set the receiver frequency */
  bool setRxFreq(double wFreq);

  /** Returns the starting write Timestamp*/
  TIMESTAMP initialWriteTimestamp(void) { return 0x10000;}

  /** Returns the starting read Timestamp*/
  TIMESTAMP initialReadTimestamp(void) { return 0;}

  /** returns the full-scale transmit amplitude **/
  double fullScaleInputValue() {return 2040.0;}

  /** returns the full-scale receive amplitude **/
  double fullScaleOutputValue() {return 2040.0;}

  /** sets the receive chan gain, returns the gain setting **/
  double setRxGain(double dB);

  /** get the current receive gain */
  double getRxGain(void) {return rxGain;}

  /** return maximum Rx Gain **/
  double maxRxGain(void);

  /** return minimum Rx Gain **/
  double minRxGain(void);

  /** sets the transmit chan gain, returns the gain setting **/
  double setTxGain(double dB);

  /** return maximum Tx Gain **/
  double maxTxGain(void);

  /** return minimum Rx Gain **/
  double minTxGain(void);


  /** Return internal status values */
  inline double getTxFreq() { return 0;}
  inline double getRxFreq() { return 0;}
  inline double getSampleRate() {return actualSampleRate;}
  inline double numberRead() { return samplesRead; }
  inline double numberWritten() { return samplesWritten;}

};

#endif // _BLADERF_DEVICE_H_

