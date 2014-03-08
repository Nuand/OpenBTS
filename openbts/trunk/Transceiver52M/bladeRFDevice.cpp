/*
* Copyright 2008, 2009 Free Software Foundation, Inc.
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
	Compilation Flags

	SWLOOPBACK	compile for software loopback testing
*/

#define usrp_to_host_u32(x) x
#define host_to_usrp_u32(x) x
#define host_to_usrp_short(x) x
#define SWLOOPBACK

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "Threads.h"
#include "bladeRFDevice.h"

#include <Logger.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

using namespace std;

bladeRFDevice::bladeRFDevice(int sps, bool skipRx)
  : skipRx(skipRx)
{
  LOG(INFO) << "creating bladeRF device...";

  this->sps = sps;
  rxGain = 0;

}

int bladeRFDevice::open(const std::string &, bool)
{
  writeLock.unlock();

  LOG(INFO) << "opening bladeRF device..";
  if (bladerf_open(&bdev, NULL)) {
      LOG(EMERG) << "Could not open bladeRF device";
      return -1;
  }
  char buf[34];
  bladerf_get_serial(bdev, buf);
  LOG(INFO) << "Openned bladeRF serial=" << buf;

  uint32_t val;

  bladerf_fpga_size bfs;
  if (!bladerf_get_fpga_size(bdev, &bfs)) {
      if (bfs == BLADERF_FPGA_40KLE)
          bladerf_load_fpga(bdev, "../Transceiver52M/hostedx40bts.rbf");
      else {
          bladerf_load_fpga(bdev, "../Transceiver52M/hostedx115bts.rbf");
      }
  } else {
      LOG(EMERG) << "Error fetching FPGA size";
      return -1;
  }
  LOG(INFO) << "bladeRF FPGA is loaded";

  bladerf_config_gpio_read(bdev, &val);
  val |= 0x10000; //enable timestamps, clears and resets everything on write
  bladerf_config_gpio_write(bdev, val);
  bladerf_config_gpio_read(bdev, &val);
  if (!(val & 0x10000)) {
      LOG(EMERG) << "Could not enable timestamps";
      return -1;
  }
  LOG(INFO) << "bladeRF timestamping enabled";

  if (BLADERF_DEVICE_SPEED_SUPER != bladerf_device_speed(bdev)) {
      LOG(EMERG) << "bladeRF transceiver only supports SuperSpeed mode at the moment";
      return -1;
  }


  struct bladerf_rational_rate rate, actual;
  rate.integer = (sps * 13e6) / 48;
  rate.num = (sps * 13e6) - rate.integer * 48;
  rate.den = 48;

  if (bladerf_set_rational_sample_rate(bdev, BLADERF_MODULE_RX, &rate, &actual)) {
      LOG(EMERG) << "Error setting RX sampling rate";
      return -1;
  }

  rate.integer = (sps * 13e6) / 48;
  rate.num = (sps * 13e6) - rate.integer * 48;
  rate.den = 48;
  if (bladerf_set_rational_sample_rate(bdev, BLADERF_MODULE_TX, &rate, &actual)) {
      LOG(EMERG) << "Error setting TX sampling rate";
      return -1;
  }

  /* Stream defaults */
#define DEFAULT_STREAM_XFERS        64
#define DEFAULT_STREAM_BUFFERS      5600
#define DEFAULT_STREAM_SAMPLES      2048
#define DEFAULT_STREAM_TIMEOUT      4000

  int status;
  status = bladerf_sync_config(bdev,
          BLADERF_MODULE_RX,
          BLADERF_FORMAT_SC16_Q12,
          DEFAULT_STREAM_BUFFERS,
          DEFAULT_STREAM_SAMPLES,
          DEFAULT_STREAM_XFERS,
          DEFAULT_STREAM_TIMEOUT
          );

  if (status != 0) {
      LOG(EMERG) << "Failed to intialize RX sync handle" << bladerf_strerror(status);
  }

  status = bladerf_sync_config(bdev,
          BLADERF_MODULE_TX,
          BLADERF_FORMAT_SC16_Q12,
          8,
          DEFAULT_STREAM_SAMPLES,
          4,
          DEFAULT_STREAM_TIMEOUT
          );

  if (status != 0) {
      LOG(EMERG) << "Failed to intialize TX sync handle" << bladerf_strerror(status);
  }


  samplesRead = 0;
  samplesWritten = 0;
  started = false;

  return NORMAL;
}



bool bladeRFDevice::start()
{
  LOG(INFO) << "starting bladeRF...";
  if (!bdev) return false;

  writeLock.lock();

  if (bladerf_enable_module(bdev, BLADERF_MODULE_RX, 1)) {
      LOG(EMERG) << "Error enabling RX";
      return -1;
  }

  if (bladerf_enable_module(bdev, BLADERF_MODULE_TX, 1)) {
      LOG(EMERG) << "Error enabling TX";
      return -1;
  }

  writeLock.unlock();

  // Set gains to midpoint
  setTxGain((minTxGain() + maxTxGain()) / 2);
  setRxGain((minRxGain() + maxRxGain()) / 2);

  data = new short[currDataSize];
  dataStart = 0;
  dataEnd = 0;
  timeStart = 0;
  timeEnd = 0;
  timestampOffset = 0;
  latestWriteTimestamp = 0;
  lastPktTimestamp = 0;
  hi32Timestamp = 0;
  isAligned = false;

  return true;
}

bool bladeRFDevice::stop()
{
  bladerf_enable_module(bdev, BLADERF_MODULE_RX, 0);
  bladerf_enable_module(bdev, BLADERF_MODULE_TX, 0);
  delete[] currData;

  return true;
}

double bladeRFDevice::maxTxGain()
{
  return 5.0;
}

double bladeRFDevice::minTxGain()
{
  return 0.5;
}

double bladeRFDevice::maxRxGain()
{
  return 5.0;
}

double bladeRFDevice::minRxGain()
{
  return 0.5;
}

double bladeRFDevice::setTxGain(double dB) {

   writeLock.lock();
   if (dB > maxTxGain()) dB = maxTxGain();
   if (dB < minTxGain()) dB = minTxGain();

   LOG(NOTICE) << "Setting TX gain to " << dB << " dB.";

   bladerf_set_txvga2(bdev, dB);

   writeLock.unlock();

   return dB;
}


double bladeRFDevice::setRxGain(double dB) {

   writeLock.lock();
   if (dB > maxRxGain()) dB = maxRxGain();
   if (dB < minRxGain()) dB = minRxGain();

   LOG(NOTICE) << "Setting RX gain to " << dB << " dB.";
   bladerf_set_rxvga2(bdev, dB);

   writeLock.unlock();

   return dB;
}

struct bladerf_superspeed_timestamp {
    uint32_t rsvd;
    uint32_t time_lo;
    uint32_t time_hi;
    uint32_t flags;
    uint16_t samples[512*2-8];
};


#define BST_SZ (sizeof(struct bladerf_superspeed_timestamp))
// NOTE: Assumes sequential reads
int bladeRFDevice::readSamples(short *buf, int len, bool *overrun,
        TIMESTAMP timestamp,
        bool *underrun,
        unsigned *RSSI)
{

    timestamp += timestampOffset;

    if (timestamp + len < timeStart) {
        memset(buf, 0, len*2*sizeof(short));
        return len;
    }

    if (underrun) *underrun = false;

    uint32_t readBuf[2000];

    while (1) {
        int readLen=0;
        int ret;
        struct bladerf_superspeed_timestamp *bst;
        {
            int numSamplesNeeded = timestamp + len - timeEnd;
            if (numSamplesNeeded <=0) break;
            readLen = BST_SZ * ((int) ceil((float) numSamplesNeeded/BST_SZ));
            if (readLen > 8000) readLen = (8000/BST_SZ)*BST_SZ;
        }

        overrun = 0;

        if ((ret = bladerf_sync_rx(bdev, readBuf, readLen/4, NULL, 0)))
            LOG(EMERG) << "Error receiving" << (readLen / 4) << "   " << ret;


        bst = (struct bladerf_superspeed_timestamp *)readBuf;

        for (int pktNum = 0; pktNum < (readLen/BST_SZ); pktNum++, bst++) {
            unsigned payloadSz = (512 * 2 - 8) * sizeof (uint16_t);

            TIMESTAMP pktTimestamp = (((uint64_t)bst->time_hi) << 32 | bst->time_lo) / 2;

            if (pktTimestamp < lastPktTimestamp) {
                LOG(EMERG) << "Received out of order packet";
                continue;
            }
            lastPktTimestamp = pktTimestamp;

            // Rob: disabled temporarily
            /*
            if (bst->flags & 0x04) {
                if (underrun) *underrun = true;
                LOG(DEBUG) << "UNDERRUN in TRX->bladeRF interface";
            }
            if (RSSI) *RSSI = (bst->flags >> 24) & 0x3f;
            */


            unsigned cursorStart = (pktTimestamp - timeStart) + dataStart;
            while (cursorStart*2 > currDataSize) {
                cursorStart -= currDataSize/2;
            }
            if (cursorStart*2 + payloadSz/2 > currDataSize) {
                // need to circle around buffer
                // write first part to circular buffer
                memcpy(data + cursorStart*2, bst->samples, (currDataSize - cursorStart*2) * sizeof(uint16_t) );
                // write second part to circular buffer
                memcpy(data, bst->samples + (currDataSize/2 - cursorStart) , payloadSz - (currDataSize - cursorStart*2) * sizeof(uint16_t));
            } else {
                // single write
                memcpy(data + cursorStart*2, bst->samples, payloadSz);
            }
            if (pktTimestamp + payloadSz/2/sizeof(uint16_t) > timeEnd) {
                timeEnd = pktTimestamp+payloadSz/2/sizeof(uint16_t);
            }
            LOG(DEBUG) << "timeStart: " << timeStart << ", timeEnd: " << timeEnd << ", pktTimestamp: " << pktTimestamp;
        }
    }

    // copy desired data to buf
    unsigned bufStart = dataStart+(timestamp-timeStart);
    if (bufStart + len < currDataSize/2) {
        LOG(DEBUG) << "bufStart: " << bufStart;
        // single read
        memcpy(buf, data + bufStart*2, len * 2 * sizeof(uint16_t));
        memset(data + bufStart*2, 0, len * 2 * sizeof(uint16_t));
    } else {
        LOG(DEBUG) << "len: " << len << ", currDataSize/2: " << currDataSize/2 << ", bufStart: " << bufStart;
        unsigned firstLength = (currDataSize/2 - bufStart);
        LOG(DEBUG) << "firstLength: " << firstLength;

        // read first part of circular buffer
        memcpy(buf, data + bufStart*2, firstLength * 2 * sizeof(uint16_t));
        memset(data + bufStart*2, 0, firstLength * 2 * sizeof(uint16_t));

        // read second part of circular buffer
        memcpy(buf + firstLength*2, data, (len - firstLength) * 2 * sizeof(uint16_t));
        memset(data, 0, (len - firstLength) * 2 * sizeof(uint16_t));
    }
    dataStart = (bufStart + len) % (currDataSize/2);
    timeStart = timestamp + len;

    return len;
}

struct bladerf_superspeed_timestamp leftover;
int leftover_len;
uint64_t tx_time = 0x20000;
#define PK_SZ 508
#define PK_SZ_B (PK_SZ*2)
int bladeRFDevice::writeSamples(short *buf, int len, bool *underrun,
        unsigned long long timestamp,
        bool isControl)
{
    int olen;
    olen = len;
    int i;

    if (!bdev) return 0;

    if (leftover_len + len < PK_SZ) {
        memcpy(&leftover.samples[leftover_len * 2], buf, 2 * sizeof(short) * len);
        leftover_len += len;
        return olen;
    }

    if (leftover_len) {
        memcpy(&leftover.samples[leftover_len * 2], buf, 2 * sizeof(short) * (PK_SZ - leftover_len));
        len -= PK_SZ - leftover_len;
        buf += 2 * (PK_SZ - leftover_len);
        leftover.time_lo = tx_time & 0x0ffffffffll;
        leftover.time_hi = tx_time >> 32;
        tx_time += PK_SZ * 2;
        leftover.rsvd = 0xdeadbeef;
        leftover.flags = -1;
        bladerf_sync_tx(bdev, (void*)&leftover, 512, NULL, NULL);
    }

    while (len >= PK_SZ) {
        memcpy(&leftover.samples[leftover_len * 2], buf, 2 * sizeof(short) * (PK_SZ - leftover_len));
        len -= PK_SZ;
        buf += 2 * (PK_SZ - leftover_len);
        leftover.time_lo = tx_time & 0x0ffffffffll;
        leftover.time_hi = tx_time >> 32;
        tx_time += PK_SZ * 2;
        leftover.rsvd = 0xdeadbeef;
        leftover.flags = -1;
        bladerf_sync_tx(bdev, (void*)&leftover, 512, NULL, NULL);
    }
    memcpy(&leftover.samples, buf, 2 * sizeof(short) * (len));
    leftover_len = len;

    writeLock.unlock();

    return olen;

}

bool bladeRFDevice::updateAlignment(TIMESTAMP timestamp)
{
  // bladeRF never goes out of alignment
  return true;
}

bool bladeRFDevice::setTxFreq(double wFreq) {
    LOG(EMERG) << "set TX freq: " << wFreq << std::endl;
    bladerf_set_frequency(bdev, BLADERF_MODULE_TX, wFreq);
    return true;
};
bool bladeRFDevice::setRxFreq(double wFreq) {
    LOG(EMERG) << "set RX freq: " << wFreq << std::endl;
    bladerf_set_frequency(bdev, BLADERF_MODULE_RX, wFreq);
    return true;
};

RadioDevice *RadioDevice::make(int sps, bool skipRx)
{
	return new bladeRFDevice(sps, skipRx);
}
