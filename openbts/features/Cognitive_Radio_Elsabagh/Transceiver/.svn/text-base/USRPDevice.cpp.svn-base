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


#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "Threads.h"
#include "USRPDevice.h"

#include <Logger.h>


using namespace std;

string write_it(unsigned v) {
  string s = "   ";
  s[0] = (v>>16) & 0x0ff;
  s[1] = (v>>8) & 0x0ff;
  s[2] = (v) & 0x0ff;
  return s;
}


const float USRPDevice::LO_OFFSET = 4.0e6;
const double USRPDevice::masterClockRate = (double) 64.0e6;

bool USRPDevice::compute_regs(double freq,
			      unsigned *R,
			      unsigned *control,
			      unsigned *N,
			      double *actual_freq) 
{
  
  float phdet_freq = 64.0e6/R_DIV;
  int desired_n = (int) round(freq*freq_mult/phdet_freq);
  *actual_freq = desired_n * phdet_freq/freq_mult;
  float B = floor(desired_n/16);
  float A = desired_n - 16*B;
  unsigned B_DIV = int(B);
  unsigned A_DIV = int(A);
  if (B < A) return false;
  *R = (R_RSV<<22) | 
    (BSC << 20) | 
    (TEST << 19) | 
    (LDP << 18) | 
    (ABP << 16) | 
    (R_DIV << 2);
  *control = (P<<22) | 
    (PD<<20) | 
    (CP2 << 17) | 
    (CP1 << 14) | 
    (PL << 12) | 
    (MTLD << 11) | 
    (CPG << 10) | 
    (CP3S << 9) | 
    (PDP << 8) | 
    (MUXOUT << 5) | 
    (CR << 4) | 
    (PC << 2);
  *N = (DIVSEL<<23) | 
    (DIV2<<22) | 
    (CPGAIN<<21) | 
    (B_DIV<<8) | 
    (N_RSV<<7) | 
    (A_DIV<<2);
  return true;
}


bool USRPDevice::tx_setFreq(double freq, double *actual_freq) 
{
  unsigned R, control, N;
  if (!compute_regs(freq, &R, &control, &N, actual_freq)) return false;
  if (R==0) return false;
  
  m_uTx->_write_spi(0,SPI_ENABLE_TX_A,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((R & ~0x3) | 1));
  m_uTx->_write_spi(0,SPI_ENABLE_TX_A,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((control & ~0x3) | 0));
  usleep(10000);
  m_uTx->_write_spi(0,SPI_ENABLE_TX_A,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((N & ~0x3) | 2));
  
  if (m_uTx->read_io(0) & PLL_LOCK_DETECT)  return true;
  if (m_uTx->read_io(0) & PLL_LOCK_DETECT)  return true;
  return false;
}


bool USRPDevice::rx_setFreq(double freq, double *actual_freq) 
{
  unsigned R, control, N;
  if (!compute_regs(freq, &R, &control, &N, actual_freq)) return false;
  if (R==0) return false;
  
  m_uRx->_write_spi(0,SPI_ENABLE_RX_B,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((R & ~0x3) | 1));
  m_uRx->_write_spi(0,SPI_ENABLE_RX_B,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((control & ~0x3) | 0));
  usleep(10000);
  m_uRx->_write_spi(0,SPI_ENABLE_RX_B,SPI_FMT_MSB | SPI_FMT_HDR_0,
		    write_it((N & ~0x3) | 2));
  
  
  if (m_uRx->read_io(1) & PLL_LOCK_DETECT)  return true;
  if (m_uRx->read_io(1) & PLL_LOCK_DETECT)  return true;
  return false;
}


USRPDevice::USRPDevice (double _desiredSampleRate) 
{
  LOG(INFO) << "creating USRP device...";
  decimRate = (unsigned int) round(masterClockRate/_desiredSampleRate);
  actualSampleRate = masterClockRate/decimRate;

#ifdef SWLOOPBACK 
  samplePeriod = 1.0e6/actualSampleRate;
  loopbackBufferSize = 0;
  gettimeofday(&lastReadTime,NULL);
  firstRead = false;
#endif
}

bool USRPDevice::make(bool wSkipRx) 
{
  skipRx = wSkipRx;

  LOG(INFO) << "making USRP device..";
#ifndef SWLOOPBACK 
  string rbf = "std_inband.rbf";
  //string rbf = "inband_1rxhb_1tx.rbf"; 
  m_uRx.reset();
  if (!skipRx) {
  try {
    m_uRx = usrp_standard_rx_sptr(usrp_standard_rx::make(0,decimRate,1,-1,
                                                         usrp_standard_rx::FPGA_MODE_NORMAL,
                                                         1024,16*8,rbf));
#ifdef HAVE_LIBUSRP_3_2
    m_uRx->set_fpga_master_clock_freq(masterClockRate);
#endif
  }
  
  catch(...) {
    LOG(ERROR) << "make failed on Rx";
    m_uRx.reset();
    return false;
  }

  if (m_uRx->fpga_master_clock_freq() != masterClockRate)
  {
    LOG(ERROR) << "WRONG FPGA clock freq = " << m_uRx->fpga_master_clock_freq()
               << ", desired clock freq = " << masterClockRate;
    m_uRx.reset();
    return false;
  }
  }

  try {
    m_uTx = usrp_standard_tx_sptr(usrp_standard_tx::make(0,decimRate*2,1,-1,
                                                         1024,16*8,rbf));
#ifdef HAVE_LIBUSRP_3_2
    m_uTx->set_fpga_master_clock_freq(masterClockRate);
#endif
  }
  
  catch(...) {
    LOG(ERROR) << "make failed on Tx";
    m_uTx.reset();
    return false;
  }

  if (m_uTx->fpga_master_clock_freq() != masterClockRate)
  {
    LOG(ERROR) << "WRONG FPGA clock freq = " << m_uTx->fpga_master_clock_freq()
               << ", desired clock freq = " << masterClockRate;
    m_uTx.reset();
    return false;
  }

  if (!skipRx) m_uRx->stop();
  m_uTx->stop();
  
#endif

  samplesRead = 0;
  samplesWritten = 0;
  started = false;
  
  return true;
}

bool USRPDevice::start() 
{
  LOG(INFO) << "starting USRP...";
#ifndef SWLOOPBACK 
  if (!m_uRx && !skipRx) return false;
  if (!m_uTx) return false;
  
  if (!skipRx) m_uRx->stop();
  m_uTx->stop();

  // power up and configure daughterboards
  m_uTx->_write_oe(0,0,0xffff);
  m_uTx->_write_oe(0,(POWER_UP|RX_TXN|ENABLE), 0xffff);
  m_uTx->write_io(0,(~POWER_UP|RX_TXN),(POWER_UP|RX_TXN|ENABLE));
  m_uTx->write_io(0,ENABLE,(RX_TXN | ENABLE));
  m_uTx->_write_fpga_reg(FR_ATR_MASK_0 ,0);//RX_TXN|ENABLE);
  m_uTx->_write_fpga_reg(FR_ATR_TXVAL_0,0);//,0 |ENABLE);
  m_uTx->_write_fpga_reg(FR_ATR_RXVAL_0,0);//,RX_TXN|0);
  m_uTx->_write_fpga_reg(40,0);
  m_uTx->_write_fpga_reg(42,0);
  m_uTx->set_pga(0,m_uTx->pga_max()); // should be 20dB
  m_uTx->set_pga(1,m_uTx->pga_max());
  m_uTx->set_mux(0x00000098);
  LOG(INFO) << "TX pgas: " << m_uTx->pga(0) << ", " << m_uTx->pga(1);

  if (!skipRx) {
    m_uRx->_write_fpga_reg(FR_ATR_MASK_0  + 3*3,0);
    m_uRx->_write_fpga_reg(FR_ATR_TXVAL_0 + 3*3,0);
    m_uRx->_write_fpga_reg(FR_ATR_RXVAL_0 + 3*3,0);
    m_uRx->_write_fpga_reg(43,0);
    m_uRx->_write_oe(1,(POWER_UP|RX_TXN|ENABLE), 0xffff);
    m_uRx->write_io(1,(~POWER_UP|RX_TXN|ENABLE),(POWER_UP|RX_TXN|ENABLE));
    //m_uRx->write_io(1,0,RX2_RX1N); // using Tx/Rx/
    m_uRx->write_io(1,RX2_RX1N,RX2_RX1N); // using Rx2
    m_uRx->set_adc_buffer_bypass(2,true);
    m_uRx->set_adc_buffer_bypass(3,true);
    m_uRx->set_pga(2,m_uRx->pga_max()); // should be 20dB
    m_uRx->set_pga(3,m_uRx->pga_max());
    m_uRx->set_mux(0x00000032);
    m_uRx->write_aux_dac(1,0,(int) ceil(0.2*4096.0/3.3)); // set to maximum gain 
  }

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

 
  if (!skipRx) 
  started = (m_uRx->start() && m_uTx->start());
  else
  started = m_uTx->start();
  return started;
#else
  gettimeofday(&lastReadTime,NULL);
  return true;
#endif
}

bool USRPDevice::stop() 
{
#ifndef SWLOOPBACK 
  if (!m_uRx) return false;
  if (!m_uTx) return false;
  
  // power down
  m_uTx->write_io(0,(~POWER_UP|RX_TXN),(POWER_UP|RX_TXN|ENABLE));
  m_uRx->write_io(1,~POWER_UP,(POWER_UP|ENABLE));
  
  delete[] currData;
  
  started = !(m_uRx->stop() && m_uTx->stop());
  return !started;
#else
  return true;
#endif
}


// NOTE: Assumes sequential reads
int USRPDevice::readSamples(short *buf, int len, bool *overrun, 
			    TIMESTAMP timestamp,
			    bool *underrun,
			    unsigned *RSSI) 
{
#ifndef SWLOOPBACK 
  if (!m_uRx) return 0;
  
  timestamp += timestampOffset;
  
  if (timestamp + len < timeStart) {
    memset(buf,0,len*2*sizeof(short));
    return len;
  }

  if (underrun) *underrun = false;
 
  uint32_t *readBuf = NULL;
 
  while (1) {
    //guestimate USB read size
    int readLen=0;
    {
      int numSamplesNeeded = timestamp + len - timeEnd;
      if (numSamplesNeeded <=0) break;
      readLen = 512 * ((int) ceil((float) numSamplesNeeded/126.0));
    }
    
    // read USRP packets, parse and save A/D data as needed
    if (readBuf!=NULL) delete[] readBuf;
    readBuf = new uint32_t[readLen/4];
    readLen = m_uRx->read((void *)readBuf,readLen,overrun);
    for(int pktNum = 0; pktNum < (readLen/512); pktNum++) {
      // tmpBuf points to start of a USB packet
      uint32_t* tmpBuf = (uint32_t *) (readBuf+pktNum*512/4);
      TIMESTAMP pktTimestamp = usrp_to_host_u32(tmpBuf[1]);
      uint32_t word0 = usrp_to_host_u32(tmpBuf[0]);
      uint32_t chan = (word0 >> 16) & 0x1f;
      unsigned payloadSz = word0 & 0x1ff;
      LOG(DEBUG) << "first two bytes: " << hex << word0 << " " << dec << pktTimestamp;

      bool incrementHi32 = ((lastPktTimestamp & 0x0ffffffffll) > pktTimestamp);
      if (incrementHi32 && (timeStart!=0)) {
           LOG(DEBUG) << "high 32 increment!!!";
           hi32Timestamp++;
      }
      pktTimestamp = (((TIMESTAMP) hi32Timestamp) << 32) | pktTimestamp;
      lastPktTimestamp = pktTimestamp;

      if (chan == 0x01f) {
	// control reply, check to see if its ping reply
        uint32_t word2 = usrp_to_host_u32(tmpBuf[2]);
	if ((word2 >> 16) == ((0x01 << 8) | 0x02)) {
          timestamp -= timestampOffset;
	  timestampOffset = pktTimestamp - pingTimestamp + PINGOFFSET;
	  LOG(DEBUG) << "updating timestamp offset to: " << timestampOffset;
          timestamp += timestampOffset;
	  isAligned = true;
	}
	continue;
      }
      if (chan != 0) {
	LOG(DEBUG) << "chan: " << chan << ", timestamp: " << pktTimestamp << ", sz:" << payloadSz;
	continue;
      }
      if ((word0 >> 28) & 0x04) {
	if (underrun) *underrun = true; 
	LOG(DEBUG) << "UNDERRUN in TRX->USRP interface";
      }
      if (RSSI) *RSSI = (word0 >> 21) & 0x3f;
      
      if (!isAligned) continue;
      
      unsigned cursorStart = pktTimestamp - timeStart + dataStart;
      while (cursorStart*2 > currDataSize) {
	cursorStart -= currDataSize/2;
      }
      if (cursorStart*2 + payloadSz/2 > currDataSize) {
	// need to circle around buffer
	memcpy(data+cursorStart*2,tmpBuf+2,(currDataSize-cursorStart*2)*sizeof(short));
	memcpy(data,tmpBuf+2+(currDataSize/2-cursorStart),payloadSz-(currDataSize-cursorStart*2)*sizeof(short));
      }
      else {
	memcpy(data+cursorStart*2,tmpBuf+2,payloadSz);
      }
      if (pktTimestamp + payloadSz/2/sizeof(short) > timeEnd) 
	timeEnd = pktTimestamp+payloadSz/2/sizeof(short);

      LOG(DEBUG) << "timeStart: " << timeStart << ", timeEnd: " << timeEnd << ", pktTimestamp: " << pktTimestamp;

    }	
  }     
 
  // copy desired data to buf
  unsigned bufStart = dataStart+(timestamp-timeStart);
  if (bufStart + len < currDataSize/2) { 
    LOG(DEBUG) << "bufStart: " << bufStart;
    memcpy(buf,data+bufStart*2,len*2*sizeof(short));
    memset(data+bufStart*2,0,len*2*sizeof(short));
  }
  else {
    LOG(DEBUG) << "len: " << len << ", currDataSize/2: " << currDataSize/2 << ", bufStart: " << bufStart;
    unsigned firstLength = (currDataSize/2-bufStart);
    LOG(DEBUG) << "firstLength: " << firstLength;
    memcpy(buf,data+bufStart*2,firstLength*2*sizeof(short));
    memset(data+bufStart*2,0,firstLength*2*sizeof(short));
    memcpy(buf+firstLength*2,data,(len-firstLength)*2*sizeof(short));
    memset(data,0,(len-firstLength)*2*sizeof(short));
  }
  dataStart = (bufStart + len) % (currDataSize/2);
  timeStart = timestamp + len;
  if (readBuf!=NULL) delete[] readBuf;
 
  return len;
  
#else
  if (loopbackBufferSize < 2) return 0;
  int numSamples = 0;
  struct timeval currTime;
  gettimeofday(&currTime,NULL);
  double timeElapsed = (currTime.tv_sec - lastReadTime.tv_sec)*1.0e6 + 
    (currTime.tv_usec - lastReadTime.tv_usec);
  if (timeElapsed < samplePeriod) {return 0;}
  int numSamplesToRead = (int) floor(timeElapsed/samplePeriod);
  if (numSamplesToRead < len) return 0;
  
  if (numSamplesToRead > len) numSamplesToRead = len;
  if (numSamplesToRead > loopbackBufferSize/2) {
    firstRead =false; 
    numSamplesToRead = loopbackBufferSize/2;
  }
  memcpy(buf,loopbackBuffer,sizeof(short)*2*numSamplesToRead);
  loopbackBufferSize -= 2*numSamplesToRead;
  memcpy(loopbackBuffer,loopbackBuffer+2*numSamplesToRead,
	 sizeof(short)*loopbackBufferSize);
  numSamples = numSamplesToRead;
  if (firstRead) {
    int new_usec = lastReadTime.tv_usec + (int) round((double) numSamplesToRead * samplePeriod);
    lastReadTime.tv_sec = lastReadTime.tv_sec + new_usec/1000000;
    lastReadTime.tv_usec = new_usec % 1000000;
  }
  else {
    gettimeofday(&lastReadTime,NULL);
    firstRead = true;
  }
  samplesRead += numSamples;
  
  return numSamples;
#endif
}

int USRPDevice::writeSamples(short *buf, int len, bool *underrun, 
			     unsigned long long timestamp,
			     bool isControl) 
{
#ifndef SWLOOPBACK 
  if (!m_uTx) return 0;
  
  int numWritten = 0;
  unsigned isStart = 1;
  unsigned RSSI = 0;
  unsigned CHAN = (isControl) ? 0x01f : 0x00;
  len = len*2*sizeof(short);
  int numPkts = (int) ceil((float)len/(float)504);
  unsigned isEnd = (numPkts < 2);
  uint32_t *outPkt = new uint32_t[128*numPkts];
  int pktNum = 0;
  while (numWritten < len) {
    // pkt is pointer to start of a USB packet
    uint32_t *pkt = outPkt + pktNum*128;
    isEnd = (len - numWritten <= 504);
    unsigned payloadLen = ((len - numWritten) < 504) ? (len-numWritten) : 504;
    pkt[0] = (isStart << 12 | isEnd << 11 | (RSSI & 0x3f) << 5 | CHAN) << 16 | payloadLen;
    pkt[1] = timestamp & 0x0ffffffffll;
    memcpy(pkt+2,buf+(numWritten/sizeof(short)),payloadLen);
    numWritten += payloadLen;
    timestamp += payloadLen/2/sizeof(short);
    isStart = 0;
    pkt[0] = host_to_usrp_u32(pkt[0]);
    pkt[1] = host_to_usrp_u32(pkt[1]);
    pktNum++;
  }
  m_uTx->write((const void*) outPkt,sizeof(uint32_t)*128*numPkts,NULL);
  delete[] outPkt;  

  samplesWritten += len/2/sizeof(short);
  return len/2/sizeof(short);
#else
  int retVal = len;
  memcpy(loopbackBuffer+loopbackBufferSize,buf,sizeof(short)*2*len);
  samplesWritten += retVal;
  loopbackBufferSize += retVal*2;
   
  return retVal;
#endif
}

bool USRPDevice::updateAlignment(TIMESTAMP timestamp) 
{
#ifndef SWLOOPBACK 
  short data[] = {0x00,0x02,0x00,0x00};
  uint32_t *wordPtr = (uint32_t *) data;
  *wordPtr = host_to_usrp_u32(*wordPtr);
  bool tmpUnderrun;
  if (writeSamples((short *) data,1,&tmpUnderrun,timestamp & 0x0ffffffffll,true)) {
    pingTimestamp = timestamp;
    return true;
  }
  return false;
#else
  return true;
#endif
}

#ifndef SWLOOPBACK 
bool USRPDevice::setTxFreq(double wFreq) {
  // Tune to wFreq+LO_OFFSET, to prevent LO bleedthrough from interfering with transmitted signal.
  double actFreq;
  if (!tx_setFreq(wFreq+LO_OFFSET,&actFreq)) return false;
  bool retVal = m_uTx->set_tx_freq(0,(wFreq-actFreq));
  LOG(INFO) << "set TX: " << wFreq-actFreq << " actual TX: " << m_uTx->tx_freq(0);
  return retVal;
};

bool USRPDevice::setRxFreq(double wFreq) {
  // Tune to wFreq-2*LO_OFFSET, to
  //   1) prevent LO bleedthrough (as with the setTxFreq method above)
  //   2) The extra LO_OFFSET pushes potential transmitter energy (GSM BS->MS transmissions 
  //        are 45Mhz above MS->BS transmissions) into a notch of the baseband lowpass filter 
  //        in front of the ADC.  This possibly gives us an extra 10-20dB Tx/Rx isolation.
  double actFreq;
  if (!rx_setFreq(wFreq-2*LO_OFFSET,&actFreq)) return false;
  bool retVal = m_uRx->set_rx_freq(0,(wFreq-actFreq));
  LOG(DEBUG) << "set RX: " << wFreq-actFreq << " actual RX: " << m_uRx->rx_freq(0);
  return retVal;
};

#else
bool USRPDevice::setTxFreq(double wFreq) { return true;};
bool USRPDevice::setRxFreq(double wFreq) { return true;};
#endif
