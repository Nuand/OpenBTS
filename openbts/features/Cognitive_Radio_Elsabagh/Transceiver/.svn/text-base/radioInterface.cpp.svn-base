
#include "radioInterface.h"
#include <Logger.h>



GSM::Time VectorQueue::nextTime() const
{
  GSM::Time retVal;
  mLock.lock();
  while (mQ.size()==0) mWriteSignal.wait(mLock);
  retVal = mQ.top()->time();
  mLock.unlock();
  return retVal;
}

RadioInterface::RadioInterface(USRPDevice *wUsrp,
                               int wReceiveOffset,
			       int wSamplesPerSymbol,
			       GSM::Time wStartTime)

{
  underrun = false;

  //sendHistory = new signalVector(INHISTORY);
  rcvHistory = NULL;
 // sendBuffer = NULL;
  rcvBuffer = NULL;
  //sendLPF = NULL;
  rcvLPF = NULL;
  mOn = false;

  usrp = wUsrp;
  receiveOffset = wReceiveOffset;
  samplesPerSymbol = wSamplesPerSymbol;
  mClock.set(wStartTime);

}

RadioInterface::~RadioInterface(void) {
  //if (sendBuffer!=NULL) delete sendBuffer;
  if (rcvBuffer!=NULL) delete rcvBuffer;
  //if (sendHistory!=NULL) delete sendHistory;
  if (rcvHistory!=NULL) delete rcvHistory;
  //if (sendLPF!=NULL) delete sendLPF;
  if (rcvLPF!=NULL) delete rcvLPF;
  //mTransmitFIFO.clear();
  mReceiveFIFO.clear();
}

short *RadioInterface::USRPifyVector(signalVector &wVector)
{

  short *retVector = new short[2*wVector.size()];

  signalVector::iterator itr = wVector.begin();
  short *shortItr = retVector;
  while (itr < wVector.end()) {
    *shortItr++ = (short) host_to_usrp_short((short)itr->real());
    *shortItr++ = (short) host_to_usrp_short((short)itr->imag());
    itr++;
  }

  return retVector;

}

signalVector *RadioInterface::unUSRPifyVector(short *shortVector, int numSamples)

{

  signalVector *newVector = new signalVector(numSamples);

  signalVector::iterator itr = newVector->begin();
  short *shortItr = shortVector;

// need to flip I and Q from USRP
#ifndef SWLOOPBACK
#define FLIP_IQ 1
#else
#define FLIP_IQ 0
#endif

  while (itr < newVector->end()) {
    *itr++ = Complex<float>(usrp_to_host_short(*(shortItr+FLIP_IQ)),
		            usrp_to_host_short(*(shortItr+1-FLIP_IQ)));
    //LOG(DEEPDEBUG) << (*(itr-1));
    shortItr += 2;
  }

  return newVector;

}


bool started = false;

#define POLYPHASESPAN 10
 /*
void RadioInterface::pushBuffer(void) {

  if (sendBuffer->size() < INCHUNK) {
    return;
  }

  int numChunks = sendBuffer->size()/INCHUNK;

  signalVector* truncatedBuffer = new signalVector(numChunks*INCHUNK);
  sendBuffer->segmentCopyTo(*truncatedBuffer,0,numChunks*INCHUNK);

  if (!sendLPF) {
    int P = OUTRATE; int Q = INRATE;
    float cutoffFreq = (P < Q) ? (1.0/(float) Q) : (1.0/(float) P);
    sendLPF = createLPF(cutoffFreq,651,P);
  }

  // resample data to USRP sample rate
  signalVector *inputVector = new signalVector(*sendHistory,*truncatedBuffer);
  signalVector *resampledVector = polyphaseResampleVector(*inputVector,
					    OUTRATE,
					    INRATE,sendLPF);
  delete inputVector;

  // Set transmit gain and power here.
  scaleVector(*resampledVector,13500.0); ///2.25); // this gets 2W out of 3318PA at 885Mhz
  //scaleVector(*resampledVector,100.0);

  short *resampledVectorShort = USRPifyVector(*resampledVector);

  // start the USRP when we actually have data to send to the USRP.
  if (!started) {
    started = true;
    LOG(INFO) << "Starting USRP";
    usrp->start();
    LOG(DEBUG) << "USRP started";
    usrp->updateAlignment(10000);
    usrp->updateAlignment(10000);
  }

  // send resampleVector
  writingRadioLock.lock();
  int samplesWritten = usrp->writeSamples(resampledVectorShort+OUTHISTORY*2,
					  (resampledVector->size()-OUTHISTORY),
					  &underrun,
					  writeTimestamp);
  //LOG(DEEPDEBUG) << "writeTimestamp: " << writeTimestamp << ", samplesWritten: " << samplesWritten;
  writeTimestamp += (TIMESTAMP) samplesWritten;
  wroteRadioSignal.signal();
  writingRadioLock.unlock();

  LOG(DEEPDEBUG) << "converted " << truncatedBuffer->size()
       << " transceiver samples into " << samplesWritten
       << " radio samples ";


  delete resampledVector;
  delete []resampledVectorShort;

  // update the history of sent data
  truncatedBuffer->segmentCopyTo(*sendHistory,truncatedBuffer->size()-INHISTORY,
				INHISTORY);

  // update the buffer, i.e. keep the samples we didn't send
  signalVector *tmp = sendBuffer;
  sendBuffer = new signalVector(sendBuffer->size()-truncatedBuffer->size());
  tmp->segmentCopyTo(*sendBuffer,truncatedBuffer->size(),
		     sendBuffer->size());
  delete tmp;
  delete truncatedBuffer;

}

*/



void RadioInterface::pullBuffer(void)
{

 // writingRadioLock.lock();
  // These timestamps are in samples @ 400 kHz.
 //	while (readTimestamp > writeTimestamp - (TIMESTAMP) 2*OUTCHUNK) {
  //  LOG(DEEPDEBUG) << "waiting..." << readTimestamp << " " << writeTimestamp;
   //wroteRadioSignal.wait(writingRadioLock);
    //wroteRadioSignal.wait(writingRadioLock,1);
  //}
  //writingRadioLock.unlock();

  bool localUnderrun;

   // receive receiveVector
  short* shortVector = new short[OUTCHUNK*2];
  LOG(DEEPDEBUG) << "begn1 read sample  " ;
  int samplesRead = usrp->readSamples(shortVector,OUTCHUNK,&overrun,readTimestamp,&localUnderrun);
  LOG(DEEPDEBUG) << "number of Samples read 1  " << samplesRead << " [3] " << shortVector[3]<<"  &[3]  "<< &shortVector[3];
  underrun |= localUnderrun;
  readTimestamp += (TIMESTAMP) samplesRead;
  while (samplesRead < OUTCHUNK) {
    int oldSamplesRead = samplesRead;
    samplesRead += usrp->readSamples(shortVector+2*samplesRead,
				     OUTCHUNK-samplesRead,
				     &overrun,
				     readTimestamp,
				     &localUnderrun);
    underrun |= localUnderrun;
    readTimestamp += (TIMESTAMP) (samplesRead - oldSamplesRead);
  }

  signalVector *receiveVector = unUSRPifyVector(shortVector,samplesRead);
  delete []shortVector;
  if (!rcvLPF) {
    int P = INRATE; int Q = OUTRATE;
    float cutoffFreq = (P < Q) ? (1.0/(float) Q) : (1.0/(float) P);
    rcvLPF = createLPF(cutoffFreq,961,P);
  }

  signalVector *retVector = NULL;

  if (!rcvHistory) {
    rcvHistory = new signalVector(OUTHISTORY);
    rcvHistory->fill(0);
  }

  // resample received data to multiple of GSM symbol rate
  signalVector inputVector(*rcvHistory,*receiveVector);
  retVector = polyphaseResampleVector(inputVector,
				      INRATE,OUTRATE,rcvLPF);

  // push sampled data to back of receive buffer
  signalVector *tmp = retVector;
  retVector = new signalVector(retVector->size()-INHISTORY);
  tmp->segmentCopyTo(*retVector,INHISTORY,tmp->size()-INHISTORY);
  delete tmp;

  LOG(DEEPDEBUG) << "converted " << receiveVector->size()
	<< " radio samples into " << retVector->size()
	<< " transceiver samples ";

  // update history of received data
  receiveVector->segmentCopyTo(*rcvHistory,receiveVector->size()-OUTHISTORY,OUTHISTORY);

  delete receiveVector;

  if (rcvBuffer) {
    signalVector *tmp = rcvBuffer;
    rcvBuffer = new signalVector(*tmp,*retVector);
    delete tmp;
    delete retVector;
  }
  else
    rcvBuffer = retVector;


}

bool RadioInterface::tuneTx(double freq)
{
  if (mOn) return false;
  return usrp->setTxFreq(freq);
}

bool RadioInterface::tuneRx(double freq)
{
  if (mOn) return false;
  return usrp->setRxFreq(freq);
}


void RadioInterface::start()
{
  LOG(INFO) << "starting radio interface...";
  writeTimestamp = 20000;
  readTimestamp = 20000;

 // mTransmitRadioServiceLoopThread.start((void* (*)(void*))TransmitRadioServiceLoopAdapter,
	//				(void*)this);
  mReceiveRadioServiceLoopThread.start((void* (*)(void*))ReceiveRadioServiceLoopAdapter,
				       (void*)this);

  //mAlignRadioServiceLoopThread.start((void * (*)(void*))AlignRadioServiceLoopAdapter,
    //                                 (void*)this);
  mOn = true;
  LOG(DEBUG) << "radio interface started!";
}

/*
void *TransmitRadioServiceLoopAdapter(RadioInterface *radioInterface)
{
  while (1) {
    radioInterface->driveTransmitRadio();
    pthread_testcancel();
  }
  return NULL;
}*/

void *ReceiveRadioServiceLoopAdapter(RadioInterface *radioInterface)
{

//////////////////////////ADDED For CR////////////////////////////////
	/////////////////////////////////////////////////
	//////////////////////////////////////////////
bool ext=true;
int idx=0;
  while (ext) {
    radioInterface->driveReceiveRadio();
    pthread_testcancel();
idx =idx+1;
if (idx==2) ext=false;
  LOG(DEBUG) << "drive receive idx"<<idx;
  }
  LOG(DEBUG) << "drive receive finish";

  return NULL;
}
/*
void *AlignRadioServiceLoopAdapter(RadioInterface *radioInterface)
{
  while (1) {
    radioInterface->alignRadio();
    pthread_testcancel();
  }
  return NULL;
}*/

/*
void RadioInterface::alignRadio() {
  sleep(60);
  usrp->updateAlignment(writeTimestamp+ (TIMESTAMP) 10000);
}
*/

/*
void RadioInterface::driveTransmitRadio() {

  radioVector *radioBurst = NULL;

  radioBurst = mTransmitFIFO.read();

  LOG(DEEPDEBUG) << "transmitFIFO: read radio vector at time: " << radioBurst->time();

  signalVector *newBurst = radioBurst;
  if (sendBuffer) {
    signalVector *tmp = sendBuffer;
    sendBuffer = new signalVector(*sendBuffer,*newBurst);
    delete tmp;
  }
  else
    sendBuffer = new signalVector(*newBurst);

  delete radioBurst;

  pushBuffer();
}
*/
int idxinrcv=0;
void RadioInterface::driveReceiveRadio() {
  pullBuffer();

  if (!rcvBuffer) {
    return;}

  GSM::Time rcvClock = mClock.get();
  rcvClock.decTN(receiveOffset);
  unsigned tN = rcvClock.TN();
  int rcvSz = rcvBuffer->size();
  LOG(DEEPDEBUG) << "rcvbuffersize: " << rcvSz;
  int readSz = 0;
  const int symbolsPerSlot = gSlotLen + 8;

  // while there's enough data in receive buffer, form received
  //    GSM bursts and pass up to Transceiver
  // Using the 157-156-156-156 symbols per timeslot format.
  while (rcvSz > (symbolsPerSlot + (tN % 4 == 0))*samplesPerSymbol) {
    signalVector rxVector(rcvBuffer->begin(),
			  readSz,
			  (symbolsPerSlot + (tN % 4 == 0))*samplesPerSymbol);
    GSM::Time tmpTime = rcvClock;
    if (rcvClock.FN() >= 0) {
      LOG(DEEPDEBUG) << "FN: " << rcvClock.FN();
      radioVector* rxBurst = new radioVector(rxVector,tmpTime);
      mReceiveFIFO.write(rxBurst);
    }
    mClock.incTN();
    rcvClock.incTN();
    if (mReceiveFIFO.size() >= 16) mReceiveFIFO.wait(8);
    LOG(DEEPDEBUG) << "receiveFIFO: wrote radio vector at: "  << ", new size: " << mReceiveFIFO.size() ;
    readSz += (symbolsPerSlot+(tN % 4 == 0))*samplesPerSymbol;
    rcvSz -= (symbolsPerSlot+(tN % 4 == 0))*samplesPerSymbol;

    tN = rcvClock.TN();
  }

  signalVector *tmp = new signalVector(rcvBuffer->size()-readSz);
  rcvBuffer->segmentCopyTo(*tmp,readSz,tmp->size());
  delete rcvBuffer;
  rcvBuffer = tmp;
idxinrcv = idxinrcv+1;
}
