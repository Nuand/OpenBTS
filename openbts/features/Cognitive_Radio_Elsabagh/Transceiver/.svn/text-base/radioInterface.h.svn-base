
#include "sigProcLib.h"
#include "USRPDevice.h"
#include "GSMCommon.h"
#include "Interthread.h"

/** samples per GSM symbol */
#define SAMPSPERSYM 1

/** parameters for polyphase resampling */
#define INRATE     (65*SAMPSPERSYM)
#define OUTRATE    (96)
#define INHISTORY  (INRATE*2)
#define OUTHISTORY (OUTRATE*2)
#define INCHUNK    (INRATE*9)
#define OUTCHUNK   (OUTRATE*9)

/** class used to organize GSM bursts by GSM timestamps */
class radioVector : public signalVector {

private:

  GSM::Time mTime;   ///< the burst's GSM timestamp

public:
  /** constructor */
  radioVector(const signalVector& wVector,
	      GSM::Time& wTime): signalVector(wVector),mTime(wTime) {};

  /** timestamp read and write operators */
  GSM::Time time() const { return mTime;}
  void time(const GSM::Time& wTime) { mTime = wTime;}

  /** comparison operator, used for sorting */
  bool operator>(const radioVector& other) const {return mTime > other.mTime;}

};

/** a priority queue of radioVectors, i.e. GSM bursts, sorted so that earliest element is at top */
class VectorQueue : public InterthreadPriorityQueue<radioVector> {

public:

  /** the top element of the queue */
  GSM::Time nextTime() const;

};

/** a FIFO of radioVectors */
class VectorFIFO : public InterthreadQueueWithWait<radioVector> {};

/** the basestation clock class */
class RadioClock {

private:

  GSM::Time mClock;
  Mutex mLock;
  Signal updateSignal;

public:

  /** Set clock */
  void set(const GSM::Time& wTime) { mLock.lock(); mClock = wTime; updateSignal.signal(); mLock.unlock();}
  //void set(const GSM::Time& wTime) { mLock.lock(); mClock = wTime; updateSignal.broadcast(); mLock.unlock();}

  /** Increment clock */
  void incTN() { mLock.lock(); mClock.incTN(); updateSignal.signal(); mLock.unlock();}
  //void incTN() { mLock.lock(); mClock.incTN(); updateSignal.broadcast(); mLock.unlock();}

  /** Get clock value */
  GSM::Time get() { mLock.lock(); GSM::Time retVal = mClock; mLock.unlock(); return retVal;}

  /** Wait until clock has changed */
  void wait() {mLock.lock(); updateSignal.wait(mLock,1); mLock.unlock();}
  // FIXME -- If we take away the timeout, a lot of threads don't start.  Why?
  //void wait() {mLock.lock(); updateSignal.wait(mLock); mLock.unlock();}

};
/** class to interface the transceiver with the USRP */
class RadioInterface {

private:

 //Thread mTransmitRadioServiceLoopThread;     ///< thread that handles transmission of GSM bursts
  Thread mReceiveRadioServiceLoopThread;      ///< thread that handles reception of GSM bursts
  //Thread mAlignRadioServiceLoopThread;	     ///< thread that synchronizes transmit and receive sections

  //VectorFIFO  mTransmitFIFO;		      ///< FIFO that holds transmit bursts
  VectorFIFO  mReceiveFIFO;		      ///< FIFO that holds receive  bursts

  //signalVector* sendHistory;		      ///< block of previous transmitted samples
  signalVector* rcvHistory;		      ///< block of previous received samples

  USRPDevice *usrp;			      ///< the USRP object

  //signalVector* sendBuffer;		      ///< block of samples to be transmitted
  signalVector* rcvBuffer;		      ///< block of received samples to be processed

  //signalVector* sendLPF;		      ///< polyphase filter for resampling transmit bursts
  signalVector* rcvLPF;			      ///< polyphase filter for resampling receive bursts

  mutable Signal wroteRadioSignal;	      ///< signal that indicates samples sent to USRP
  //mutable Mutex  writingRadioLock;	      ///< mutex to lock receive thread when transmit thread is writing

  bool underrun;			      ///< indicates writes to USRP are too slow
  bool overrun;				      ///< indicates reads from USRP are too slow
  TIMESTAMP writeTimestamp;		      ///< sample timestamp of next packet written to USRP
  TIMESTAMP readTimestamp;		      ///< sample timestamp of next packet read from USRP

  RadioClock mClock;                          ///< the basestation clock!

  int samplesPerSymbol;			      ///< samples per GSM symbol
  int receiveOffset;                          ///< offset b/w transmit and receive GSM timestamps, in timeslots

  bool mOn;				      ///< indicates radio is on

  /** format samples to USRP */
  short *USRPifyVector(signalVector &wVector);

  /** format samples from USRP */
  signalVector *unUSRPifyVector(short *shortVector, int numSamples);

  /** push GSM bursts into the transmit buffer */
//  void pushBuffer(void);

  /** pull GSM bursts from the receive buffer */
  void pullBuffer(void);

public:

  /** start the interface */
  void start();

  /** constructor */
  RadioInterface(USRPDevice* wUsrp = NULL,
		 int receiveOffset = 3,
		 int wSamplesPerSymbol = SAMPSPERSYM,
		 GSM::Time wStartTime = GSM::Time(0));

  /** destructor */
  ~RadioInterface();

  /** check for underrun, resets underrun value */
  bool isUnderrun() { bool retVal = underrun; underrun = false; return retVal;}

  /** attach an existing USRP to this interface */
  void attach(USRPDevice *wUsrp) {if (!mOn) usrp = wUsrp;}

  /** return the transmit FIFO */
  //VectorFIFO* transmitFIFO() { return &mTransmitFIFO;}

  /** return the receive FIFO */
  VectorFIFO* receiveFIFO() { return &mReceiveFIFO;}

  /** return the basestation clock */
  RadioClock* getClock(void) { return &mClock;};

  /** set transmit frequency */
  bool tuneTx(double freq);

  /** set receive frequency */
  bool tuneRx(double freq);

protected:

  /** drive transmission of GSM bursts */
  //void driveTransmitRadio();

  /** drive reception of GSM bursts */
  void driveReceiveRadio();

  /** drive synchronization of Tx/Rx of USRP */
  //void alignRadio();

  /** reset the interface */
  void reset();

 // friend void *TransmitRadioServiceLoopAdapter(RadioInterface*);

  friend void *ReceiveRadioServiceLoopAdapter(RadioInterface*);

  //friend void *AlignRadioServiceLoopAdapter(RadioInterface*);

};

/** transmit thread loop */
//void *TransmitRadioServiceLoopAdapter(RadioInterface*);

/** receive thread loop */
void *ReceiveRadioServiceLoopAdapter(RadioInterface*);

/** synchronization thread loop */
//void *AlignRadioServiceLoopAdapter(RadioInterface*);



