/*
 * This Programme is writen for
 *
 *        Graduation Project
 *
 *      Mobile Communications Using USRP
 *
 *             2011
 *
 *
 * Supervisor DR.Alaa M. Abbas
 *
 *
 */

#include <stdint.h>
#include <stdio.h>
#include <Logger.h>
#include <Configuration.h>
#include "radioInterface.h"
#include <GSMCommon.cpp>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <math.h>
using namespace GSM;

////////////////////////
using namespace std;

ConfigurationTable gConfig;

int main(int argc, char *argv[]) {
   if (argc>1) gLogInit(argv[1]);
   else gLogInit("INFO");
   if (argc>2) gSetLogFile(argv[2]);
   USRPDevice *usrp = new USRPDevice(400e3);
   usrp->make();
   cout << "Usrp Is Maked.."<<endl;
   usrp->setTxFreq(890.0e6);
   cout << "TX Tuned"<<endl;
   int offset=200*(int)argv[3];
   double freq=(890000+45000+offset)*1000;
   usrp->setRxFreq(freq);
   cout << "RX Tuned to ARFCN "<< argv[3]<<endl;
   float energy=0.0;
   GSM::Time wStartTime(2,0);
   srandom(time(NULL));
   RadioInterface* radio = new RadioInterface(usrp,3,1,wStartTime);
   usrp->start();
   cout<<"Usrp Is Started " <<endl;
   usrp->updateAlignment(20000);
   usrp->updateAlignment(21000);
   radio->start();
   cout<<"Radio Started " <<endl;
   sleep(3);
   radio->receiveFIFO()->read();
   radio->receiveFIFO()->read();
   radio->receiveFIFO()->read();
   radio->receiveFIFO()->read();
   signalVector *rxsignal =radio->receiveFIFO()->read();
   cout<<"Data Readed " <<endl;
	 if (!rxsignal)
	 {
			cout<<"Data signal error"<<endl;
			delete rxsignal;
			radio=0;
			exit(0);
	 }
   signalVector::const_iterator itr = rxsignal->begin();
	 if(rxsignal->isRealOnly())
	 {
		for (;itr != rxsignal->end(); itr++)
		{
	     energy +=(itr->r * itr->r );
         cout<<" R= "<< itr->r <<" Energy= " <<energy << endl;
        }
	 }
	 else
	 {
        cout<<"the size of  Rxburst is "<<rxsignal->size();
        cout<<"\n\n______________________________________________________\n"
                 "|        Real Part (R)     |    Imaginary Part (+JX)   |\n"
                 "|__________________________|___________________________|\n";
		 for (;itr != rxsignal->end(); itr++) {
	     energy +=((itr->r * itr->r )+(itr->i * itr->i));
         cout<< "|       "<<setw(10)<< itr->r<<"         |         " <<setw(10)<< itr->i <<"        |\n";
	 }
         cout<<"|__________________________|___________________________|\n";
	 }

	  energy /= rxsignal->size();
      energy=sqrt(energy);
	  cout<<endl<<"Final Energy: "<<energy<<endl;
	  radio=0;
	  delete rxsignal;
      ofstream op_config("Energies.dat",ios::app);
      op_config<<energy<<endl;
      cout<<"Saved To Energies File"<<endl;
      op_config.close();

}
