/*
 * This Programme is writen By
 *
 *       Eng\Mohammed Ibrahim Elsabagh
 *             
 *             Communication Engineer
 *
 *                 2011
 *                     
 *
 *
 */

#include <sstream>
#include <signal.h>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <string.h>
#include <vector>
#include <iomanip>

using namespace std;

//const float THRESHOLD = 62500.0F; //For future used

class Cognitve
{
private:
	float energy;
	int __ARFCN;

public:

    Cognitve(int arfcn):__ARFCN(arfcn),energy(0.0F)
       {}

    void SetEnergy(float rs);
    bool valid();
    float GetEnergy();
    int Getarfcn();
    friend bool operator<(const Cognitve&, const Cognitve&);
    friend bool operator==(const Cognitve&, const Cognitve&);
    friend bool operator>(const Cognitve&, const Cognitve&);
};

bool Cognitve::valid()
{
	if(energy<6250000)
	{
		cout<<"Found The best ARFCN "<<__ARFCN <<" with Energy "<<energy<<endl;
		return true;
	}
	return false;
}

    float  Cognitve::GetEnergy()
{
return energy;
}
    int Cognitve::Getarfcn()
{
return __ARFCN;
}
bool operator<(const Cognitve& f1, const Cognitve& f2)
{
	return (f1.energy<f2.energy)? true : false;

}
bool operator==(const Cognitve& f1, const Cognitve& f2)
{
	return (f1.energy==f2.energy)? true :false;
}
bool operator>(const Cognitve& f1, const Cognitve& f2)
{
	return (f1.energy>f2.energy)? true : false;
}

class LowRSSI
{
public:
	bool operator() (const Cognitve* f1,const Cognitve* f2) const
	{
		return (*f1<*f2);
	}
};


void Cognitve::SetEnergy(float rs)
{
energy=rs;
}

void restartcr(int __ARFCN)
{
	//change /home/elsabagh/CRF3 >>> to current Cognitve Radio folder
    std::string cmd="/home/elsabagh/CRF3/openbts-2.6.0Mamou/Transceiver/USRPping  DEEPDEBUG crout ";
    cout<<"cmd "<<cmd;
    std::string s1;
    std::stringstream out;
    out << __ARFCN;
    s1 = out.str();
    std::string fcmd = cmd + s1;
    system(fcmd.c_str());
}



void setFreq(int __ARFCN)
{
	    vector<std::string> Configers;
	    //change /home/elsabagh/  >>>to orignal>> Openbts folder
		ifstream ip_config("/home/elsabagh/openbts-2.6.0Mamou/apps/OpenBTS.config");
		std::string orgarfcn="GSM.ARFCN ";
		if (!ip_config)
		{
			cout << "Error: Open Configure OpenBTS File " << endl;
	        return;
		}
		while (ip_config)
		{
			std::string config;
			getline(ip_config,config);
	        Configers.push_back(config);
		}
		ip_config.close();
		if(Configers.size()<15)
			{
			cout << "Error: Configures is Less Than 15 Configure " << endl;
	        return;
	        }
        cout<<"Read configures suceessed of OpenBTS. read#  "<<Configers.size()<<endl;
		for(int j=0; j< Configers.size(); j++)
			{
			if (Configers[j].size()>10)
		    {

			 if (Configers[j].find(orgarfcn)==0)
				 {
                  std::string s1;
                  std::stringstream out;
                  out << __ARFCN;
                  s1 = out.str();
		          Configers[j]=orgarfcn +s1;
	             }
	          }
	        }
	    //change /home/elsabagh/  >>>to orignal>> Openbts folder
	 ofstream op_config("/home/elsabagh/openbts-2.6.0Mamou/apps/OpenBTS.config");
	 for (int ix=0;ix<(int)Configers.size();ix++)
	 {
	 	 op_config<<Configers[ix]<<endl;
	 }
	op_config.close();
	cout<<"Set Freq To "<<__ARFCN<<endl;
    return;
}


void setFreqorg(int __ARFCN)
{
	    vector<std::string> Configers;
		ifstream ip_config("OpenBTS.config");
		std::string orgarfcn="GSM.ARFCN ";
		if (!ip_config)
		{
			cout << "Error: Open Configure OpenBTS File in this folder " << endl;
	        return;
		}
		while (ip_config)
		{
			std::string config;
			getline(ip_config,config);
	                Configers.push_back(config);
		}
		ip_config.close();
		if(Configers.size()<15)
			{
			cout << "Error: Configures is Less Than 15 Configure " << endl;
	        return;
	        }
          cout<<"Read configures suceessed of OpenBTS. read#  "<<Configers.size()<<endl;
		for(int j=0; j< Configers.size(); j++)
			{
			if (Configers[j].size()>10)
		    {

			 if (Configers[j].find(orgarfcn)==0)
				 {
                 std::string s1;
                 std::stringstream out;
                 out << __ARFCN;
                s1 = out.str();
		    Configers[j]=orgarfcn +s1;
	             }
	          }
	        }
	 ofstream op_config("OpenBTS.config");
	 for (int ix=0;ix<(int)Configers.size();ix++)
	 {
	 	 op_config<<Configers[ix]<<endl;
	 }
	op_config.close();
	cout<<"Set Freq To "<<__ARFCN<<endl;
    return;
}



int main() {
int start=2;
int duration=10;
   cout<<"\n\n                 Welcome To Cognitve Radio Energy Detector In GSM Band             \n"
         "\n"
         "This Programme Is Created By: \n"
         "    \n\n"
         "                           Eng\Mohammed Ibrahim Elsabagh    \n\n"
         "                                     Communication Engineer      \n \n"
         "                              TEL(Egypt):20-0126575312  \n\n"
         "Please Enter The Start ARFCN Number:";
cin>> start;
cout<<"\nPlease Enter the Number Of ARFCN To Scan: ";
cin>>duration;
srandom(time(NULL));
ofstream op_config("Energies.dat");
op_config<<"";
op_config.close();

vector<Cognitve*> vfreqs;

         for(int i=start;i<start+duration;i++)
            {

               vfreqs.push_back(new Cognitve(i));
               cout<<"Restarting The Transceiver...."<<endl;
               restartcr(i);
             }

                  vector<std::string> RSSIS;
		  ifstream ip_config("Energies.dat");
		if (!ip_config)
		{
			cout << "Error: open Energies file " << endl;
                        exit(0);
		}
		cout<<"Reading The Received Signal Energy"<<endl;
		while (ip_config)
		{
			std::string config;
			getline(ip_config,config);
                if(config!="")
                {
	        RSSIS.push_back(config);
                }
	      	}
		ip_config.close();

		if(RSSIS.size()<1)
			{
			cout << "Error: Energies is less than 1 " << endl;
	                exit(0);
			}
         else if (RSSIS.size()!=duration)
             {

			    cout << "Error:  Config file number not match " <<RSSIS.size()<< endl;
                            exit(0);
             }

		for(int j=0; j<(int)RSSIS.size(); j++)
	     {
                std::string s12=RSSIS[j];
                int f11 = strtod(s12.c_str(), NULL);
                float f22=(float)f11;
		(*vfreqs[j]).SetEnergy(f22);
       	     }
    sort(vfreqs.begin(),vfreqs.end(),LowRSSI());

cout<<"____________________________________________________________________\n"
      "|        |                      |                    |              |\n"
      "| ARFCN  | Downlink Freq (MHz)  |  Uplink Freq (MHz) |    Energy    |\n"
      "|________|______________________|____________________|______________|\n"
      "|        |                      |                    |              |\n";

    for(int lp=0;lp<vfreqs.size();lp++)
{
int marfcn=(*vfreqs[lp]).Getarfcn();
   float offset=200*marfcn;
   float dfreq=(890000+45000+offset)/1000;
if (marfcn<10)
{
cout<<"| "<<marfcn<<showpoint<<"      |       "<<dfreq<<"        |      "<<(dfreq-45)<<"       |  "<<setw(10)<<(*vfreqs[lp]).GetEnergy()<<"  |\n";
cout<<"|                                                                   |\n";
}
else if( marfcn<100)
{
cout<<"| "<<marfcn<<showpoint<<"     |       "<<dfreq<<"        |      "<<(dfreq-45)<<"       |  "<<setw(10)<<(*vfreqs[lp]).GetEnergy()<<"  |\n";
cout<<"|                                                                   |\n";
}
else
{
cout<<"| "<<marfcn<<showpoint<<"    |       "<<dfreq<<"        |      "<<(dfreq-45)<<"       |  "<<setw(10)<<(*vfreqs[lp]).GetEnergy()<<"  |\n";
cout<<"|                                                                   |\n";
}

}
cout<<"|___________________________________________________________________|\n";

  	if((*vfreqs[0]).valid())
  	{
cout<<"The least Energy  "<<(*vfreqs[0]).GetEnergy()<<endl;
  	setFreq((*vfreqs[0]).Getarfcn());
setFreqorg((*vfreqs[0]).Getarfcn());
  	}

char ans;
cout<<"Do You Want To Run OpenBTS?";
cin>>ans;
if(ans=='y')
{
	//change /home/elsabagh to >>>>orignal OpenBTS programm
system("cd /home/elsabagh/openbts-2.6.0Mamou/apps/");
system("/home/elsabagh/openbts-2.6.0Mamou/apps/OpenBTS");
}


}






