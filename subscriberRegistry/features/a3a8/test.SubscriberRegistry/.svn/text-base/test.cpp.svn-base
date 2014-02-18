#include <string.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include "../SubscriberRegistry.h"
#include "Configuration.h"

using namespace std;


ConfigurationTable gConfig("test.db");

SubscriberRegistry sr;

void foo(uint32_t n, string s)
{
	LOG(INFO) << sr.uintToString(n) << " " << s;
}

void foo(string s)
{
	uint64_t h;
	uint64_t l;
	sr.stringToUint(s, &h, &l);
	LOG(INFO) << sr.uintToString(h, l) << " " << s;
}

int main(int argc, char **argv)
{
	gLogInit("SubscriberRegistryTest",gConfig.getStr("Log.Level").c_str(),LOG_LOCAL7);

	// The idea is just to make sure things are connected right.  The important code is shared, and tested elsewhere.


	// add a user
	sr.addUser("imsi", "clid");
	// testing mappings of known user
	sr.getCLIDLocal("imsi");
	sr.getIMSI("clid");
	// test mapping of unknow user (so it won't be found locally)
	sr.getCLIDLocal("imsi_unknown");
	sr.getRandForAuthentication(false, "imsi_r1");
	sr.authenticate(false, "imsi_a1","rand_a1","sres_a1");


	// but test the conversions
	foo(0xffffffff, "ffffffff");
	foo(0x00000000, "00000000");
	foo(0x12345678, "12345678");
	foo(0x9abcdef0, "9abcdef0");

	foo("ffffffffffffffff0000000000000000");
	foo("0000000000000000ffffffffffffffff");
	foo("0123456789abcdef0123456789abcdef");


	// billing testing - not tested elsewhere

	sr.setPrepaid("imsi", false);
	bool b;
	sr.isPrepaid("imsi", b);
	LOG(INFO) << "should be false " << b;

	sr.setPrepaid("imsi", true);
	sr.isPrepaid("imsi", b);
	LOG(INFO) << "should be true " << b;

	sr.setSeconds("imsi", 100);
	int t;
	sr.secondsRemaining("imsi", t);
	LOG(INFO) << "should be 100 " << t;

	sr.addSeconds("imsi", -50, t);
	LOG(INFO) << "should be 50 " << t;

	sr.addSeconds("imsi", -100, t);
	LOG(INFO) << "should be 0 " << t;


}
