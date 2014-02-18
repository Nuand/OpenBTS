#include <cstdio> // sprintf

#include "GSMLogicalChannel.h"
#include "GSML3RRMessages.h"

#include "RRLPQueryController.h"


#ifdef RRLP_TEST_HACK
// TEMP TEMP TEMP - so it gets to stdout
#define RRLP_LOG_DEBUG LOG(ERROR)
#define RRLP_LOG_NOTICE LOG(ERROR)
#define RRLP_LOG_INFO LOG(ERROR)
#else
#define RRLP_LOG_DEBUG LOG(DEBUG)
#define RRLP_LOG_NOTICE LOG(NOTICE)
#define RRLP_LOG_INFO LOG(INFO)
#endif

// TODO: doing this namespace thing because I don't remember the right way.
namespace GSM {
namespace RRLP {

////////////////////////////////////////////////////////////////////////////////
//                              RRLPQueryManager

// Helper - should go in the class probably
long getNumWithDefault(const std::string& key, long default_value) {
    if (gConfig.defines(key))
        return gConfig.getNum(key);
    return default_value;
}

RRLPQueryManager::RRLPQueryManager(unsigned accuracy)
    : m_accuracy(accuracy)
    , m_numQueries(0)
    , m_numPositionResponses(0)
{
    // override from gConfig
    m_accuracy = getNumWithDefault("RRLP.Accuracy", m_accuracy);
}

RRLPQueryManager* RRLPQueryManager::instance()
{
    static RRLPQueryManager* instance = 0;
    if (instance == 0) {
        instance = new RRLPQueryManager(60 /*accuracy*/);
    }
    return instance;
}

PositionResult RRLPQueryManager::doTransaction(L3MobileIdentity mobID, LogicalChannel* chan
                    , unsigned accuracy)
{
    if (accuracy == 0)
        accuracy = m_accuracy;
    if (m_data.count(mobID) == 0) {
        // first time
        m_data.insert(std::pair<L3MobileIdentity, RRLP_MS_Data>(mobID, RRLP_MS_Data()));
    }
    RRLP_MS_Data& data = m_data[mobID];
    data.lastSentReferenceNumber = 1 + (data.lastSentReferenceNumber % 7);
    RRLPQueryController QC(chan, data.lastSentReferenceNumber, accuracy);
    return record(QC.doTransaction());
}

PositionResult RRLPQueryManager::doTransaction(L3MobileIdentity mobID, LogicalChannel* chan
                    , BitVector& rrlp_position_request)
{
    // we don't store anything in this case. maybe TODO?
    RRLPQueryController QC(chan, rrlp_position_request);
    return record(QC.doTransaction());
}

PositionResult RRLPQueryManager::record(PositionResult res)
{
    m_numQueries ++;
    if (res.mValid) m_numPositionResponses++;
    return res;
}

////////////////////////////////////////////////////////////////////////////////
//                              RRLPQueryController

// Create an RRLP Query with certain configurable params.
// requests one set of location, GPS, msBased only, within 2 seconds (2**1).
// referenceNumber - number between 1 and 7 (0 is illegal)
// accuracy - 60 is about 400 meters, generally logarithmic scale. (TODO - add reference)
void buildRRLPQueryFromParams(BitVector& out, unsigned referenceNumber, unsigned int accuracy)
{
    if (referenceNumber > 7 || referenceNumber == 0) {
        LOG(ERROR) << "referenceNumber = " << referenceNumber;
        referenceNumber = 1;
    }
    if (accuracy < 10) {
        LOG(ERROR) << "accuracy = " << accuracy;
        accuracy = 60;
    }
    // 101 0 000 0 00000 0 01 0111100  01 001 1 000
    out.resize(32);
    out.fillField(0, 1, 32);
    size_t wp = 0;
    out.writeField(wp, referenceNumber, 3);
    // extension bit - leave as 0
    wp ++;
    // a msrPositionReq, encoded as 0 in 3 bits (choice)
    wp += 3;
    // no extension
    wp += 1;
    // no optional components
    wp += 5;
    // PositionInstruct | no extension
    wp += 1;
    // PositionInstruct | msBased methodType
    out.writeField(wp, 1, 2);
    // Accuracy - 7 bits
    out.writeField(wp, accuracy, 7);
    // PositionMethod - GPS
    out.writeField(wp, 1, 2);
    // Measure Response time - actually 2^x where we put down x
    out.writeField(wp, 1, 3);
    // use one or multiple sets for response - we say 1
    out.writeField(wp, 1, 1);
    // The left 3 bits are fillers to octet boundary
    LOG(DEBUG) << "buildRRLPQueryFromParams: ref " << referenceNumber << ", acc "
              << accuracy << ";built " << out;
}

void RRLPQueryController::init(unsigned reference)
{
    if (reference > 7 || reference == 0) {
        LOG(ERROR) << "Programming error: reference must be in the range [1,7]";
    }
    m_reference = reference;
    m_timeout = 1000; // |- These are defaults, overridable
    m_retries = 5;    // |
    m_retrans_on_ack = false;
    m_continue = true;
    // initialize assistance data
    m_assistance_data.resize(2*8);
    m_assistance_data.fillField(0, 0, 16);
    // reference number in high 3 bits
    // component number in zeros are for no optional components, no extensions.
    m_assistance_data.fillField(0, m_reference, 3);
    // set component type to ASSISTANCE_DATA (bits 4,5,6)
    m_assistance_data.fillField(4, ASSISTANCE_DATA, 3);
}

RRLPQueryController::RRLPQueryController(LogicalChannel* chan,
    unsigned int referenceNumber, unsigned int accuracy)
    : m_chan(chan)
{
    init(referenceNumber);
    buildRRLPQueryFromParams(m_rrlp_position_request, m_reference, accuracy);
    LOG(DEBUG) << "Reference is " << referenceNumber << ", Assistance is " << m_assistance_data;
}

RRLPQueryController::RRLPQueryController(LogicalChannel* chan,
    const BitVector rrlp_position_request)
        : m_chan(chan)
        , m_rrlp_position_request(rrlp_position_request)
{
    init(m_rrlp_position_request.peekField(0, 3));
}

void RRLPQueryController::setReferenceNumber(unsigned reference)
{
    m_reference = reference;
    m_assistance_data.fillField(0, m_reference, 3);
    m_rrlp_position_request.fillField(0, m_reference, 3);
}

void RRLPQueryController::sendRequest()
{
    RRLP_LOG_INFO << "sending request " << m_rrlp_position_request;
    m_chan->send(L3ApplicationInformation(m_rrlp_position_request));
}
void RRLPQueryController::sendAssistanceData()
{
    RRLP_LOG_INFO << "sent assistance data " << m_assistance_data;
    m_chan->send(L3ApplicationInformation(m_assistance_data));
}

void RRLPQueryController::parseLocationInfo(size_t &pos, BitVector& rrlp)
{
    // Reference Frame
    unsigned refFrame = rrlp.readField(pos, 16);
    if (refFrame != 65535) {
        // TODO - shouldn't be INFO
        RRLP_LOG_INFO << "refFrame != 65535";
    }
    // TOW (Time of week) OPTIONAL
    unsigned TOW_Optional = rrlp.readField(pos, 1);
    unsigned TOW = 0;
    if (TOW_Optional)
        TOW = rrlp.readField(pos, 24);
    else
        RRLP_LOG_NOTICE << "unexpected non existance of TOW field in MsrPositionRsp";
    // 1 fixType - (0 is 2D, 1 is 3D) 1
    unsigned fixType = rrlp.readField(pos, 1);
    // 5 size of Ext-GeographicalInformation 0..20 (5 bits) 01101
    unsigned ExtGeographicalInformation_Size = rrlp.readField(pos, 5);
    // 4 Kind of Ext-GeographicalInformation. 9 is 1001
    unsigned ExtGeographicalInformation_Kind = rrlp.readField(pos, 4);
    // 4 0000
    pos += 4; // Spare bits
    // The following fields should be there regardless, but still better safe then sorry
    if (rrlp.size() - pos < 48) {
        LOG(ERROR) << "unexpectedly small MsrPositionRsp, missing Lat/Lon";
        return;
    }
    // 1 Latitude Sign 0
    unsigned LatSign = rrlp.readField(pos, 1);
    // 23 Latitude 01101011011010011100011
    unsigned Lat23 = rrlp.readField(pos, 23);
    // 24 Longitude 101010001111001001010011
    unsigned Lon24 = rrlp.readField(pos, 24);

    // Finally, store this
    m_pr.mValid = true;
    m_pr.mPos.mLat = (1.0 - LatSign*2.0) * Lat23 * 90.0 / (1 << 23);
    m_pr.mPos.mLon = Lon24 * 360.0 / (1 << 24);

    // Now we actually branch according to the kind - but we don't actually care for this right now.
    // so just log this stuff.
    RRLP_LOG_INFO << "unparsed " << (rrlp.size() - pos) << " bits (following the Lat/Lon) of a " << ExtGeographicalInformation_Kind;

    // 1 Altitude Direction 1
    // 15 Altitude 000000000010001
    // 1 Spare 0
    // 7 Uncertainty semi-major 0000110
    // 1 Spare 0
    // 7 Uncertainty semi-minor 0000110
    // 8 Orientatin of major axis 00110010
    // 1 Spare 0
    // 7 Uncertainty Altitude 0001000
    // 1 Spare 0
    // 7 Confidence 1000100
    // 2 Padding (to make length a multiple of 8) 00
}

/**
    Implemente decoding of a Measurement Position Response.
    conservative - decode what we know, complain but don't interfer
    if we don't.
    what we want to know:
    parsing a packet with a LocationInfo
    parsing a packet that says not enough satellites
    parsing a packet that says in need of assistance data
    we only do something with the first: record the location in m_pr
    TODO - this should follow the structure of OpenBTS code, namely this
    code should be in some sort of Message Heirarchy for RRLP (minimal,
    but with a base class etc.) and this function should be the decoder
    for a MsrPositionRsp class instance.

    TODO: The input is a L3Frame basically, but I'm using BitVector type for easy tester
     - actually not sure if I need to.
*/
void RRLPQueryController::parseMsrPositionResponse(BitVector& rrlp)
{
    // Example:
    // 0,24 Header, not interesting (checked by caller) 000001100011100000000000
    // 24,8 Length of rest of packet 00010110
    size_t pos = 32; // start by skipping the APDU header and the TV header for the PDU
    // Reference Number
    pos += 3;
    // PDU Extension bit
    unsigned PDU_Extension= rrlp.readField(pos, 1);
    RRLP_LOG_DEBUG << rrlp.segment(0, 32);
    RRLP_LOG_DEBUG << rrlp.segment(32, rrlp.size() - 32);

    if (PDU_Extension) {
        RRLP_LOG_NOTICE << "Not handling an extended PDU";
        return;
    }
    // Component Selection
    if (rrlp.readField(pos, 3) != MSR_POSITION_RSP) {
        LOG(ERROR) << "parseMsrPositionResponse called with a non MsrPositionRsp packet";
        return;
    }
    // MsrPositionRsp Extension bit
    unsigned MsrPositionRsp_Extension = rrlp.readField(pos, 1);
    if (MsrPositionRsp_Extension) {
        RRLP_LOG_NOTICE << "Not handling an extended MsrPositionRsp";
        return;
    }
    // MsrPositionRsp Optional bits
    unsigned MultipleSets_Optional = rrlp.readField(pos, 1);
    unsigned ReferenceIdentity_Optional = rrlp.readField(pos, 1);
    unsigned OTD_MeasureInfo_Optional = rrlp.readField(pos, 1);
    unsigned LocationInfo_Optional = rrlp.readField(pos, 1);
    unsigned GPS_MeasureInfo_Optional = rrlp.readField(pos, 1);
    unsigned LocationError_Optional = rrlp.readField(pos, 1);
    unsigned ExtensionContainer_Optional = rrlp.readField(pos, 1);
    // The ones before the handled components are catastrophic, since they take up
    // a yet unknown number of bits (because we didn't read the spec and implement it).
    // The ones after are not so bad, since they won't bother us, we'll just ignore them.
    if (MultipleSets_Optional || ReferenceIdentity_Optional || OTD_MeasureInfo_Optional) {
        LOG(ERROR) << "UNIMPLEMENTED optional component in MsrPositionResponse, packet unparsed (Multi, Ref, OTD): "
                   << MultipleSets_Optional << ", " << ReferenceIdentity_Optional << ", " << OTD_MeasureInfo_Optional << ": at " << pos;
        return;
    }
    if (GPS_MeasureInfo_Optional || ExtensionContainer_Optional || LocationError_Optional) {
        LOG(ERROR) << "UNIMPLEMENTED optional component in MsrPositionResponse, information thrown (GPSMeasure, Extension, Error): "
                   << GPS_MeasureInfo_Optional << ", " << ExtensionContainer_Optional << ", " << LocationError_Optional;
    }
    // TODO: basically parsing is pretty simple - for any optional bit, BY ORDER, you parse the packet with a delegate function.
    // right now just LocationInfo is parsed.
    if (LocationInfo_Optional)
        parseLocationInfo(pos, rrlp);

    // 0,3 Reference Number 001
    //          don't care right now (also, handled by caller)
    // 3,1 RRLP-Component Extension bit 0
    // 4,3 RRLP-Component Selection 001
    //          Should be 1 for MsrPositionResponse (checked by caller)
    // 7,1 MsrPosition-Rsp Extension bit 0
    // 8,1 MultipleSets exists 0
    // 9,1 ReferenceIdentify exists 0
    // 10,1 OTD-MeasureInfo exists 0
    // 11,1 LocationInfo exists 1
    // 12,1 GPS-MeasureInfo exists 0
    // 13,1 LocationError exists 0
    // 14,1 ExtensionContainer exists 0
    // 15,16 refFrame (0..65535) 1111111111111111
    // 31,1 gpsTOW existance 1
    // 32,24 gpsTOW (0..14399999 hence 24 bits) 010010101111101111011110
    // 56,1 fixType - (0 is 2D, 1 is 3D) 1
    // 57,5 size of Ext-GeographicalInformation 0..20 (5 bits) 01101
    // 62,4 Kind of Ext-GeographicalInformation. 9 is 1001
    // 66,4 0000
    // 70,1 Latitude Sign 0
    // 71,23 Latitude 01101011011010011100011
    // 94,24 Longitude 101010001111001001010011
    // 118,1 Altitude Direction 1
    // 119,15 Altitude 000000000010001
    // 134,1 Spare 0
    // 135,7 Uncertainty semi-major 0000110
    // 142,1 Spare 0
    // 143,7 Uncertainty semi-minor 0000110
    // 150,8 Orientatin of major axis 00110010
    // 158,1 Spare 0
    // 159,7 Uncertainty Altitude 0001000
    // 166,1 Spare 0
    // 167,7 Confidence 1000100
    // 174,2 Padding (to make length a multiple of 8) 00
}

/** Return: True if successful, False otherwise (means retry or fail completly)
 */
bool RRLPQueryController::parseSingleAPDU(L3Frame* resp)
{
    // TODO - this is basically parsing the RRLP - refactor
    bool component_extension = resp->peekField(32+3, 1) == 1;
    unsigned rrlp_component = (component_extension ? -1 :
            resp->peekField(32+3+1, 3)); // 32 = 24 APDU + 8 TV Length,
                    // 3 = Reference Number, 1 Extension for Component.
                    // 3 bits for the Component enum (in case of no extension)
    if (rrlp_component == ASSISTANCE_DATA_ACK) {
        LOG(INFO) << "Unexpected Assistance Data Acknowledge.";
        if (m_retrans_on_ack)
            sendRequest();
        return true;
    }
    if (rrlp_component != MSR_POSITION_RSP) {
        LOG(INFO) << "Unexpected RRLP response, component " << rrlp_component;
        return true;
    }
    parseMsrPositionResponse(*resp);
    return false;
}

const char* RR_MTI_To_String(unsigned MTI)
{
    switch (MTI) {
        case 18: return "RR STATUS";
    };
    static char buf[1024];
    sprintf(buf, "unknown %d", MTI);
    return buf;
}

/** Return: True if successful, False otherwise (means retry or fail completly)
 */
bool RRLPQueryController::doSingleReceive()
{
	// Receive an L3 frame with a timeout.  Timeout is RRLP max + 1 second.
	L3Frame* resp = m_chan->recv(m_timeout);
	if (!resp) {
		LOG(INFO) << "timed out on recieve";
        return true;
	}

    bool retry = false;

    // TODO: <rant> This is really ugly. Basically we just want to parse the response. But
    // this is complicated by the fact that some phones (G1..) return unexpected
    // messages. This is technically allowed (they can say anything on the channel),
    // so I think the solution should be systematic.
    // </rant>

    // Retry (up to limit of 3) Wehn:
    //  * We get GPRS Suspension Requests for some reason from the G1
    //  * We get an RRLP Assistance Data Ack even though we haven't sent any
    const unsigned PD_RR = 6;
    const unsigned MTI_RR_GPRS_SUSPENSION_REQUEST = 52;
    const unsigned MTI_RR_STATUS = 18;
    const unsigned MTI_RR_APDU = 56;
    unsigned cause = 0; // only used in case of MTI_RR_STATUS
	if (resp) {
        LOG(INFO) << "received " << *resp;
        if (resp->primitive() == DATA && resp->PD() == PD_RR) {
            // Is an RR
            switch (resp->MTI()) {
                case MTI_RR_GPRS_SUSPENSION_REQUEST:
                    LOG(INFO) <<
                        "received GPRS SUSPENSION REQUEST, rereceiving";
                    retry = true;
                    break;
                case MTI_RR_APDU:
                    retry = parseSingleAPDU(resp);
                    break;
                case MTI_RR_STATUS:
                    cause = resp->peekField(16, 8);
                    switch (cause) {
                        case 97:
                            LOG(INFO) << "MS says: message not implemented";
                            m_continue = false;
                            break;
                        case 98:
                            LOG(INFO) << "MS says: message type not compatible with protocol state";
                            m_continue = false;
                            break;
                        default:
                            // never saw this.
                            LOG(INFO) <<
                                "unknown RR_STATUS response, cause = "
                                      << cause;
                            retry = true;
                    }
                    break;
                default:
                    LOG(INFO) << "received unexpected RR Message "
                              << RR_MTI_To_String(resp->MTI());
            }
        } else {
            // Not RR
            LOG(INFO) << "didn't receive a RR message, or primitive != DATA";
            retry = true;
        }
	}
	delete resp;
	return retry;
}

PositionResult RRLPQueryController::doTransaction()
{
    // optional parameters, defaults used otherwise (see constructor)
    if (gConfig.defines("RRLP.Retries"))
        m_retries = gConfig.getNum("RRLP.Retries");
    if (gConfig.defines("RRLP.Timeout"))
        m_timeout = gConfig.getNum("RRLP.Timeout");
    LOG(INFO) << "doTransaction: Assistance " << m_assistance_data
              << ", Query " << m_rrlp_position_request
              << ", " << m_retries << "x" << m_timeout;
	m_pr.mValid = false;
    sendAssistanceData();
    m_retrans_on_ack = false; // don't retrans after the assistance data.
    doSingleReceive();
    if (!m_continue) return m_pr; // stopped short on account of not implemented probably.
    m_retrans_on_ack = true; // now we do want retrans. might wake up the damn thing.
    if (m_pr.mValid) return m_pr; // shouldn't happen, but I don't want to throw it if I get it.
    sendRequest();
    while (m_continue && m_retries-- > 0 && doSingleReceive()) {};
    return m_pr;
}

////////////////////////////////////////////////////////////////////////////////
// Testing Constructor - parse a message

static BitVector g_testing_vec_char;

RRLPQueryController::RRLPQueryController(BitVector& pdu)
    : m_rrlp_position_request(g_testing_vec_char)
{
    parseMsrPositionResponse(pdu);
}

}; // namespace RRLP
}; // namespace GSM
