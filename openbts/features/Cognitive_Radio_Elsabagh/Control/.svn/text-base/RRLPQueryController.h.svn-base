#ifndef __RRLP_QUERY_CONTROLLER_H__
#define __RRLP_QUERY_CONTROLLER_H__

#include "BitVector.h"
#include "CollectMSInfo.h" // for PositionResult

/**
    The classes here are divided into high level interface and lower level.

    The higher level is the RRLPQueryManager, it allows handling MS's that
    don't understand RRLP and changing the reference number between queries.

    It uses the lower level RRLPQueryController that sends and receives
    messages on a LogicalChannel.
 */

namespace GSM {
    
    class L3Frame;

    namespace RRLP {

        struct RRLP_MS_Data {
            unsigned int lastSentReferenceNumber;
        };

        // Singleton in charge of all RRLP queries done by OpenBTS. Here to
        // allow state between queries to the same machine.
        // Main functions right now:
        //  * keep track of machines that don't support RRLP, and don't bug
        //    them.
        //  * change reference number between request, as a strategy to maximize
        //    number of responses containing GPS coordinates (works somewhat with
        //    G1 phone running android 1.5).
        class RRLPQueryManager {
        private:
            std::map<L3MobileIdentity, RRLP_MS_Data> m_data;
            unsigned m_accuracy;

            unsigned m_numQueries;
            unsigned m_numPositionResponses; // the shit

            RRLPQueryManager(unsigned accuracy);

            PositionResult record(PositionResult); // helper to do recording in one place.

        public:
            unsigned numQueries() { return m_numQueries; }
            unsigned numPositions() { return m_numPositionResponses; }

            /** return singleton instance
             */
            static RRLPQueryManager* instance();

            /**
             */
            PositionResult doTransaction(L3MobileIdentity, LogicalChannel*, unsigned accuracy=0);
            /** Same but with a user provided query. Will not touch the reference number.
             */
            PositionResult doTransaction(L3MobileIdentity, LogicalChannel*, BitVector&);


            // Setter/Getter for accuracy
            void setAccuracy(unsigned accuracy) {
                m_accuracy = accuracy;
            }
            unsigned accuracy() { return m_accuracy; }
        };



        // Low level - a single query

        void buildRRLPQueryFromParams(BitVector& out, unsigned int referenceNumber, unsigned int accuracy);

        // RRLP Packet Elements definition

        typedef enum {
            MSR_POSITION_REQ = 0,
            MSR_POSITION_RSP,
            ASSISTANCE_DATA,
            ASSISTANCE_DATA_ACK,
            PROTOCOL_ERROR
        } RRLP_PDU_COMPONENT;

        class RRLPQueryController {
        private:
            void sendRequest();
            void sendAssistanceData();
            bool doSingleReceive();
            bool parseSingleAPDU(L3Frame*);
            void parseMsrPositionResponse(BitVector&); // TODO - shouldn't get the L3Frame, but just the part it wants - BitVector::segment?
            void parseLocationInfo(size_t &, BitVector&);

            // Initialization variables

            LogicalChannel* m_chan;
            BitVector m_rrlp_position_request;
            BitVector m_assistance_data;
            unsigned m_timeout;     // milliseconds for each receive
            unsigned m_retries;     // number of retries, total timeout = m_timeout*m_retries
            unsigned m_reference;   // reference number for RRLP we send, needs to be consistent
                                    // between assistance message (albeit empty) and
                                    // position request.
            // State variables
            bool m_retrans_on_ack;
            bool m_continue; // cheap way to stop the transaction from a bottom function.

            // Output variables
            PositionResult m_pr;
        private:
            // helper called by constructors
            void init(unsigned reference);

       public:
            // Constructor for testing purposes only
            RRLPQueryController(BitVector& pdu);

            // Real usage constructors
            // Give query parameters
            RRLPQueryController(LogicalChannel* chan,
                unsigned int referenceNumber, unsigned int accuracy);

            // Give query as hex string
            RRLPQueryController(LogicalChannel* chan,
                const BitVector rrlp_position_request);

            PositionResult doTransaction();

            // API meant for RRLPQueryManager
            void setReferenceNumber(unsigned reference);
        };

    }; // namespace RRLP
}; // namespace GSM


#endif // __RRLP_QUERY_CONTROLLER_H__

