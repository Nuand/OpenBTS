#ifndef __COLLECT_MS_INFO_H__
#define __COLLECT_MS_INFO_H__

#include <vector>

#include "GSML3CommonElements.h"

namespace GSM {
    class LogicalChannel;
    class L3MobileIdentity;

    namespace RRLP {
        struct Position {
            double mLat;
            double mLon;
        };

        struct PositionResult {
            bool mValid;
            Position mPos;
        };

        // You can use an empty mobID or a non existant one, it will still work.
        PositionResult doRRLPQuery(L3MobileIdentity mobID, LogicalChannel* chan
                                   ,BitVector& rrlp_position_request);
        PositionResult doRRLPQuery(L3MobileIdentity mobID, LogicalChannel* chan
                                   ,unsigned int accuracy);
        void doMultipleRRLPQueries(LogicalChannel* chan);
	void collectMSInfo(L3MobileIdentity, LogicalChannel*, bool withRRLP);
        void logMSInfo(LogicalChannel*, const PositionResult& pr, L3MobileIdentity);
    }
};

#endif // __COLLECT_MS_INFO_H__

