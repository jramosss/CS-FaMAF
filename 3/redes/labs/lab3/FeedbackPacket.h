#ifndef FEEDBACKPACKET_H
#define FEEDBACKPACKET_H

#include <omnetpp.h>

using namespace omnetpp;

class FeedbackPacket : public cPacket {
    private:
        int remainingBuffer = 0;
        simtime_t packet_ts;
    public:
        virtual int        getRemainingBuffer() {return this->remainingBuffer;};
        virtual void       setRemainingBuffer(int size) {this->remainingBuffer = size;};
        virtual simtime_t  getPacketTimeStamp () {return this->packet_ts;};
        virtual void       setPacketTimeStamp (simtime_t ts) {this->packet_ts = ts;};
        virtual            ~FeedbackPacket ();
        FeedbackPacket();
};



#endif
