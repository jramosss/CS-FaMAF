#ifndef TRANSPORTRX
#define TRANSPORTRX

#include <omnetpp.h>
#include <string.h>
#include "FeedbackPacket.h"

using namespace omnetpp;

class TransportRx: public cSimpleModule {
private:
    cQueue buffer;
    cMessage *endServiceEvent;
    simtime_t serviceTime;
    cOutVector bufferSizeVector;
    cOutVector packetDropVector;

public:
    TransportRx();
    virtual ~TransportRx();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(TransportRx);

TransportRx::TransportRx() {
    endServiceEvent = NULL;
}

TransportRx::~TransportRx() {
    cancelAndDelete(endServiceEvent);
}

void TransportRx::initialize() {
    buffer.setName("bufferRx");
    endServiceEvent = new cMessage("endService");
    bufferSizeVector.setName("rxBufferSize");
    packetDropVector.setName("rxDroppedPkt");
}

void TransportRx::finish() {
}

void TransportRx::handleMessage(cMessage* msg){
    if (msg == endServiceEvent) {
        if (!this->buffer.isEmpty()){
            cPacket* pkt = (cPacket*)this->buffer.pop();
            send(pkt,"toApp");
            serviceTime = pkt->getDuration();
            scheduleAt(simTime() + serviceTime, endServiceEvent);
        }
    }
    std::cout << "RX: Me llego el paquete: " << msg->getTimestamp() << endl;
    FeedbackPacket* fPkt = new FeedbackPacket();
    fPkt-> setByteLength (20);
    fPkt-> setKind(2);
    fPkt->setRemainingBuffer(par("bufferSize").longValue() - buffer.getLength());
    fPkt->setPacketTimeStamp(msg->getTimestamp());
    this->bufferSizeVector.record(1);
    try {
        send (fPkt, "toOut$o");
        std:: cout << "RX: Enviando fPkt: " << fPkt->getPacketTimeStamp() << endl;
    }
    catch (const std::exception& ex) {
        std::cout << "RX: No se pudo enviar el Fpkt" << ex.what() << endl;
    }
}

#endif



