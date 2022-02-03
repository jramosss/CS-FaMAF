#ifndef GENERATOR
#define GENERATOR

#include <string.h>
#include <string>
#include <omnetpp.h>

using namespace omnetpp;

class Generator : public cSimpleModule {
private:
    cPacket *sendMsgEvent;
    cStdDev transmissionStats;
    cOutVector packetGenVector;
public:
    Generator();
    virtual ~Generator();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};
Define_Module(Generator);

Generator::Generator() {
    sendMsgEvent = NULL;

}

Generator::~Generator() {
    cancelAndDelete(sendMsgEvent);
}

void Generator::initialize() {
    packetGenVector.setName("packetGen");
    transmissionStats.setName("TotalTransmissions");
    // create the send packet
    sendMsgEvent = new cPacket("sendEvent");
    sendMsgEvent->setByteLength(par("packetByteSize"));
    // schedule the first event at random time
    scheduleAt(par("generationInterval"), sendMsgEvent);
}

void Generator::finish() {
}

void Generator::handleMessage(cMessage *msg) {

    // create new packet
    std::string simt = getSimulation()->getSimTime().str();
    cPacket *pkt = new cPacket(simt.c_str());
    packetGenVector.record(1);
    // set packet size in bytes
    pkt->setByteLength(par("packetByteSize"));
    // send to the output
    send(pkt, "out");

    // compute the new departure time
    simtime_t departureTime = simTime() + par("generationInterval");
    // schedule the new packet generation
    scheduleAt(departureTime, sendMsgEvent);
}

#endif /* GENERATOR */
