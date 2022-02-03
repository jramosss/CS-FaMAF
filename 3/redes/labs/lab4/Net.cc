#ifndef NET
#define NET

#include <string.h>
#include <omnetpp.h>
#include <packet_m.h>

using namespace omnetpp;

class Net: public cSimpleModule {
private:

public:
    Net();
    virtual ~Net();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(Net);

#endif /* NET */

Net::Net() {
}

Net::~Net() {
}

void Net::initialize() {

    // manda el paquete para que averigue cuantos nodos hay en la red
    Packet *pkt = new Packet("initPacket",this->getParentModule()->getIndex());
    pkt->setByteLength(1);
    pkt->setSource(this->getParentModule()->getIndex());
    pkt->setDestination(this->getParentModule()->getIndex());
    pkt->setHopCount(1);
    pkt->setKind(2);
    pkt->setName("initPacket");
    send(pkt, "toLnk$o",0);

}

void Net::finish() {
}

void Net::handleMessage(cMessage *msg) {

    // All msg (events) on net are packets
    Packet *pkt = (Packet *) msg;

    // If this node is the final destination, send to App
    if ( pkt->getDestination() == this->getParentModule()->getIndex())
        send(msg, "toApp$o");

    // If not, forward the packet to some else... to who?
    else if (pkt->getKind() == 1)   // el paquete debe ir en sentido antihorario
    {
        pkt->setHopCount(pkt->getHopCount()+1);
        send(msg, "toLnk$o",1);
    }

    // el paquete va en sentido horario, tanto kind(1) como kind(2)
    else {
        pkt->setHopCount(pkt->getHopCount()+1);
        send(msg, "toLnk$o", 0);
    }

}
