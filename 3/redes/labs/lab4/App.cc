#ifndef APP
#define APP

#include <string.h>
#include <omnetpp.h>
#include <packet_m.h>
#include <stdio.h>

using namespace omnetpp;

class App: public cSimpleModule {
private:
    cMessage *sendMsgEvent;
    cStdDev delayStats;
    cOutVector delayVector;
    cOutVector hopsVector;
    cOutVector sourceVector;
    int nodes;
public:
    App();
    virtual ~App();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void setNodes(int nodes);
    virtual int getNodes();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(App);

#endif /* APP */

App::App() {
}

App::~App() {
}

void App::initialize() {

    this->setNodes(0); //de momento no sabe a cuantos nodos puede enviar paquetes

    // If interArrivalTime for this node is higher than 0
    // initialize packet generator by scheduling sendMsgEvent
    if (par("interArrivalTime").doubleValue() != 0) {
        sendMsgEvent = new cMessage("sendEvent");
        scheduleAt(par("interArrivalTime"), sendMsgEvent);
    }

    // Initialize statistics
    delayStats.setName("TotalDelay");
    delayVector.setName("Delay");
    hopsVector.setName("HopsVector");
    sourceVector.setName("SourceVector");
}

void App::finish() {
    // Record statistics
    recordScalar("Average delay", delayStats.getMean());
    recordScalar("Number of packets", delayStats.getCount());
}

void App::setNodes(int nodes){
    this->nodes = nodes;
}

int App::getNodes(){
   return this->nodes;
}

void App::handleMessage(cMessage *msg) {


    // if msg is a sendMsgEvent, create and send new packet
    if (msg == sendMsgEvent) {
        if (this->getNodes() > 0)
        {
            // create new packet
            Packet *pkt = new Packet("packet",this->getParentModule()->getIndex());
            pkt->setByteLength(par("packetByteSize"));
            pkt->setSource(this->getParentModule()->getIndex());
            pkt->setDestination(par("destination"));
            pkt->setHopCount(0);
            int hopsToDest = pkt->getSource() - pkt->getDestination();
            int netSize = this->getNodes();
            if (hopsToDest > 0)    // kind 0 es clockwise, 1 es anticlockwise; depende de la distancia entre nodos
            {
                if (hopsToDest > floor((netSize -1) / 2)) // para n/2
                    pkt->setKind(1);
                else
                    pkt->setKind(0);
            }
            else
            {
                if (hopsToDest < - floor((netSize-1) / 2))
                    pkt->setKind(0);
                else
                    pkt->setKind(1);
            }

            // send to net layer
            send(pkt, "toNet$o");
        // compute the new departure time and schedule next sendMsgEvent
        }
        simtime_t departureTime = simTime() + par("interArrivalTime");
        scheduleAt(departureTime, sendMsgEvent);

    }

    // else, msg is a packet from net layer
    else {
        if (msg->getKind() == 2)    // mmsg es un contador de nodos
        {
            Packet* pkt = (Packet*) msg;
            this->setNodes(pkt->getHopCount());
            std::cout << "ACA ME LLEGO EL HELLO CON " << this->getNodes()<< " NODOS" << endl;
            delete(msg);
            return;
        }
        // compute delay and record statistics
        simtime_t delay = simTime() - msg->getCreationTime();
        delayStats.collect(delay);
        delayVector.record(delay);
        Packet* pkt = (Packet*) msg;
        hopsVector.record(pkt->getHopCount());
        sourceVector.record(pkt->getSource());
        // delete msg
        delete (msg);
    }

}

