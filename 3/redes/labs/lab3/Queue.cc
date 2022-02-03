#ifndef QUEUE
#define QUEUE

#include <string.h>
#include <omnetpp.h>


using namespace omnetpp;

class Queue: public cSimpleModule {
private:
    cQueue buffer;
    cPacket *endServiceEvent;
    simtime_t serviceTime;
    cOutVector bufferSizeVector;
    cOutVector packetDropVector;
public:
    Queue();
    virtual ~Queue();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *pkt);

};

Define_Module(Queue);

Queue::Queue() {
    endServiceEvent = NULL;
}


Queue::~Queue() {
    cancelAndDelete(endServiceEvent);
}

void Queue::initialize() {
    buffer.setName("buffer");
    bufferSizeVector.setName("bufferSize");
    endServiceEvent = new cPacket("endService");
}

void Queue::finish() {
}

void Queue::handleMessage(cMessage *pkt) {
    // if msg is signaling an endServiceEvent
    if (pkt == endServiceEvent) {
        // if packet in buffer, send next one
        if (!buffer.isEmpty()) {
            // dequeue packet
            cPacket *pkt = (cPacket*) buffer.pop();
            // send packet
            send(pkt, "out");
            serviceTime = pkt->getDuration();
            scheduleAt(simTime() + serviceTime, endServiceEvent);
        }
    }
    // check buffer Limit
    else if (buffer.getLength() >= par("bufferSize").longValue()) {
    // drop the packet
    	delete pkt;
    	this->bubble("packet dropped");
    	packetDropVector.record(1);

	} else {
		// enqueue the packet
		buffer.insert(pkt);
    	bufferSizeVector.record(buffer.getLength());
		// if the server is idle
		if (!endServiceEvent->isScheduled()) {
			// start the service now
			scheduleAt(simTime() + 0, endServiceEvent) ;
		}
	}

}

#endif /* QUEUE */
