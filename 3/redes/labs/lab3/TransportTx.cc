#ifndef TRANSPORTX
#define TRANSPORTX

#include <string.h>
#include <omnetpp.h>
#include <iostream>

#include "FeedbackPacket.h"


using namespace omnetpp;

class TransportTx : public cSimpleModule {
private:
    cQueue buffer;
    cMessage *endServiceEvent;
    simtime_t serviceTime;
    cOutVector bufferSizeVector;
    cOutVector packetDropVector;
    //Puede el modulo enviar paquetes?
    bool       shouldSend;
    //Tamaño restante del buffer de sink
    int        sinkRemainingBuffer;
    //Time stamp del ultimo paquete enviado
    simtime_t  lastSentMsgTs;
    //El mensaje recibido es el primero de todo el programa?
    bool first;
    //Contador de paquetes encolados sin enviar
    int        count;

public:
    TransportTx();
    virtual ~TransportTx();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(TransportTx);

TransportTx::TransportTx(){
    endServiceEvent = NULL;
}

TransportTx::~TransportTx(){
    cancelAndDelete(endServiceEvent);
}

void TransportTx::initialize(){
    buffer.setName("bufferTx");
    endServiceEvent = new cPacket("endService");
    bufferSizeVector.setName("txBufferSize");
    packetDropVector.setName("txDroppedPkt");
    shouldSend = true;
    lastSentMsgTs = 0;
    first = true;
    count = 0;
}

void TransportTx::finish() {}

void TransportTx::handleMessage(cMessage* msg){
    //Entro por el try o por el catch?
    bool crashed = false;
    //Si el mensaje recibido es un paquete normal
    if (!msg->getKind()) {
        //Lo guardamos en el buffer
        this->buffer.insert(msg);
        std::cout<<"TX: Encolando mensaje: " << msg->getName() << endl;
        //E incrementamos el contador de paquetes encolados sin enviar
        this->count++;
    }

    if (first) {
        std::cout<<"First!" << endl;
        if (!this->buffer.isEmpty()) {
            //Decolamos el paquete de la cola
            cPacket* sMsg = (cPacket*)this->buffer.pop();
            //Le establecemos el timeStamp de este preciso instante
            //a modo de identificador
            sMsg->setTimestamp();
            send(sMsg,"toOut$o");
            std:: cout << "TX: Enviando mensaje: " << sMsg->getTimestamp() << endl;
            this->lastSentMsgTs = sMsg->getTimestamp();
        }
        this->bufferSizeVector.record(buffer.getLength());
        first = false;
    }

    //Si ocurre un "timeout"
    if (this->count >= 10) {
        this->count = 0;

        if (!this->buffer.isEmpty()) {
            cPacket* sMsg = (cPacket*)this->buffer.pop();
            sMsg->setTimestamp();
            try {
                send(sMsg,"toOut$o");
                std:: cout << "TX: Enviando mensaje: " << sMsg->getTimestamp() << endl;
            }
            catch (const std::exception& ex) {
                std:: cout << "TX: Error al enviar el mensaje: " << sMsg->getTimestamp() << endl;
                crashed = true;
            }
            //Si no paso por el catch, todo salio bien
            if (!crashed)
                this->lastSentMsgTs = sMsg->getTimestamp();
        }
    }

    //Si el mensaje recibido es un feedbackpacket
    if (msg->getKind() == 2) {
        //Reestablecemos el contador de "timeout"
        this->count = 0;

        FeedbackPacket* fPkt = (FeedbackPacket*) msg;

        this->sinkRemainingBuffer = fPkt->getRemainingBuffer();
        //Si queda espacio en el buffer del sink y el TS del ack recibido es igual
        //al del ultimo paquete enviado
        this->shouldSend = this->sinkRemainingBuffer > 0 && fPkt->getPacketTimeStamp() == this->lastSentMsgTs;
        std::cout << "TX: " << fPkt->getPacketTimeStamp() << "," << this->lastSentMsgTs << endl;
        std::cout << "TX: ShouldSend: " << this->shouldSend << endl;
        //Si no podemos enviar, salimos de la funcion
        if (this->shouldSend) {
            if (!this->buffer.isEmpty()) {
                cPacket* sMsg = (cPacket*)this->buffer.pop();
                sMsg->setTimestamp();
                try {
                    send(sMsg,"toOut$o");
                    std:: cout << "TX: Enviando mensaje: " << sMsg->getTimestamp() << endl;
                }
                catch (const std::exception& ex) {
                    std:: cout << "TX: Error al enviar el mensaje: " << sMsg->getTimestamp() << endl;
                    crashed = true;
                }
                if (!crashed) {
                    this->lastSentMsgTs = sMsg->getTimestamp();
                    this->bufferSizeVector.record(buffer.getLength());
                }
            }
        }

        delete (msg);
    }
}

#endif

