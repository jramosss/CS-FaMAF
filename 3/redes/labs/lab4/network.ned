simple App
{
    parameters:
        volatile double interArrivalTime = default(0); // sec
        int packetByteSize = default(125); // bytes
        int destination = default(0);
        @display("i=block/app");
    gates:
        inout toNet;
}

simple Net
{
    parameters:
        @display("i=block/dispatch");
    gates:
        inout toApp;
        inout toLnk[];
}

simple Lnk
{
    parameters:
        @display("i=block/queue");
    gates:
        inout toNet;
        inout toOut;
}

module Node
{
    parameters:
        @display("i=block/routing");
        int interfaces;
    gates:
        inout toNod[interfaces];
    submodules:
        app: App {
            parameters:
                @display("p=75,50");
        }
        net: Net {
            parameters:
                @display("p=75,120");
        }
        lnk[interfaces]: Lnk {
            parameters:
                @display("p=75,190,r,70");
        }
    connections allowunconnected:
        app.toNet <--> net.toApp;
        for i=0..interfaces-1 {
            net.toLnk++ <--> lnk[i].toNet;
            lnk[i].toOut <--> toNod[i];
        }
}

network Network
{
    submodules:
        node[8]: Node {
            @display("p=100,100,ri,100,100");
            interfaces=2;
        }
    connections allowunconnected:
        node[0].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[1].toNod[0];
        node[1].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[2].toNod[0];
        node[2].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[3].toNod[0];
        node[3].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[4].toNod[0];
        node[4].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[5].toNod[0];
        node[5].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[6].toNod[0];
        node[6].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[7].toNod[0];
        node[7].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[0].toNod[0];
}

//network NetworkStar
//{
//    submodules:
//        node[57]: Node {
//            @display("i=misc/node_vs,gold");
//            interfaces=4;
//        }
//    connections allowunconnected:
//        node[0].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[1].toNod[2];
//        node[1].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[2].toNod[2];
//        node[1].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[4].toNod[2];
//        node[3].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[4].toNod[3];
//        node[4].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[5].toNod[2];
//        node[4].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[7].toNod[2];
//        node[5].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[6].toNod[2];
//        node[5].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[10].toNod[2];
//        node[6].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[7].toNod[3];
//        node[6].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[9].toNod[2];
//        node[7].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[8].toNod[2];
//        node[7].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[12].toNod[2];
//        node[9].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[11].toNod[2];
//        node[10].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[11].toNod[3];
//        node[10].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[13].toNod[2];
//        node[11].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[12].toNod[3];
//        node[11].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[14].toNod[2];
//        node[12].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[15].toNod[2];
//        node[13].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[18].toNod[2];
//        node[14].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[15].toNod[3];
//        node[14].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[22].toNod[2];
//        node[15].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[16].toNod[2];
//        node[16].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[17].toNod[2];
//        node[16].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[23].toNod[2];
//        node[18].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[19].toNod[2];
//        node[18].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[21].toNod[2];
//        node[19].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[20].toNod[2];
//        node[19].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[26].toNod[2];
//        node[20].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[21].toNod[3];
//        node[20].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[24].toNod[2];
//        node[21].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[22].toNod[3];
//        node[21].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[23].toNod[3];
//        node[23].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[24].toNod[3];
//        node[24].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[28].toNod[2];
//        node[25].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[27].toNod[2];
//        node[26].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[32].toNod[2];
//        node[27].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[29].toNod[2];
//        node[27].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[30].toNod[2];
//        node[28].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[29].toNod[3];
//        node[28].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[31].toNod[2];
//        node[29].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[31].toNod[3];
//        node[30].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[36].toNod[2];
//        node[31].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[33].toNod[2];
//        node[32].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[34].toNod[2];
//        node[33].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[37].toNod[2];
//        node[34].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[35].toNod[2];
//        node[34].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[40].toNod[2];
//        node[35].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[36].toNod[3];
//        node[35].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[39].toNod[2];
//        node[36].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[37].toNod[3];
//        node[37].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[38].toNod[2];
//        node[38].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[39].toNod[3];
//        node[38].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[42].toNod[2];
//        node[39].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[40].toNod[3];
//        node[39].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[41].toNod[2];
//        node[40].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[43].toNod[2];
//        node[40].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[44].toNod[3];
//        node[41].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[42].toNod[3];
//        node[41].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[43].toNod[3];
//        node[43].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[46].toNod[2];
//        node[44].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[45].toNod[2];
//        node[44].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[47].toNod[2];
//        node[45].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[46].toNod[3];
//        node[46].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[49].toNod[2];
//        node[47].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[48].toNod[2];
//        node[48].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[50].toNod[2];
//        node[48].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[51].toNod[2];
//        node[49].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[50].toNod[3];
//        node[50].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[53].toNod[2];
//        node[51].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[52].toNod[2];
//        node[51].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[53].toNod[3];
//        node[52].toNod[1] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[54].toNod[2];
//        node[53].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[56].toNod[2];
//        node[54].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[55].toNod[2];
//        node[55].toNod[0] <--> {  datarate = 1Mbps; delay = 100us; } <--> node[56].toNod[3];
//}


