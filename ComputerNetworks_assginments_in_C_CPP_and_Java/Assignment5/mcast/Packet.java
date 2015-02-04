
package mcast;

import java.io.*;
import java.net.*;
import java.util.*;

class MulticastPacket {
    public static final int PACKET_SIZE = 256;
    public static final int HEADER_SIZE = 20;
    public static final int PACKET_PRELUDE = Integer.parseInt("01011011", 2); // SB
    
    private static long getBits(byte[] buff, int start, int stop) {
	long bits = 0;
	for(int i = start; i < stop; i++) {
	    long bite = buff[i] < 0 ? (long) (256 + buff[i]) : buff[i];
	    bite <<= ((stop - i - 1) * 8);
	    bits += bite;
	}
	return bits;
    }
    
    public static boolean[] parseFlags(byte[] buff, int start, int stop) {
	//return new boolean[1];
	boolean[] buddy = new boolean[(stop - start) * 8];
	int mask = 0x01;
	for(int i = 0; i < buddy.length; i++) {
	    //System.out.println("" + (mask << i));
	    int bit = (buff[start] & (mask << i)) >> i;
	    buddy[i] = (bit == 1);
	}
	return buddy;
    }
    
    public static short parseGid(byte[] buff, int start, int stop) {
	long bits = getBits(buff, start, stop);
	return (short) bits;
    }
    
    public static byte[] parseSource(byte[] buff, int start, int stop) {
	byte[] bytes = new byte[stop - start];
	for(int i = 0; i < bytes.length; i++) {
	    bytes[i] = buff[i + start];
	}
	return bytes;
    }
    
    public static long parseLaunch(byte[] buff, int start, int stop) {
	return getBits(buff, start, stop);
    }
    
    public static int parseLength(byte[] buff, int start, int stop) {
	long bits = getBits(buff, start, stop);
	return (int) bits;
    }
    
    public static char[] parsePayload(byte[] buff, int start, int stop) {
	char[] melion = new char[stop - start];
	for(int i = 0; i < melion.length; i++) {
	    melion[i] = (char) buff[i + start];
	}
	return melion;
    }
    
    public static byte[] formatRPFHeader(short gid, byte[] src, long time, int len, boolean prune, boolean unprune) throws IllegalArgumentException {
	if(prune && unprune)
	    throw new IllegalArgumentException();
	byte[] pkt = new byte[PACKET_SIZE];
	pkt[0] = (byte) PACKET_PRELUDE;
	pkt[1] = prune ? (byte) 1 : (byte) 0;
	pkt[1] = unprune ? (byte) 2 : pkt[1];
	//System.out.println("ERROR [format header]: the flag is " + pkt[1]);
	pkt[2] = (byte) ((gid >> 8) & 0xFF);
	pkt[3] = (byte) (gid & 0xFF);
	for(int i = 0; i < src.length; i++) {
	    pkt[i + 4] = src[i];
	}
	for(int i = 0; i < 8; i++) {
	    pkt[i + 8] = (byte) ((time >> ((7 - i) * 8)) & 0xFF);
	}
	for(int i = 0; i < 4; i++) {
	    pkt[i + 16] = (byte) ((len >> ((3 - i) * 8)) & 0xFF);
	}
	return pkt;
    }
    
    public static byte[] formatCBTHeader(short gid, byte[] src, long time, int len, boolean center, boolean seek, boolean drop) throws IllegalArgumentException {
	if((center && seek) || (seek && drop) || (drop && center))
	    throw new IllegalArgumentException();
	byte[] pkt = new byte[PACKET_SIZE];
	pkt[0] = (byte) PACKET_PRELUDE;
	pkt[1] = center ? (byte) 1 : (byte) 0;
	pkt[1] = seek ? (byte) 2 : pkt[1];
	pkt[1] = drop ? (byte) 4 : pkt[1];
	pkt[2] = (byte) ((gid >> 8) & 0xFF);
	pkt[3] = (byte) (gid & 0xFF);
	for(int i = 0; i < src.length; i++) {
	    pkt[i + 4] = src[i];
	}
	for(int i = 0; i < 8; i++) {
	    pkt[i + 8] = (byte) ((time >> ((7 - i) * 8)) & 0xFF);
	}
	for(int i = 0; i < 4; i++) {
	    pkt[i + 16] = (byte) ((len >> ((3 - i) * 8)) & 0xFF);
	}
	return pkt;
    }
    
    public static String spitRandomBytes(int bytes) {
	char[] melion = new char[bytes];
	Random rand = new Random();
	for(int i = 0; i < melion.length; i++) {
	    melion[i] = (char) (rand.nextInt(93) + 33);
	}
	return new String(melion);
    }
}

class PacketDescriptor {
    public int gid;
    public String src;
    public int size;
    public long latency;
    public boolean client;
    
    public PacketDescriptor(int gid, String src, int size, long latency, boolean client) {
	this.gid = gid;
	this.src = src;
	this.size = size + MulticastPacket.HEADER_SIZE;
	this.latency = latency;
	this.client = client;
    }
    
    public PacketDescriptor(int gid, String src, int size, long latency) {
	this(gid, src, size, latency, true);
    }
}

class PacketLog {
    private LinkedList<PacketDescriptor> sentList;
    private LinkedList<PacketDescriptor> receivedList;
    private long startTime;
    
    public PacketLog() {
	sentList = new LinkedList<PacketDescriptor>();
	receivedList = new LinkedList<PacketDescriptor>();
	startTime = System.currentTimeMillis();
    }
    
    public void pushSentPacket(PacketDescriptor pd) {
	sentList.add(pd);
    }
    
    public void pushReceivedPacket(PacketDescriptor pd) {
	receivedList.add(pd);
    }
    
    public int getTotalBytesSent() {
	int sum = 0;
	for(PacketDescriptor pd : sentList) {
	    sum += pd.size;
	}
	return sum;
    }
    
    public int getTotalBytesReceived() {
	int sum = 0;
	for(PacketDescriptor pd : receivedList) {
	    sum += pd.size;
	}
	return sum;
    }
    
    public int getBroadcastBytesSent() {
	int sum = 0;
	for(PacketDescriptor pd : sentList) {
	    if(pd.client) sum += pd.size;
	}
	return sum;
    }
    
    public int getBroadcastBytesReceived() {
	int sum = 0;
	for(PacketDescriptor pd : receivedList) {
	    if(pd.client) sum += pd.size;
	}
	return sum;
    }
    
    public double getAverageLatency(String from) {
	double time = 0.0;
	int gpackets = 0;
	for(PacketDescriptor pd : receivedList) {
	    if(pd.src.equals(from) && pd.client) {
		time += pd.latency;
		gpackets++;
	    }
	}
	if(gpackets == 0)
	    return 0.0;
	else
	    return time / gpackets;
    }
}

class MulticastFloodThread extends Thread {
    private DatagramSocket socket;
    private byte[] packet;
    private String host;
    
    public MulticastFloodThread(DatagramSocket socket, String host, byte[] packet) {
	this.socket = socket;
	this.host = host;
	this.packet = packet;
    }
    
    public void run() {
	try {
	    InetAddress faddr = InetAddress.getByName(host);
	    DatagramPacket pkt = new DatagramPacket(packet, packet.length, faddr, socket.getLocalPort());
	    socket.send(pkt);
	}
	catch(UnknownHostException e) {
	    System.err.println("ERROR [mcast flood]: could not flood packet to host " + host);
	}
	catch(IOException e) {
	    System.err.println("ERROR [mcast flood]: could not write packet to socket.");
	    System.err.println("ERROR [mcast flood]: " + e.getMessage());
	}
    }
}