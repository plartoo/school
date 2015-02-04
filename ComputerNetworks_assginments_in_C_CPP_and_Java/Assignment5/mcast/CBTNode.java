
package mcast;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.regex.*;

/*  
 * Packet Format:
 * - Total size: 256 bytes (0.25 kbytes)
 * - - Prelude: 1 byte
 * - - Flags: 1 byte
 * - - Group ID: 2 bytes
 * - - Group Source: 4 bytes
 * - - Launchtime: 8 bytes
 * - - Length: 4 bytes
 * - - Payload: 236 bytes
 */

public class CBTNode {
    public static String name;
    public static Integer port;
    public static HashSet<String> allNodes;
    public static PacketLog packets;
    private static final int PSTATE_DEFAULT = 0;
    private static final int PSTATE_NEIGHBORS = 1;
    private static final int PSTATE_DONE = 2;
    
    public static void main(String args[]) {
	if(args.length < 2) {
	    System.out.println("USAGE: java CBTNode <port> <top file> [<stats>]");
	    System.exit(0);
	}
	
	DatagramSocket cbtSocket;
	RoutingTable routingTable;
	CBTMulticastTable mcastTable;
	String file = "";
	List<String> sendQueue;
	packets = new PacketLog();
	
	try {
	    // get initial data
	    name = InetAddress.getLocalHost().getHostName().split("\\.")[0];
	    allNodes = new HashSet<String>();
	    port = Integer.parseInt(args[0]);
	    cbtSocket = new DatagramSocket(port);
	    // timeout is for benefit of unicast 
	    cbtSocket.setSoTimeout(5000);
	    file = args[1];
	    routingTable = parseTopologyFile(file);
	    mcastTable = new CBTMulticastTable(name);
	    sendQueue = Collections.synchronizedList(new LinkedList<String>());
	    
	    // get unicast routing information
	    UnicastSendThread uSend = new UnicastSendThread(routingTable, cbtSocket);
	    UnicastReceiveThread uRecv = new UnicastReceiveThread(routingTable, cbtSocket);
	    uSend.start(); uRecv.start();
	    uRecv.join();
	    
	    // commence multicast algorithm
	    cbtSocket.setSoTimeout(0);
	    CBTMulticastReceiveThread mRecv = new CBTMulticastReceiveThread(routingTable, mcastTable, cbtSocket);
	    CBTMulticastSendThread mSend = new CBTMulticastSendThread(cbtSocket, routingTable, mcastTable, sendQueue);
	    CBTCommandLineThread mCmd = new CBTCommandLineThread(mcastTable, sendQueue);
	    mRecv.start(); mSend.start(); mCmd.start();
	    mCmd.join();
	    
	    // print stats
	    if(args.length > 2 && args[2].equals("stats")) {
		System.out.println("\nMulticast session ended. Results... ");
		System.out.println("\tReceived Content (bytes): " + packets.getBroadcastBytesReceived());
		System.out.println("\tReceived Total (bytes): " + packets.getTotalBytesReceived());
		System.out.println("\tSent Content (bytes): " + packets.getBroadcastBytesSent());
		System.out.println("\tSent Total (bytes): " + packets.getTotalBytesSent());
		for(String node : allNodes) {
		    if(!node.equals(name)) {
			System.out.println("\tAvg Cost from " + node + ": " + packets.getAverageLatency(node));
		    }
		}
	    }
	    System.exit(0);
	}
	catch(FileNotFoundException e) {
	    System.err.println("FATAL ERROR: Could not find topology file " + file);
	    e.printStackTrace();
	}
	catch(Exception e) {
	    e.printStackTrace();
	}
	finally {
	    System.exit(-1);
	}
    }
    
    public static RoutingTable parseTopologyFile(String file) throws IOException {
	HashMap<String, Double> initialTable = new HashMap<String, Double>();
	BufferedReader in = new BufferedReader(new FileReader(file));
	String fline = in.readLine();
	int state = PSTATE_DEFAULT;
	
	while(fline != null) {
	    if(fline.matches("^end")) {
		if(state == PSTATE_NEIGHBORS) {
		    state = PSTATE_DONE;
		}
	    }
	    else if(fline.matches("^[A-Za-z0-9_\\-]+")) {
		if(state == PSTATE_DEFAULT || state == PSTATE_DONE) {
		    String[] splice = fline.split("\\s");
		    System.out.println("DEBUG: Observing entry for " + splice[0]);
		    allNodes.add(splice[0]);
		    if(state == PSTATE_DEFAULT && splice[0].equals(name)) {
			state = PSTATE_NEIGHBORS;
		    }
		}
		else if(state == PSTATE_NEIGHBORS) {
		    break;
		}
	    }
	    else if(fline.matches("^  [A-Za-z0-9_\\-]+ (\\d*\\.\\d+|\\d+)")) {
		if(state == PSTATE_NEIGHBORS) {
		    String[] splice = fline.split("\\s");
		    String neighbor = splice[2];
		    Double cost = Double.parseDouble(splice[3]);
		    initialTable.put(neighbor, cost);
		}
	    }
	    
	    fline = in.readLine();
	}
	
	return new RoutingTable(initialTable);
    }
}

class CBTCommandLineThread extends Thread {
    private BufferedReader cmdIn;
    private CBTMulticastTable mTable;
    private List<String> msgQueue;
    private Object ioLock;
    
    public CBTCommandLineThread(CBTMulticastTable mTable, List<String> msgQueue) {
	cmdIn = new BufferedReader(new InputStreamReader(System.in));
	this.mTable = mTable;
	this.msgQueue = msgQueue;
	this.ioLock = ioLock;
    }
    
    public void run() {
	try {
	    while(true) {
		String cline = "";
		cline = cmdIn.readLine();
		
		// join: duh
		if(cline.matches("^join [0-9]+")) {
		    String[] splice = cline.split("\\s");
		    String cmd = splice[0];
		    String grp = splice[1];
		    Integer gno = Integer.parseInt(grp);
		    if(gno < 0 || gno >= 65536) {
			System.err.println("ERROR [join]: group number must be between 0 and 65535.");
			continue;
		    }
		    
		    if(!mTable.hasCenter(gno)) {
			mTable.addCenter(gno, mTable.getHost());
			mTable.addGroup(gno);
			System.out.println("DEBUG [join]: joined group #" + gno + ", we are center.");
			synchronized(msgQueue) {
			    msgQueue.add("CGRP" + gno + "\r\n");
			}
		    }
		    else if(!mTable.hasGroup(gno)) {
			mTable.addGroup(gno);
			System.out.println("DEBUG [join]: joined group #" + gno);
			synchronized(msgQueue) {
			    msgQueue.add("JGRP" + gno + "\r\n");
			}
		    }
		    else {
			System.err.println("ERROR [join]: Already a member of group #" + gno);
			continue;
		    }
		}
		// send: broadcast message to this group
		else if(cline.matches("^send [0-9]+ .+")) {
		    String[] splice = cline.split("\\s");
		    String cmd = splice[0];
		    String grp = splice[1];
		    Integer gno = Integer.parseInt(grp);
		    if(gno < 0 || gno >= 65536) {
			System.err.println("ERROR [send]: group number must be between 0 and 65535.");
			continue;
		    }
		    if(!mTable.hasGroup(gno)) {
			System.err.println("ERROR [send]: Not a member of group #" + gno);
			continue;
		    }
		    Pattern msgPattern = Pattern.compile("^(send) ([0-9]+) (.+)");
		    Matcher msgMatch = msgPattern.matcher(cline);
		    String msg = "";
		    if(msgMatch.matches()) {
			msg = msgMatch.group(3);
		    } 
		    synchronized(msgQueue) {
			msgQueue.add("SGRP" + gno + "\r\n" + msg);
		    }
		}
		// drop: leave group
		else if(cline.matches("^drop [0-9]+")) {
		    String[] splice = cline.split("\\s");
		    String cmd = splice[0];
		    String grp = splice[1];
		    Integer gno = Integer.parseInt(grp);
		    if(gno < 0 || gno >= 65536) {
			System.out.println("ERROR [drop]: group number must be between 0 and 65535.");
			continue;
		    }
		    if(mTable.hasGroup(gno)) {
			mTable.removeGroup(gno);
			System.out.println("DEBUG [drop]: dropped group #" + gno);
			synchronized(msgQueue) {
			    msgQueue.add("DGRP" + gno + "\r\n");
			}
		    }
		}
		// bombard: send random n bytes
		else if(cline.matches("^bombard [0-9]+ [0-9]+")) {
		    String[] splice = cline.split("\\s");
		    Integer gno = Integer.parseInt(splice[1]);
		    Integer bno = Integer.parseInt(splice[2]);
		    if(gno < 0 || gno >= 65536) {
			System.out.println("ERROR [drop]: group number must be between 0 and 65535.");
			continue;
		    }
		    if(bno <= 0 || bno > 236) {
			System.out.println("ERROR [drop]: must send 1 to 236 bytes.");
			continue;
		    }
		    
		    String msg = MulticastPacket.spitRandomBytes(bno);
		    synchronized(msgQueue) {
			msgQueue.add("SGRP" + gno + "\r\n" + msg);
		    }
		}
		// quit: end program
		else if(cline.matches("^quit")) {
		    return;
		}
		else {
		    System.out.println("ERROR [mcast cmd]: unrecognized command \"" + cline + "\"");
		}
	    }
	}
	catch(Exception e) {
	    e.printStackTrace();
	    return;
	}
    }
}

class CBTMulticastReceiveThread extends Thread {
    private RoutingTable uTable;
    private CBTMulticastTable mTable;
    private DatagramSocket socket;
    
    public CBTMulticastReceiveThread(RoutingTable uTable, CBTMulticastTable mTable, DatagramSocket socket) {
	this.uTable = uTable;
	this.mTable = mTable;
	this.socket = socket;
    }
    
    public void run() {
	while(true) {
	    try {
		// catch new packet
		byte[] buff = new byte[MulticastPacket.PACKET_SIZE];
		DatagramPacket recvPacket = new DatagramPacket(buff, buff.length);
		socket.receive(recvPacket);
		
		// parse packet info
		byte pPrelude = buff[0];
		boolean[] pFlags = MulticastPacket.parseFlags(buff, 1, 2);
		short pGid = MulticastPacket.parseGid(buff, 2, 4);
		byte[] pSource = MulticastPacket.parseSource(buff, 4, 8);
		long pLaunch = MulticastPacket.parseLaunch(buff, 8, 16);
		int pLength = MulticastPacket.parseLength(buff, 16, 20);
		char[] pPayload = MulticastPacket.parsePayload(buff, 20, 20 + pLength);
		String incoming = recvPacket.getAddress().getHostName().split("\\.")[0];
		String src = InetAddress.getByAddress(pSource).getHostName().split("\\.")[0];
		Integer gid = (int) pGid;
		
		// format forwarding packet
		long nLaunch = pLaunch + uTable.get(incoming, incoming).longValue();
		byte[] nbuff = MulticastPacket.formatCBTHeader(pGid, pSource, nLaunch, pLength, pFlags[0], pFlags[1], pFlags[2]);
		System.arraycopy(buff, 20, nbuff, 20, 236);
		
		// push packet descriptor
		boolean trv = pFlags[0] || pFlags[1] || pFlags[2];
		String mh = mTable.getHost();
		CBTNode.packets.pushReceivedPacket(new PacketDescriptor(gid, src, pLength, nLaunch, !trv));
		
		// receive message from new group center
		if(pFlags[0]) {
		    String up = src.equals(mTable.getHost()) ? "" : uTable.findShortestTo(src, mTable.getHost()).key;
		    // if we're on shortest path to center,
		    // propagate message some more
		    if(up.equals(incoming)) {
			mTable.addCenter(gid, src);
			mTable.setUpstream(gid, up);
			for(String down : uTable.getNeighbors()) {
			    if(!down.equals(up)) {
				new MulticastFloodThread(socket, down, nbuff).start();
				// push packet descriptor
				CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, nLaunch, false));
			    }
			}
		    }
		}
		// receive join message for a certain group
		else if(pFlags[1]) {
		    // add incoming to downstream
		    mTable.addDownstream(gid, incoming);
		    // propagate further upstream if we are not a group member
		    if(!mTable.hasGroup(gid)) {
			String up = mTable.getUpstream(gid);
			new MulticastFloodThread(socket, up, nbuff).start();
			// push packet descriptor
			CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, nLaunch, false));
		    }
		}
		// receive drop message for a certain group
		else if(pFlags[2]) {
		    // remove from downstream
		    mTable.removeDownstream(gid, incoming);
		    // propagate further upstream if we have no more downstreams
		    // and we are not a group member
		    if(!mTable.hasGroup(gid) && mTable.getDownstream(gid).isEmpty()) {
			String up = mTable.getUpstream(gid);
			new MulticastFloodThread(socket, up, nbuff).start();
			// push packet descriptor
			CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, nLaunch, false));
		    }
		}
		// broadcast message
		else {
		    // display message if we're a group member
		    if(mTable.hasGroup(gid)) {
			Long currentTime = System.currentTimeMillis();
			System.out.println("[mcast recv]: Got message from " + src + " via " + incoming + " [" + gid + "]: ");
			System.out.println("\t" + new String(pPayload));
			System.out.println("[mcast recv]: Broadcast cost was " + nLaunch);
		    }
		    
		    // get receivers, remove incoming
		    HashSet<String> receivers = mTable.getReceivers(gid);
		    receivers.remove(incoming);
		    if(!mTable.hasGroup(gid) && !receivers.isEmpty())
			System.out.println("DEBUG [mcast recv]: Got message from group #" + gid + " via " + incoming + ", forwarding...");
		    for(String recv : receivers) {
			if(recv != null) {
			    new MulticastFloodThread(socket, recv, nbuff).start();
			    // push packet descriptor
			    CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, nLaunch));
			}
		    }
		}
	    }
	    catch(Exception e) {
		System.err.println("ERROR [mcast recv]: " + e.getMessage());
		e.printStackTrace();
	    }
	}
    }
}

class CBTMulticastSendThread extends Thread {
    private DatagramSocket socket;
    private RoutingTable uTable;
    private CBTMulticastTable mTable;
    private List<String> msgQueue;
    
    public CBTMulticastSendThread(DatagramSocket socket, RoutingTable uTable, CBTMulticastTable mTable, List<String> msgQueue) {
	this.socket = socket;
	this.uTable = uTable;
	this.mTable = mTable;
	this.msgQueue = msgQueue;
    }
    
    public void run() {
	while(true) {
	    try {
		String front = "";
		Integer gid = -1;
		String payload = "";
		boolean brk = false;
		while(!brk) {
		    synchronized(msgQueue) { brk = !msgQueue.isEmpty(); }
		    if(brk) {
			synchronized(msgQueue) { front = msgQueue.remove(0); }
		    }
		}
		
		gid = Integer.parseInt(front.split("\\s+")[0].substring(4));
		payload = front.substring(front.split("\\s+")[0].length() + 2);
		
		byte[] pSource = InetAddress.getByName(mTable.getHost()).getAddress();
		//Long pLaunch = System.currentTimeMillis();
		Integer pLength = payload.length();
		String mh = mTable.getHost();
		if(front.matches("CGRP[0-9]+\r\n")) {
		    byte[] pkt = MulticastPacket.formatCBTHeader(gid.shortValue(), pSource, 0, pLength, true, false, false);
		    System.arraycopy(payload.getBytes(), 0, pkt, 20, payload.length());
		    for(String down : uTable.getNeighbors()) {
			new MulticastFloodThread(socket, down, pkt).start();
			// push packet descriptor
			CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, 0, false));
		    }
		}
		else if(front.matches("JGRP[0-9]+\r\n")) {
		    byte[] pkt = MulticastPacket.formatCBTHeader(gid.shortValue(), pSource, 0, pLength, false, true, false);
		    System.arraycopy(payload.getBytes(), 0, pkt, 20, payload.length());
		    String center = mTable.getCenter(gid);
		    String up = mTable.getUpstream(gid);
		    if(up == null) {
			up = uTable.findShortestTo(center, mTable.getHost()).key;
			mTable.setUpstream(gid, up);
		    }
		    new MulticastFloodThread(socket, up, pkt).start();
		    // push packet descriptor
		    CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, 0, false));
		}
		else if(front.matches("DGRP[0-9]+\r\n")) {
		    byte[] pkt = MulticastPacket.formatCBTHeader(gid.shortValue(), pSource, 0, pLength, false, false, true);
		    System.arraycopy(payload.getBytes(), 0, pkt, 20, payload.length());
		    String up = mTable.getUpstream(gid);
		    if(mTable.getDownstream(gid).isEmpty()) {
			new MulticastFloodThread(socket, up, pkt).start();
			// push packet descriptor
			CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, 0, false));
		    }
		}
		else {
		    byte[] pkt = MulticastPacket.formatCBTHeader(gid.shortValue(), pSource, 0, pLength, false, false, false);
		    System.arraycopy(payload.getBytes(), 0, pkt, 20, payload.length());
		    for(String recv : mTable.getReceivers(gid)) {
			new MulticastFloodThread(socket, recv, pkt).start();
			// push packet descriptor
			CBTNode.packets.pushSentPacket(new PacketDescriptor(gid, mh, pLength, 0));
		    }
		}
	    }
	    catch(IllegalArgumentException e) {
		System.err.println("ERROR [mcast send]: prune and unprune bits cannot both be true.");
	    }
	    catch(Exception e) {
		e.printStackTrace();
		continue;
	    }
	}
    }
}

class CBTMulticastTable {
    private String host;
    private HashSet<Integer> groups;
    private HashMap<Integer, String> centers;
    private HashMap<Integer, String> upstream;
    private HashMap<Integer, HashSet<String>> downstream;
    
    public CBTMulticastTable(String host) {
	this.host = host;
	this.groups = new HashSet<Integer>();
	this.centers = new HashMap<Integer, String>();
	this.upstream = new HashMap<Integer, String>();
	this.downstream = new HashMap<Integer, HashSet<String>>();
    }
    
    public synchronized boolean addGroup(Integer gid) {
	if(!centers.containsKey(gid))
	    return false;
	return groups.add(gid);
    }
    
    public synchronized boolean hasGroup(Integer gid) {
	return groups.contains(gid);
    }
    
    public synchronized boolean removeGroup(Integer gid) {
	return groups.remove(gid);
    }
    
    public synchronized boolean addCenter(Integer gid, String cen) {
	if(centers.containsKey(gid))
	    return false;
	return centers.put(gid, cen) == null;
    }
    
    public synchronized String getCenter(Integer gid) {
	return centers.get(gid);
    }
    
    public synchronized boolean hasCenter(Integer gid) {
	return centers.containsKey(gid);
    }
    
    public synchronized boolean removeCenter(Integer gid) {
	if(!centers.containsKey(gid))
	    return false;
	centers.remove(gid);
	return true;
    }
    
    public synchronized boolean setUpstream(Integer gid, String up) {
	if(upstream.get(gid) != null)
	    return false;
	return upstream.put(gid, up) == null;
    }
    
    public synchronized String getUpstream(Integer gid) {
	return upstream.get(gid);
    }
    
    public synchronized boolean isUpstream(Integer gid, String up) {
	return upstream.get(gid).equals(up);
    }
    
    public synchronized boolean addDownstream(Integer gid, String down) {
	if(!centers.containsKey(gid))
	    return false;
	if(downstream.get(gid) == null)
	    downstream.put(gid, new HashSet<String>());
	return downstream.get(gid).add(down);
    }
    
    public synchronized HashSet<String> getDownstream(Integer gid) {
	return downstream.get(gid);
    }
    
    public synchronized boolean hasDownstream(Integer gid, String down) {
	return downstream.containsKey(gid) && downstream.get(gid).contains(down);
    }
    
    public synchronized boolean removeDownstream(Integer gid, String down) {
	if(!centers.containsKey(gid))
	    return false;
	if(downstream.get(gid) == null)
	    return false;
	return downstream.get(gid).remove(down);
    }
    
    public synchronized HashSet<String> getReceivers(Integer gid) {
	HashSet<String> receivers = new HashSet<String>();
	HashSet<String> downstream = getDownstream(gid);
	String upstream = getUpstream(gid);
	if(downstream != null) receivers.addAll(downstream);
	if(upstream != null) receivers.add(upstream);
	return receivers;
    }
    
    public String getHost() {
	return host;
    }
}
