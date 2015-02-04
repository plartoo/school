
package mcast;

import java.io.*;
import java.net.*;
import java.util.*;

class UnicastReceiveThread extends Thread {
    private RoutingTable table;
    private DatagramSocket socket;
    public static Integer ticks;
    private static final Integer BUFFSIZE = 10240;
    
    public UnicastReceiveThread(RoutingTable table, DatagramSocket socket) {
	this.table = table;
	this.socket = socket;
	ticks = RoutingTable.MAX_NODES;
    }
    
    public void run() {
	try {
	    while(true) {
		// read packets from neighbors
		byte[] buff = new byte[BUFFSIZE];
		ByteArrayInputStream binStream = new ByteArrayInputStream(buff);
		DatagramPacket packet = new DatagramPacket(buff, buff.length);
		socket.receive(packet);
		
		// if we have a packet, get the table and update our table
		if(packet.getLength() > 0) {
		    ObjectInputStream oinStream = new ObjectInputStream(binStream);
		    RoutingTable nTable = (RoutingTable) oinStream.readObject();
		    table.update(nTable);
		}
	    }
	}
	catch(SocketTimeoutException e) {
	    System.out.println("DEBUG: Routing table fully propagated. Continuing...");
	    return;
	}
	catch(ClassNotFoundException e) {
	    System.err.println("ERROR: Couldn't cast received object to a routing table.");
	    return;
	}
	catch(IOException e) {
	    e.printStackTrace();
	    return;
	}
    }
}

class UnicastSendThread extends Thread {
    private RoutingTable table;
    private DatagramSocket socket;
    public static Integer ticks;
    private static final Integer BUFFSIZE = 10240;
    
    public UnicastSendThread(RoutingTable table, DatagramSocket socket) {
	this.table = table;
	this.socket = socket;
	ticks = RoutingTable.MAX_NODES;
    }
    
    public void run() {
	try {
	    for(int i = 0; i < ticks; i++) {
		// wait 2 second
		Thread.sleep(2000);
		System.out.println("DEBUG: ## update number " + (i + 1));
		for(String host : table.getKnownHosts()) {
		    // find shortest path to given host
		    RoutingTable.TableEntry hop = table.findShortestTo(host, table.getHostName());
		    System.out.print("Shortest path to " + host + ": ");
		    System.out.print(hop.cost + " via node " + hop.key);
		    System.out.print("\n");
		}
		for(String neighbor : table.getNeighbors()) {
		    // send our updated table to our neighbors
		    ByteArrayOutputStream boutStream = new ByteArrayOutputStream();
		    ObjectOutputStream ooutStream = new ObjectOutputStream(boutStream);
		    ooutStream.writeObject(table);
		    byte[] buff = boutStream.toByteArray();
		    InetAddress naddr = InetAddress.getByName(neighbor);
		    DatagramPacket packet = new DatagramPacket(buff, buff.length, naddr, socket.getLocalPort());
		    socket.send(packet);
		}
		System.out.print("\n");
		
	    }
	}
	catch(UnknownHostException e) {
	    System.err.println("WARNING: " + e.getMessage());
	}
	catch(InterruptedException e) {
	    System.err.println("WARNING: Unicast send thread was interrupted; returning...");
	    return;
	}
	catch(IOException e) {
	    System.err.println("ERROR: " + e.getMessage());
	    e.printStackTrace();
	    return;
	}
    }
}

class RoutingTable implements Serializable {
    private String hostName;
    private HashMap<String, HashMap<String, Double>> table;
    private HashSet<String> neighbors;
    public static final Integer MAX_NODES = 15;
    
    public class TableEntry {
	public String key;
	public Double cost;
	
	public TableEntry(String host, Double cost) {
	    this.key = host;
	    this.cost = cost;
	}
    }
    
    public RoutingTable(HashMap<String, Double> init) {
	try {
	    hostName = InetAddress.getLocalHost().getHostName().split("\\.")[0];
	}
	catch(UnknownHostException e) {
	    System.err.println("FATAL ERROR: Wow, okay, so apparently localhost is an unknown host? Fascinating. Well I guess I'll kill myself then.");
	    System.exit(-1);
	}
	table = new HashMap<String, HashMap<String, Double>>();
	neighbors = new HashSet<String>();
	
	for(String key : init.keySet()) {
	    neighbors.add(key);
	}
	
	for(String key : init.keySet()) {
	    addHost(key);
	    put(key, key, init.get(key));
	}
    }
    
    public void addHost(String host) {
	if(hasHost(host))
	    return;
	
	HashMap<String, Double> row = new HashMap<String, Double>();
	for(String neighbor : neighbors) {
	    row.put(neighbor, Double.MAX_VALUE);
	}
	table.put(host, row);
    }
    
    public synchronized boolean hasHost(String host) {
	return table.containsKey(host);
    }
    
    public boolean hasNeighbor(String host) {
	return neighbors.contains(host);
    }
    
    public synchronized Double get(String dest, String via) {
	if(!hasHost(dest) || !hasNeighbor(via))
	    return Double.POSITIVE_INFINITY;
	
	return table.get(dest).get(via);
    }
    
    public synchronized boolean put(String dest, String via, Double cost) {
	if(!hasHost(dest) || !hasNeighbor(via))
	    return false;
	
	table.get(dest).put(via, cost);
	return true;
    }
    
    public synchronized TableEntry findShortestTo(String dest, String exclude) {
	// if we haven't heard of this host, disregard request
	if(!hasHost(dest))
	    return null;
	
	TableEntry ent = new TableEntry(null, Double.MAX_VALUE);
	// for each neighbor,
	for(String via : table.get(dest).keySet()) {
	    // exclude any cause of two-node cycles; else,
	    if(via.equals(exclude))
		continue;
	    
	    // if we've found a new minimum cost, update
	    Double cost = get(dest, via);
	    if(cost < ent.cost) {
		ent.key = via;
		ent.cost = cost;
	    }
	}
	
	return ent;
    }
    
    public synchronized void update(RoutingTable nTable) {
	if(nTable == null)
	    return;
	
	// for each known host of a neighboring table,
	for(String host : nTable.getKnownHosts()) {
	    // again, protects against two-node cycles
	    if(hostName.equals(host))
		continue;
	    // add host if we don't yet know of it
	    if(!hasHost(host))
		addHost(host);
	    
	    // compare neighbor's shortest path (+ hop) to what we think is the shortest path
	    Double hop = get(nTable.getHostName(), nTable.getHostName());
	    Double current = get(host, nTable.getHostName());
	    Double nCurrent = nTable.findShortestTo(host, hostName).cost + hop;
	    
	    // put neighbor's value if it's lower or if neighbor experienced a cost change
	    if(nCurrent < current/* || nTable.hasLocalChange()*/) {
		put(host, nTable.getHostName(), nCurrent);
	    }
	}
    }
    
    public synchronized Set<String> getKnownHosts() {
	return table.keySet();
    }
    
    public Set<String> getNeighbors() {
	return neighbors;
    }
    
    public String getHostName() {
	return hostName;
    }
    
    public String toString() {
	return hostName + "\n" + table.toString();
    }
}
/*
class UnicastRoutingTable implements Serializable {
    private String hostName;
    private HashMap<String, HashMap<String, Double>> table;
    private HashSet<String> neighbors;
    
    public UnicastRoutingTable(String hostName, HashMap<String, HashMap<String, Double>> init) {
	this.hostName = hostName;
	this.table = new HashMap<String, HashMap<String, Double>>();
	this.neighbors = new HashSet<String>();
	
	initialize(init);
    }
    
    private void initialize(HashMap<String, HashMap<String, Double>> init) {
	// add rows for every host
	for(String h : init.keySet()) {
	    table.put(h, new HashMap<String, Double>());
	}
	
	for(String n : init.get(hostName).keySet()) {
	    neighbors.add(n);
	    // add via entry for each neighbor
	    for(String h : init.keySet()) {
		table.get(h).put(n, Double.MAX_VALUE);
	    }
	    
	    // put initial neighbor value
	    Double nCost = init.get(hostName).get(n);
	    table.get(n).put(n, nCost);
	}
	
	
    }
}
*/