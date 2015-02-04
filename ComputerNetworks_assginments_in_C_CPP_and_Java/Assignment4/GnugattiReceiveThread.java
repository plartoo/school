
import java.io.*;
import java.net.*;
import java.util.*;

public class GnugattiReceiveThread extends Thread {
    private GnugattiPeer peer;
    private Socket socket;
    
    public GnugattiReceiveThread(GnugattiPeer gpeer, Socket psocket) {
	peer = gpeer;
	socket = psocket;
    }
    
    public void run() {
	// ...
	try {
	    ObjectInputStream oin = new ObjectInputStream(socket.getInputStream());
	    GnugattiPacket rpacket = (GnugattiPacket) oin.readObject();
	    long rqid = rpacket.getQid();
	    int rttl = rpacket.getTTL();
	    String rdata = rpacket.getContents();
	    InetAddress rsrc = rpacket.getOrigin();
	    InetAddress nrec = socket.getInetAddress();
	    
	    if(rpacket.isQuery()) {
		if(peer.hasHandledQuery(rqid)) {
		    synchronized(Gnugatti.printlock) {
			System.out.println("[Gnugatti Receive] Already handled query " + rqid);
		    }
		    return;
		}
		peer.handleQuery(rqid);
		
		synchronized(Gnugatti.printlock) {
		    System.out.println("[Gnugatti Receive] Received packet:");
		    System.out.println("\tSource: " + rsrc.getHostName());
		    System.out.println("\tQuery ID: " + rqid);
		    System.out.println("\tTTL: " + (rttl - 1));
		}
		
		String srch = rdata.substring(7, rdata.length());
		ArrayList<String> matches = peer.findMatches(srch);
		
		String resp = "";
		InetAddress localhost = null;
		try {
		    localhost = InetAddress.getLocalHost();
		}
		catch(UnknownHostException e) {
		    System.err.println("ERROR: couldn't get local host address, for some reason...?");
		    return;
		}
		
		for(String match : matches) {
		    synchronized(Gnugatti.printlock) {
			System.out.println("[Gnugatti Receive] Found match for " + srch + ": " + match);
		    }
		    if(match == matches.get(matches.size() - 1))
			resp += match;
		    else
			resp += match + ", ";
		}
		
		if(!resp.isEmpty()) {
		    Socket conn = new Socket(rsrc, Gnugatti.port);
		    GnugattiPacket cpacket = new GnugattiPacket(rqid, -1, "REPLY: " + resp, localhost);
		    ObjectOutputStream oout = new ObjectOutputStream(conn.getOutputStream());
		    oout.writeObject(cpacket);
		    conn.close();
		}
		
		if(rttl - 1 <= 0) {
		    synchronized(Gnugatti.printlock) {
			System.out.println("[Gnugatti Receive] Query " + rqid + " from " + rsrc + " has expired.");
		    }
		    return;
		}
		else {
		    GnugattiPacket spacket = new GnugattiPacket(rqid, rttl - 1, rdata, rsrc);
		    for(InetAddress neighbor : peer.getNeighbors()) {
			if(!neighbor.equals(nrec)) {
			    System.out.println("[Gnugatti Receive] Flooding peer query to " + neighbor.getHostName());
			    Socket nconn = new Socket(neighbor, Gnugatti.port);
			    ObjectOutputStream ooout = new ObjectOutputStream(nconn.getOutputStream());
			    ooout.writeObject(spacket);
			    nconn.close();
			}
		    }
		}
	    }
	    else if(rpacket.isReply()) {
		String matches = rdata.substring(7, rdata.length());
		synchronized(Gnugatti.printlock) {
		    System.out.println("[Gnugatti Receive] " + rsrc.getHostName() + " found matches for our query " + rqid + ": ");
		    for(String match : matches.split(",\\s")) {
			System.out.println("\t" + match);
		    }
		}
		//System.out.print("[" + matches + "]\n");
	    }
	    else {
		peer.addNeighbor(rsrc);
		System.out.print("[Gnugatti Receive] Contacted by " + rsrc.getHostName());
		System.out.print(" with neighborhood establishment message.\n");
		/*
		System.out.println("[Gnugatti Receive] Current neighbors:");
		for(InetAddress neighbor : peer.getNeighbors()) {
		    System.out.println("\t[Neighbor] " + neighbor.getHostName());
		}
		*/
	    }
	}
	catch(EOFException e) {
	    System.err.println("ERROR: connection to " + socket.getInetAddress().getHostName() + " closed unexpectedly.");
	    return;
	}
	catch(IOException e) {
	    System.err.println("ERROR: could not open connection to neighbor...is it running currently?");
	    return;
	}
	catch(ClassNotFoundException e) {
	    System.err.println("ERROR: could not cast packet received from peer as GnugattiPacket.");
	    e.printStackTrace();
	    return;
	}
    }
}
