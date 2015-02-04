
import java.io.*;
import java.net.*;
import java.util.*;

public class GnugattiSendThread extends Thread {
    private GnugattiPeer peer;
    private Scanner queryScan;
    
    public GnugattiSendThread(GnugattiPeer gpeer) {
	peer = gpeer;
	queryScan = new Scanner(System.in);
    }
    
    public void run() {
	try {
	    System.out.println("[Gnugatti Send] Enter query at any time!");
	    while(true) {
		String prefix = queryScan.nextLine();
		long qid = GnugattiPacket.computeQid(peer.getHashPrefix(), prefix.hashCode());
		if(peer.hasHandledQuery(qid)) {
		    System.out.println("[Gnugatti Send] We have already sent query for " + prefix);
		    continue;
		}
		else {
		    peer.handleQuery(qid);
		}
		String req = "QUERY: " + prefix;
		InetAddress me = InetAddress.getLocalHost();
		GnugattiPacket qpacket = new GnugattiPacket(qid, req, me);
		
		ArrayList<String> localmatches = peer.findMatches(prefix);
		if(!localmatches.isEmpty()) {
		    synchronized(Gnugatti.printlock) {
			System.out.println("[Gnugatti Send] Found local matching files for " + prefix + ": ");
			for(String match : localmatches) {
			    System.out.println("\t" + match);
			}
		    }
		}
		
		for(InetAddress neighbor : peer.getNeighbors()) {
		    Socket nconn = new Socket(neighbor, Gnugatti.port);
		    ObjectOutputStream oout = new ObjectOutputStream(nconn.getOutputStream());
		    oout.writeObject(qpacket);
		    nconn.close();
		}
		
		synchronized(Gnugatti.printlock) {
		    System.out.println("[Gnugatti Send] Flooded query \"" + prefix + "\" to all neighbors.");
		}
	    }
	}
	catch(IOException e) {
	    e.printStackTrace();
	    return;
	}
    }
}