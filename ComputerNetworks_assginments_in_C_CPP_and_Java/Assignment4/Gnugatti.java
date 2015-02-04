
import java.io.*;
import java.net.*;
import java.util.*;

public class Gnugatti {
    public static Integer port;
    public static ServerSocket listenSocket;
    public static GnugattiPeer peer;
    public static Object printlock;
    
    public static void main(String[] args) {
	// correct in full effect
	if(args.length < 2)
	    System.out.println("USAGE: java GnugattiPeer <port> <share-dir> [<neighbor> ...]");
	
	// get port and peer info
	printlock = new Object();
	port = Integer.parseInt(args[0]);
	try {
	    peer = new GnugattiPeer(args[1]);
	}
	catch(FileNotFoundException e) {
	    System.err.println("FATAL ERROR: no such directory " + args[1] + ".");
	    System.exit(-1);
	}
	
	// add given neighbors
	for(int i = 2; i < args.length; i++) {
	    try {
		peer.addNeighbor(args[i]);
		Socket nconn = new Socket(InetAddress.getByName(args[i]), port);
		GnugattiPacket npacket = new GnugattiPacket(-1, -1, "HI NEIGHBOR", InetAddress.getLocalHost());
		ObjectOutputStream oout = new ObjectOutputStream(nconn.getOutputStream());
		oout.writeObject(npacket);
	    }
	    catch(UnknownHostException e) {
		System.err.println("ERROR: could not resolve hostname " + args[i] + ".");
	    }
	    catch(IOException e) {
		System.err.println("ERROR: could not send friendship packet to " + args[i]);
		e.printStackTrace();
	    }
	}
	
	// open a socket for listening
	try {
	    listenSocket = new ServerSocket(port);
	}
	catch(IOException e) {
	    System.err.println("FATAL ERROR: could not bind socket to port " + port + ".");
	    System.exit(-1);
	}
	
	new GnugattiSendThread(peer).start();
	
	// open TCP socket for peer communication
	while(true) {
	    try {
		Socket peerSocket = listenSocket.accept();
		if(!peer.hasNeighbor(peerSocket.getInetAddress()))
		   peer.addNeighbor(peerSocket.getInetAddress());
		new GnugattiReceiveThread(peer, peerSocket).start();
	    }
	    catch(IOException e) {
		System.err.println("ERROR: could not accept client connection on port " + port + ".");
	    }
	}
    }
}
