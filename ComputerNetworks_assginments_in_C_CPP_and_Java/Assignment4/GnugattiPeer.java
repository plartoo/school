
import java.io.*;
import java.net.*;
import java.util.*;

public class GnugattiPeer {
    private File directory;
    private ArrayList<String> files;
    private HashSet<InetAddress> neighbors;
    private String hostname;
    private HashSet<Long> handledQueries;
    
    public GnugattiPeer(String filepath) throws FileNotFoundException {
	directory = new File(filepath);
	if(!directory.exists())
	    throw new FileNotFoundException();
	files = new ArrayList<String>();
	addFiles(directory);
	neighbors = new HashSet<InetAddress>();
	//neighborAddrs = new HashSet<InetAddress>();
	handledQueries = new HashSet<Long>();
	try {
	    hostname = InetAddress.getLocalHost().getHostName();
	}
	catch(UnknownHostException e) {
	    hostname = "void";
	}
    }
    
    private void addFiles(File dir) {
	for(File f : dir.listFiles()) {
	    if(f.isDirectory())
		addFiles(f);
	    else
		files.add(f.getName());
	}
    }
    
    public synchronized void addNeighbor(String host) throws UnknownHostException {
	InetAddress naddr = InetAddress.getByName(host);
	if(neighbors.contains(naddr))
	    return;
	
	neighbors.add(naddr);
    }
    
    public synchronized void addNeighbor(InetAddress host) {
	if(neighbors.contains(host))
	    return;
	neighbors.add(host);
    }
    /*
      public void addNeighborConnection(String host, Socket conn) throws UnknownHostException {
      InetAddress naddr = InetAddress.getByName(host);
      if(neighbors.containsKey(naddr))
      return;
      
      neighbors.put(naddr, conn);
      }
      
      public void addNeighborConnection(InetAddress host, Socket conn) {
      if(neighbors.get(host) != null)
      return;
      
      neighbors.put(host, conn);
      }*/
    
    public ArrayList<String> findMatches(String prefix) {
	ArrayList<String> matches = new ArrayList<String>();
	for(String fname : files) {
	    if(fname.length() >= prefix.length() && fname.substring(0, prefix.length()).equals(prefix)) {
		matches.add(fname);
	    }
	}
	return matches;
    }
    
    public String getDirectory() {
	return directory.getAbsolutePath();
    }
    
    public synchronized HashSet<InetAddress> getNeighbors() {
	return neighbors;
    }
    /*
      public Socket getNeighborConnection(InetAddress addr) {
      return neighbors.get(addr);
      }*/
    
    public int getHashPrefix() {
	short hprefix = 0;
	for(byte b : hostname.getBytes()) {
	    hprefix += (short) b;
	}
	return (int) hprefix;
    }
    
    public synchronized boolean hasNeighbor(String host) throws UnknownHostException {
	return neighbors.contains(InetAddress.getByName(host));
    }
    
    public synchronized boolean hasNeighbor(InetAddress addr) {
	return neighbors.contains(addr);
    }
    
    public synchronized boolean handleQuery(long qid) {
	return handledQueries.add(qid);
    }
    
    public synchronized boolean hasHandledQuery(long qid) {
	return handledQueries.contains(qid);
    }
}
