
import java.io.*;
import java.net.*;

public class GnugattiPacket implements Serializable {
    private Long qid;
    private Integer ttl;
    private String contents;
    private InetAddress origin;
    
    private static final Integer DEFAULT_TTL = 10;//0;//300;
    
    public GnugattiPacket(long q, String c, InetAddress o) {
	this(q, DEFAULT_TTL, c, o);
    }
    
    public GnugattiPacket(long q, int t, String c, InetAddress o) {
	qid = q;
	ttl = t;
	contents = c;
	origin = o;
    }
    
    public static long computeQid(int peerfix, int chash) {
	return ((long) peerfix << 32) + (long) chash;
    }
    
    public Long getQid() {
	return qid;
    }
    
    public Integer getTTL() {
	return ttl;
    }
    
    public String getContents() {
	return contents;
    }
    
    public InetAddress getOrigin() {
	return origin;
    }
    
    public boolean isQuery() {
	return contents.substring(0, 6).equals("QUERY:");
    }
    
    public boolean isReply() {
	return contents.substring(0, 6).equals("REPLY:");
    }
}
