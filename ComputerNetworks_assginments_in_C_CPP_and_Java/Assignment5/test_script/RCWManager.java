import java.util.*;
import java.io.*;
import java.net.*;

public class RCWManager {
	public static void main(String[] args) {		
		new RCWManager();
	}
	// constructor
	// 
	public RCWManager() {
		calAndWrite();
	}
	// calculate and write based on readFile()
	// 
	public void calAndWrite() {	
		// write to a file
		try {
			File file = new File("result.txt"); 
			if (!file.exists()) {
				file.createNewFile();
			} 
			FileWriter fw = new FileWriter(file.getAbsoluteFile());
			BufferedWriter bw = new BufferedWriter(fw);
			
			String avgRcv = "Avg. Received Content (bytes): ";
			String avgRcvTotal = "Avg. Received Total (bytes): ";
			String avgSnt = "Avg. Sent Content (bytes): ";
			String avgSntTotal = "Avg. Sent Total (bytes): ";
			
			String dir = "./";
			ArrayList<File> fileList = new ArrayList<File>();
			listAllFiles(dir, fileList);
			// lists to hold rcvContent, rcvTotal, sentContent, sentTotal
			int[] rcvContent = new int[1];
			int[] rcvTotal = new int[1];
			int[] sentContent = new int[1];
			int[] sentTotal = new int[1];
			// post four lines
			HashMap<String, Double> postFourHm = new HashMap<String, Double>();
			
			for (int i=0; i<fileList.size(); i++) {
				String filePath = fileList.get(i).toString();
				readFile(filePath, rcvContent, rcvTotal, sentContent, sentTotal, postFourHm);
			} 
			
			System.out.printf("\nBefore Avg: \nrcvContent == %d, rcvTotal == %d, sentContent == %d, sentTotal == %d", rcvContent[0], rcvTotal[0], sentContent[0], sentTotal[0]);
			rcvContent[0] = rcvContent[0] / fileList.size();
			rcvTotal[0] = rcvTotal[0] / fileList.size();
			sentContent[0] = sentContent[0] / fileList.size();
			sentTotal[0] = sentTotal[0] / fileList.size();
			
			bw.write(avgRcv + rcvContent[0] + "\n");
			bw.write(avgRcvTotal + rcvTotal[0] + "\n");
			bw.write(avgSnt + sentContent[0] + "\n");
			bw.write(avgSntTotal + sentTotal[0] + "\n");
			
			System.out.printf("\nAfter Avg: \nrcvContent == %d, rcvTotal == %d, sentContent == %d, sentTotal == %d", rcvContent[0], rcvTotal[0], sentContent[0], sentTotal[0]);
			
			double cycle1Cost = 0.0;              int cycle1Cnt = 0;
			double cycle2Cost = 0.0;              int cycle2Cnt = 0;
			double cycle3Cost = 0.0;              int cycle3Cnt = 0;
			double marcyCost = 0.0;               int marcyCnt = 0;
			double nyeCost = 0.0;                 int nyeCnt = 0;
			double sawteethCost = 0.0;            int sawteethCnt = 0;
			double sewardCost = 0.0;              int sewardCnt = 0;
			
			System.out.println("\nHashMap size == " + postFourHm.size());
			// iterate over HashMap postFourHm		
			for (Map.Entry<String, Double> entry : postFourHm.entrySet()) {
				System.out.println(entry.getKey() + " == " + entry.getValue());
				String[] key = entry.getKey().split("-");
				String from = key[0];
				double cost = entry.getValue();
				if (from.equals("cycle1")) {
					cycle1Cost += cost;	
					cycle1Cnt++;				
				}
				else if (from.equals("cycle2")) {
					cycle2Cost += cost;	
					cycle2Cnt++;
				}
				else if (from.equals("cycle3")) {
					cycle3Cost += cost;	
					cycle3Cnt++;
				}
				else if (from.equals("marcy")) {
					marcyCost += cost;	
					marcyCnt++;
				}
				else if (from.equals("nye")) {
					nyeCost += cost;	
					nyeCnt++;
				}
				else if (from.equals("sawteeth")) {
					sawteethCost += cost;	
					sawteethCnt++;
				}
				else if (from.equals("seward")) {
					sewardCost += cost;	
					sewardCnt++;
				}
			}
			
			bw.write("Avg Cost from cycle1: " + cycle1Cost/cycle1Cnt + ", (" + cycle1Cost + " / " + cycle1Cnt + ")\n");
			bw.write("Avg Cost from cycle2: " + cycle2Cost/cycle2Cnt + ", (" + cycle2Cost + " / " + cycle2Cnt + ")\n");
			bw.write("Avg Cost from cycle3: " + cycle3Cost/cycle3Cnt + ", (" + cycle3Cost + " / " + cycle3Cnt + ")\n");
			bw.write("Avg Cost from marcy: " + marcyCost/marcyCnt + ", (" + marcyCost + " / " + marcyCnt + ")\n");
			bw.write("Avg Cost from nye: " + nyeCost/nyeCnt + ", (" + nyeCost + " / " + nyeCnt + ")\n");
			bw.write("Avg Cost from sawteeth: " + sawteethCost/sawteethCnt + ", (" + sawteethCost + " / " + sawteethCnt + ")\n");
			bw.write("Avg Cost from seward: " + sewardCost/sewardCnt + ", (" + sewardCost + " / " + sewardCnt + ")\n");
			
			bw.close(); 
			System.out.println("Done writing"); 
			
			System.out.printf("\nAvg Cost from cycle1: %.2f, (%.2f / %d)", cycle1Cost/cycle1Cnt, cycle1Cost, cycle1Cnt);
			System.out.printf("\nAvg Cost from cycle2: %.2f, (%.2f / %d)", cycle2Cost/cycle2Cnt, cycle2Cost, cycle2Cnt);
			System.out.printf("\nAvg Cost from cycle3: %.2f, (%.2f / %d)", cycle3Cost/cycle3Cnt, cycle3Cost, cycle3Cnt);
			System.out.printf("\nAvg Cost from marcy: %.2f, (%.2f / %d)", marcyCost/marcyCnt, marcyCost, marcyCnt);
			System.out.printf("\nAvg Cost from nye: %.2f, (%.2f / %d)", nyeCost/nyeCnt, nyeCost, nyeCnt);
			System.out.printf("\nAvg Cost from sawteeth: %.2f, (%.2f / %d)", sawteethCost/sawteethCnt, sawteethCost, sawteethCnt);
			System.out.printf("\nAvg Cost from seward: %.2f, (%.2f / %d)", sewardCost/sewardCnt, sewardCost, sewardCnt);
			System.out.print("\n");
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}
	// all the files in the current folder for readFile()
	// 
	public void listAllFiles(String directory, ArrayList<File> fileList) {		
		System.out.println(directory + " is searched to find textual included files or directories");
		File file = new File(directory);
		FilenameFilter fnf = new FilenameFilter() {
			public boolean accept(File f, String name) {
				return (name.indexOf((int)'.') == -1);
			}	
		};
		File[] filePath = file.listFiles(fnf);
		if (filePath == null || filePath.length == 0) {
			// System.out.println("NULL returned array of abstract pathname");
			return;
		}			
		for (int i=0; i<filePath.length; i++) {
			if (filePath[i].isDirectory()) {
				directory += filePath[i].toString();
				listAllFiles(directory, fileList);
			}
			else if (filePath[i].isFile()) {
				fileList.add(filePath[i]);
			}
			// System.out.println(i + ": " + filePath[i]);
		}
	}
	
	// read one file, meanwhile calculate first four content items
	// 
	public void readFile(String file, int[] rcvContent, int[] rcvTotal, int[] sentContent, int[] sentTotal, HashMap<String, Double> postFourHm) {		
        FileInputStream fis = null;
		InputStreamReader isr = null;
		BufferedReader br = null;
		try {
			fis = new FileInputStream(file);
			isr = new InputStreamReader(fis);
			br = new BufferedReader(isr);
			String aLine;
			int count = 0;
			while ((aLine = br.readLine()) != null) {
				String[] words = aLine.split(" ");
				if (count > 0 && count <= 4) {					
					if (count == 1) {
						// System.out.println("words length == " + words.length);
						rcvContent[0] += Integer.parseInt(words[3]);
					} 
					if (count == 2) {
						rcvTotal[0] += Integer.parseInt(words[3]);
					}
					if (count == 3) {
						sentContent[0] += Integer.parseInt(words[3]);
					}
					if (count == 4) {
						sentTotal[0] += Integer.parseInt(words[3]);
					}
				}
				else if (count >= 5) {
					String[] hostFile = file.split("\\.txt");
					String host = hostFile[0];
					String[] startNode = words[3].split("\\:");
					String start = startNode[0];
					postFourHm.put(start + "-" + host, Double.parseDouble(words[4]));
				}
				count++;
			}
		}
		catch(FileNotFoundException ex) {
			 System.err.println("Could not find file.");
		}
		catch(IOException e) {
			System.err.println(e);
		}
		finally {
            try {
                br.close();
				isr.close();
                fis.close();
            } 
			catch(IOException ex) {
				System.out.println(ex);
            }
        }		
    }	
}