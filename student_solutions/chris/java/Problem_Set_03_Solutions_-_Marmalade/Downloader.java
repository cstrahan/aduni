import java.io.*;
import java.net.*;

public class Downloader extends Thread
{
    private int myindex;
    private String myname;
    private String myip;
    private int myport;
    private boolean oktodownload = false;
    private int filesize = 0;

    public Downloader (int index, String name, String ip, int port)
    {
	myindex = index;
	myname = name;
	myip = ip;
	myport = port;
    }

    public void run()
    {
	try
	    {System.out.println("Downloader started.");
		Socket s = new Socket(myip, myport);
		Connection connection = new Connection(s, Connection.DOWNLOADING);

		Searcher.addFileTransfer(connection.getIPAddress(), myname);

		String greetstring = ("GET /get/" + myindex + "/" + myname + " HTTP/1.0\r\nConnection: Keep-Alive\r\nRange: bytes=0-\r\n\r\n"); 
		byte[] greeting = greetstring.getBytes();

		connection.getByteWriter().write(greeting, 0, greeting.length);
		connection.getByteWriter().flush();
		
		String responseline;

		while (!((responseline = connection.getTextReader().readLine()).equals(""))) // Run through the HTTP header
		    {
			responseline = connection.getTextReader().readLine();
			if (responseline.startsWith("Content-length: "))
			    {
				filesize = Integer.parseInt(responseline.substring(16)); // Start reading right after the space
				oktodownload = true;
				Searcher.updateConnectionStatus(connection.getIPAddress(), myname, "Received handshake...");
			    }
		    }

		if (oktodownload)
		    {
			File towrite = new File((SharedDirectory.getOurSavePath().getPath() + File.separatorChar + myname));
			if (towrite.createNewFile())
			    {
				BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(towrite));
				Searcher.updateConnectionStatus(connection.getIPAddress(), myname, "Downloading...");
				for (int i = 0; i < filesize; i++)
				    {
				    out.write((byte)connection.getByteReader().read());
				    if (((i % 10000) == 0) && (i != 0)) // Give the user an update every 10 kb of downloading.
					// Good Lord, Java's typecasting leads to absurd parentheses! -------------v
					Searcher.updateFileTransferStatus(connection.getIPAddress(), myname, ((int)((((double)i) / filesize) * 100)));
				    }
				Searcher.updateFileTransferStatus(connection.getIPAddress(), myname, 100);
				Searcher.updateConnectionStatus(connection.getIPAddress(), myname, "Complete.");
			    }
			else
			    {
				Searcher.updateConnectionStatus(connection.getIPAddress(), myname, "Unable to create new file in shared directory.");
			    }
		    }
		else
		    {
			Searcher.updateConnectionStatus(connection.getIPAddress(), myname, "Bad HTTP handshake.");
		    }
	    }
	catch (IOException e)
	    {
		System.out.println("Unable to connect.");
	    }
    }
}
		



