
import java.io.*;
import java.net.*;



class Download{

    public static void main(String[] args){
	Socket s = null;
	PrintWriter fout;
	BufferedReader fin;
	try{
	    s = new Socket(args[0],80);
	    System.out.println("connected");
	    fout = new PrintWriter(
                        new OutputStreamWriter(s.getOutputStream()));
	    fin = new BufferedReader(
                      new InputStreamReader(s.getInputStream()));
	    String path = "GET " + args[1] +  " HTTP/1.0\n\n";
	    // send GET
	    fout.print(path);
	    // Must flush or command never gets sent!!
	    fout.flush();
	    //read response
	    String line;
	    System.out.println("sent request");
	    while((line = fin.readLine()) != null){
		System.out.println(line);
	    }
	    s.close();
	}
	catch(IOException e){
	    System.out.println("Cannot connect");
	}
    }

}
