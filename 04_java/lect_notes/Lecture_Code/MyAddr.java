
import java.io.*;
import java.net.*;

class MyAddr{

    public static void main(String[] args){
	try{
	    InetAddress in = InetAddress.getLocalHost();
	    System.out.println(in.getHostName());
	    System.out.println(in.getHostAddress());
	}
	catch(IOException e){
	}
    }

}

