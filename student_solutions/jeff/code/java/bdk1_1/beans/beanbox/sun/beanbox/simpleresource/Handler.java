package sun.beanbox.simpleresource;

import java.net.*;
import java.io.*;

public class Handler extends URLStreamHandler {
    public URLConnection openConnection(URL u) throws IOException {
	return new SimpleResourceConnection(u);
    }
}

