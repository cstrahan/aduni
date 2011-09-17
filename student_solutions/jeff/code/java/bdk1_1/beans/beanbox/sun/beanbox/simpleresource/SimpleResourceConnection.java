package sun.beanbox.simpleresource;

import java.net.*;
import java.io.*;
import sun.beanbox.*;

public class SimpleResourceConnection extends URLConnection {
    private static boolean debug = false;

    private Object resource;	// the resource we are fetching
    private String cookie;	// identification of the loader instance to use
    private String name;	// name of the resource
    private final String prefix = SimpleClassLoader.urlPrefix;
    private final int prefixLength = prefix.length();

    protected SimpleResourceConnection (URL url)
		throws MalformedURLException, IOException
    {
	super(url);
	debug("SimpleResourceConnection("+url+")");
	String file = url.getFile();
	if (file.startsWith("/")) {
	    file = file.substring(1);
	}
	if (! file.startsWith(prefix)) {
	    throw new MalformedURLException("SimpleResource file should start with /SIMPLE");
	}
	cookie = file.substring(prefixLength, file.indexOf("/+/"));
	name = file.substring(file.indexOf("/+/")+3);

	debug(" cookie: "+cookie);
	debug(" name: "+name);
    }

    public void connect() throws IOException {
	debug("Looking for "+cookie+", "+name+" in SimpleResourceLoader");
	Object o = SimpleClassLoader.getLocalResource(cookie, name);
	if (o == null) {
	    debug("Invalid resource name");
	    resource = null;
	    return;
	} else {
	    debug("Found resource "+o);
	    resource = o;
	}
    }

    public Object getContent() throws IOException {
	if (!connected) {
	    connect();
	}
	return resource;
    }

    public InputStream getInputStream() throws IOException {
	if (!connected) {
	    connect();
	}

	if (resource instanceof InputStream) {
	    return (InputStream) resource;
	}
	return SimpleClassLoader.getLocalResourceAsStream(cookie, name);
    }


    private void debug(String msg) {
	if (debug) {
	    System.err.println(msg);
	}
    }


}
