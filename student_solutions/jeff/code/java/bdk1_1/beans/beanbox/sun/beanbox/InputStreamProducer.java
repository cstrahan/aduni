package sun.beanbox;

/**
 * An interface for producing an input stream on demand
 *
 * A null means that the resource is not (or no longer) available.
 * So, a one-shot producer may provide a value the first time but not
 * after that.
 */

import java.io.*;

public interface InputStreamProducer {
    InputStream getInputStream();
}
