
package sun.beanbox;

/**
 * A utility class to help time internal operations.
 *						KGH 1/20/97
 */

import java.util.Date;

public class Timer {

    private long startMillis;
    private long endMillis;

    public Timer() {
	reset();
    }

    public Timer(boolean running) {
	reset();
	if (!running) {
	    endMillis = startMillis;
	}
    }

    public void reset() {
	startMillis = getMillis();
	endMillis = 0;
    }

    public void stop() {
	endMillis = getMillis();
    }

    public String elapsed() {
	long millis = delta();
	if (millis > 1000L)  {
	   char xx[] = new char[2];
	   xx[0] = (char)('0' + (millis % 1000)/100);
	   xx[1] = (char)('0' + ((millis+5) % 100)/10);
	   return ("" + millis/1000 + "." + new String(xx) + " sec");
	} else {
	   return ("" + millis + " millis");
	}
    }

    public void add(Timer tim) {
	if (endMillis == 0) {
	    throw new Error("Can only add to a stopped Timer");
	}
	endMillis += tim.delta();
    }

    private long delta() {
	if (endMillis == 0) {
	    stop();
	}
	return (endMillis - startMillis);
    }

    private long getMillis() {
	return (new Date()).getTime();
    }

}
