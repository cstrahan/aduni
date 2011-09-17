
// This interface describes the method that gets fired by the Bridge Tester
// bean 

package sunw.demo.test;

public interface BridgeTesterListener extends java.util.EventListener {
    
    void eventNumber1(BridgeTesterEvent ble);
    void eventNumber2(String e);
    void eventNumber3(short s);
    void eventNumber4(int[] intArray);
}
