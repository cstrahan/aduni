public class LinkedListTest
{
  public static void main(String[] args)
  {
    System.out.println("TESTING INTEGERS");
    
    PQueue pq = new PQueue();
    Comparable test = new Integer(1);

    System.out.println(pq.length());
    pq.insert(test);
    System.out.println(pq.length());
    System.out.println(pq.contents());

    test = new Integer(7);
    
    pq.insert(test);
    System.out.println(pq.contents());
    System.out.println(pq.length());

    test = new Integer(112);
    
    pq.insert(test);
    System.out.println(pq.contents());
    System.out.println(pq.length());

    test = new Integer(4);
    
    pq.insert(test);
    System.out.println(pq.contents());
    System.out.println(pq.length());

    System.out.println(pq.removeMax());

    //---------------------------------------------//

    System.out.println("TESTING STRINGS");
    
    //    PQueue p2 = new PQueue();
    Comparable test2 = new String("A");

    System.out.println(pq.length());
    //pq.insert(test2);
    System.out.println(pq.length());
    System.out.println(pq.contents());

    test2 = new String("C");
    
    pq.insert(test2);
    System.out.println(pq.contents());
    System.out.println(pq.length());

    test2 = new String("B");
    
    pq.insert(test2);
    System.out.println(pq.contents());
    System.out.println(pq.length());

    test2 = new String("Ant");
    
    pq.insert(test2);
    System.out.println(pq.contents());
    System.out.println(pq.length());


    System.out.println(pq.removeMax());

  }
}

