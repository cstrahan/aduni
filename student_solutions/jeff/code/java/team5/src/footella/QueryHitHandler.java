/**
   QueryHitHandler.java
   part of footella
   @author JMR
   @version $Id: QueryHitHandler.java,v 1.3 2001/01/31 18:32:23 jeff Exp $

   A simple class to take query hits and report them to the control handler
*/

import java.util.*;

class QueryHitHandler {
  QueryHitHandler(ControlHandler c) {
    control = c;
  }

  public void reportResult(QueryHitObject q)
  {
    Vector v = q.getResultSet();
    for(int i = 0; i < v.size(); i++) {
      ResultTableObject r = 
        new ResultTableObject((ResultObject)v.get(i), q);
      control.addResult(r);
    }
  }
  private ControlHandler control;
}



















