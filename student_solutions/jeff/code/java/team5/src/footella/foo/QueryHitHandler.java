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
