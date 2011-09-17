public class Gum
{
  public static void main(String[] args)
  {
    Walk walk = new Walk();
    Chew chew = new Chew();
    Pat pat = new Pat();
    walk.start();
    chew.start();
    pat.start();
  }
}

class Walk extends Thread
{
  public void run()
  {
    for(;;)
    {
      try
      {
        System.out.print("Left, ");
        sleep(500);
        System.out.print("Right, ");
        sleep(500);
      }
      catch(InterruptedException e) {}
    }
  }
}

class Chew extends Thread
{
  public void run()
  {
    for(;;)
    {
      try
      {
        time = (int)(Math.random() * 7000);
        sleep(time);
        System.out.println("Chew!");
      }
      catch(InterruptedException e) {}
    }
  }
  private int time;
}


  
class Pat extends Thread
{
  public void run()
  {
    for(;;)
    {
      try
      {
        time = (int)(Math.random() * 16420);
        sleep(time);
        System.out.print("<pat>, ");
      }
      catch(InterruptedException e) {}
    }
  }
  private int time;
}      
