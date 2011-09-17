class TestFunc
{
  public static void main(String[] args)
  {
    double a = 1.0;
    double b = 3.0;
    double err = .000000001;
    
    double answer;
    CosFunc myCosFunc = new CosFunc();
    SinFunc mySinFunc = new SinFunc();
    
    answer = myCosFunc.bracketRoot(a, b, err);
    System.out.println(answer);

    a = 3.0;
    b = 4.0;
    
    double PI = mySinFunc.bracketRoot
      (a, b, err);
    
    a = 0.0;
    b = PI / 2;
    answer = myCosFunc.defIntegral(a, b, 1000000);
    System.out.println(answer);

    answer = myCosFunc.defIntegral(a, PI, 1000000);
    System.out.println(answer);
    
  }
}

    
