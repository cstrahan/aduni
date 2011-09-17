class PolyDriver
{
  public static void main(String[] args)
  {
    int[] aInt = { -1, 1};
    Poly aPoly = new Poly(aInt);

    System.out.println(aPoly);
    aPoly = aPoly.scale(-1);
    System.out.println(aPoly);

    Poly bPoly;
    bPoly = aPoly.mul(aPoly);
    System.out.println(bPoly);
  }
}

