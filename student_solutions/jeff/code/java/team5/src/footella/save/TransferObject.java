class TransferObject
{
  TransferObject(ResultTableObject object)
  {
    this.object = object;
  }

  public String[] getInfo() {
    String[] temp = new String[3];
    temp[0] = object.getFileName();
    temp[1] = "" + object.getFileSize();
    temp[2] = "" + transfer;
    return temp;
  }
        
  public void updateTransfer(int t)
  {
    transfer = t;
  }
  
  private int transfer;
  private ResultTableObject object;
}

    
