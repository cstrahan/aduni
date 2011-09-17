public class Clerk implements Runnable
{
    private Bank theBank;         // The employer
    private Transaction inTray;   // the In try holding a transaction
    // Constructor
    public Clerk(Bank theBank)
    {
	this.theBank = theBank;
	inTray = null;
    }

    // Receive a transaction
    public void doTransaction(Transaction transaction)
    {
	inTray = transaction;
    }

    // The working clerk...
    public void run()
    {
	while(true)
	    {
		while(inTray == null)  // No transaction waiting?
		    {
			try
			    {
				Thread.sleep(150);    // Take a break
			    }
			catch(InterruptedException e)
			    {
				System.out.println(e);
			    }
		    }

	theBank.doTransaction(inTray);
	inTray = null;                         // Did the job, in-tray is empty
	    }
    }

    // Busy check
    public boolean isBusy()
    {
	return inTray != null;         // Full in tray is busy!
    }
}

		
	
	
	
    
