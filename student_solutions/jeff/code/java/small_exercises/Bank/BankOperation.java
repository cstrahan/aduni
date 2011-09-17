import java.util.Random;

public class BankOperation
{
    public static void main(String[] args)
    {
	int[] initialBalance = {500, 800};       // The intial account balance
	int[] totalCredits = new int[initialBalance.length];           // Total credits on the account
	int[] totalDebits = new int[initialBalance.length];            // Total debits on the account
	int transactionCount = 20;      // Number of debits and credits

	// Create the the bank and the clerks
	Bank theBank = new Bank();
	Clerk clerk1 = new Clerk(theBank);
	Clerk clerk2 = new Clerk(theBank);

	// Create the accounts, and initialize total credits and debits
	Account[] accounts = new Account[initialBalance.length];
	for(int i = 0; i < initialBalance.length; i++)
	    {
		accounts[i] = new Account(i + 1, initialBalance[i]); // Create accounts
		totalCredits[i] = totalDebits[i] = 0;
	    }

	// Create the threads for the clerks as daemon, and start them off
	Thread clerk1Thread = new Thread(clerk1);
	Thread clerk2Thread = new Thread(clerk2);
	clerk1Thread.setDaemon(true);
	clerk2Thread.setDaemon(true);
	clerk1Thread.start();
	clerk2Thread.start();

	// Create transactions randomly distributed between the accounts
	Random rand = new Random();
	Transaction transaction;
	int amount;
	int select;
	for(int i = 1 ; i <= transactionCount ; i++)
	    {
		select = rand.nextInt(accounts.length);
		amount = 50 + rand.nextInt(26);
		transaction = new Transaction(accounts[select],
					      Transaction.CREDIT,
					      amount);
		totalCredits[select] += amount;

		// Wait until the first clerk is free
		while(clerk1.isBusy()) 
		    try
			{
			    Thread.sleep(25);
			}
		    catch(InterruptedException e)
			{
			    System.out.println(e);
			}
	
		
		clerk1.doTransaction(transaction);

		// generate a random account index for debit operation
		select = rand.nextInt(accounts.length);
		amount = 30 + rand.nextInt(31);
		transaction = new Transaction(accounts[select],
					      Transaction.DEBIT,
					      amount);

		totalDebits[select] += amount;

		// Wait until the second clerk is free
		while(clerk2.isBusy())
		    try
			{
			    Thread.sleep(25);
			}
		    catch(InterruptedException e)
			{
			    System.out.println(e);
			}
		

		clerk2.doTransaction(transaction);
	    }

	   // Wait until both Clerks are done
	   while(clerk1.isBusy() || clerk2.isBusy()) 
	       try {
		   Thread.sleep(25);
	       }
	       catch(InterruptedException e) {
		   System.out.println(e);
	       }
	

	   // Output Results
	   for(int i = 0; i < accounts.length ; i++)
	       {
		   
		   System.out.println("Account Number: " + accounts[i].getAccountNumber() + "\n" +
				      "Original balance    : $" + initialBalance[i] + "\n" +
				      "Total credits       : $" + totalCredits[i]+ "\n" +
				      "Total debits        : $" + totalDebits[i] + "\n" +
				      "Final balance       : $" + accounts[i].getBalance() + "\n" +
				      "Should be           : $" + (initialBalance[i] + totalCredits[i] - totalDebits[i]));
	       }
    }
}
