// Define the bank
class Bank
{
  // Perform a transaction
  synchronized public void doTransaction(Transaction transaction)
  {
    int balance = transaction.getAccount().getBalance();   // Get current balance

    switch(transaction.getTransactionType())
    {
      case Transaction.CREDIT:
        // Credits require a lot of checks...
        try
        {
          Thread.sleep(100);
        }
        catch(InterruptedException e)
        {
          System.out.println(e);
        }
        balance += transaction.getAmount();          // Increment the balance
        break;

      case Transaction.DEBIT:
        // Debits require even more checks...
        try
        {
          Thread.sleep(150);
        }
        catch(InterruptedException e)
        {
          System.out.println(e);
        }
        balance -= transaction.getAmount();          // Decrement the balance
        break;

      default:                                       // We should never get here
        System.out.println("Invalid transaction");
        System.exit(1);
   }
    transaction.getAccount().setBalance(balance);    // Restore the account balance
  }
}
