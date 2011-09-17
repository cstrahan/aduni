// Defines a customer account
public class Account
{
    // Constructor
    public Account(int accountNumber, int balance)
    {
	this.accountNumber = accountNumber;
	this.balance = balance;
    }

    // Return the current balance
    public int getBalance()
    {
	return balance;
    }

    public int getAccountNumber()
    {
	return accountNumber;
    }
    
    public void setBalance(int balance)
    {
	this.balance = balance;
    }

    public String toString()
    {
	return "A//C No. "+accountNumber+" : $"+balance;
    }

    private int balance;
    private int accountNumber;
}

    
    
	
    





