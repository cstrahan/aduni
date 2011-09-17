class Transaction
{
    // Transaction types
    public static final int DEBIT = 0;
    public static final int CREDIT = 1;
    public static String[] types = {"Debit","Credit"};

    // Constructor
    public Transaction(Account account, int transactionType, int amount)
    {
	this.account = account;
	this.transactionType = transactionType;
	this.amount = amount;
    }

    public Account getAccount()
    {
	return account;
    }

    public int getAmount()
    {
	return amount;
    }

    public int getTransactionType()
    {
	return transactionType;
    }

    public String toString()
    {
	return types[transactionType] + " A//C: " + ": $" + amount;
    }

    private Account account;
    private int amount;
    private int transactionType;
}
