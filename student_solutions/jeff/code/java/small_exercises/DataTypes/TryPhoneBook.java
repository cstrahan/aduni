class TryPhoneBook
{
    public static void main(String[] args)
    {
	PhoneBook book = new PhoneBook();
	FormattedInput in = new FormattedInput();
	Person someone;

	for(;;)
	    {
		System.out.println("Enter 1 to enter a new phone book entry\n" +
				   "Enter 2 to find the number for a name entry\n" +
				   "Enter 3 to list all the entries\n" +
				   "Enter 4 to quit.");

		int what = in.intRead(); // User option
		switch(what)
		    {
		    case 1:
			book.addEntry(BookEntry.readEntry());
			break;
		    case 2:
			someone = Person.readPerson();
			BookEntry entry = book.getEntry(someone);
			if(entry == null)
			    System.out.println("The number for " + someone
					       + " was not found. ");
			else System.out.println("The number for " + someone + " is "
						+ book.getEntry(someone).getNumber());
			break;
		    case 3:
			book.listEntries();
			break;
		    case 4:
			book.save();
			System.out.println("Ending program.");
			return;
		    default:
			System.out.println("Invalid selection -- please try again.");
			break;
		    }
	    }
    }
}
