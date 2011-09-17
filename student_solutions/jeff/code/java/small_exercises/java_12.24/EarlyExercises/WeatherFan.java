public class WeatherFan
{
    public static void main(String[] args)
    {
	float[][] temperature = new float[10][365]; // Temperature array

	//Generate temperatures
	for(int i = 0; i < temperature.length; i++)
	    for(int j = 0; j < temperature[i].length; j++)
		temperature[i][j] = (float)(45.0*Math.random() - 10.0);

	//Calculate the average per location
	for(int i =  0; i < temperature.length ; i++)
	    {
		float average = 0.0f;       // Place to store the average

		for(int j = 0; j < temperature[0].length; j++)
		    average += temperature[i][j];
		// Coutput the average temperature for the current location
		System.out.println("Average temperature at location "
				   + (i + 1) + " = " + average/(float)temperature[i].length);
	    }
    }
}
