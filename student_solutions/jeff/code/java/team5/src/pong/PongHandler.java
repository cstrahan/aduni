public class PongHandler
{
    /**
       Called by Gateway when incoming pong received
    */
    public static void receivePong(Pong pong)
    {
	routePong(pong);
	handlePong(pong);
    }

    public static void routePong(Pong pong)
    {
	pong.decTtl();
	pong.incHops();

	int ttl = pong.getTtl();
	if (ttl > 0) Gateway.deliver(pong);
    }

    public static void handlePong(Pong pong)
    {
	// DO STUFF - like, update total files/kilobytes shared info
    }
}

