public class PushHandler
{
    /** Push is one case where Gateway must already determine whether the
	push is merely to be routed on or whether it is intended for us -
	must call routePush or pushFile accordingly.
    */
    public static void routePush(Push push)
    {
	push.decTtl();
	push.incHops();

	int ttl = push.getTtl();
	if (ttl > 0) Gateway.deliver(push);
    }

    public static void pushFile(Push push)
    {
	// connect and upload...?
    }

    /**
       Presumably initiated by user
    */
    public static void initiatePush(QueryHitObject qho, int fileIndex)
    {
	byte[] messageId = new byte[16];
	messageId = qho.getMessageId();
	int payloadDescriptor = 0x40;
	int ttl = 10; // ???
	int hops = 0;
	int payloadLength = 26;
	
	byte[] serventID = new byte[16];
	serventID = qho.getServentID();
	byte[] ipAddress = new byte[4];
	ipAddress = qho.getIpAddress();
	int port = qho.getPort();

	Push push = new Push(messageId, payloadDescriptor, ttl, hops, payloadLength
			     serventID, fileIndex, ipAddress, port);
	push.setSocketId(qho.getSocketId());
	Gateway.deliver(push);
    }
}







