using System;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using TypesLibrary;

namespace Server
{
	/// <remarks>
	/// Sample server to demonstrate the use of .NET Remoting.
	/// </remarks>
	public class SampleServer
	{
		public static int Main()
		{
			// Create an instance of a channel
			TcpChannel channel = new TcpChannel(8080);
			ChannelServices.RegisterChannel(channel);

			// Register as an available service with the name HelloWorld
			RemotingConfiguration.RegisterWellKnownServiceType(
				typeof (ServingObject),
				"HelloWorld",
				WellKnownObjectMode.SingleCall);

			Console.WriteLine("Press the enter key to exit...");
			Console.ReadLine();
			return 0;
		}

	}
}
