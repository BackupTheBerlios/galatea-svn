using System;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using TypesLibrary;

namespace Client
{
	/// <remarks>
	/// </remarks>
	public class SampleClient
	{
		
		public static int Main()
		{
			// Create a channel for communicating w/ the remote object
			// Notice no port is specified on the client
			TcpChannel chan = new TcpChannel();
			ChannelServices.RegisterChannel(chan);

			// Create an instance of the remote object
			ServingObject obj = (ServingObject) Activator.GetObject( 
				typeof(ServingObject),
				"tcp://localhost:8080/HelloWorld" );

			// Use the object
			if( obj.Equals(null) )
			{
				Console.WriteLine("Error: unable to locate server");
			}
			else
			{
				Console.WriteLine(obj.HelloWorld());
				obj.Echo( "Second time hello world" );
			}
			return 0;
		} 
	}
}