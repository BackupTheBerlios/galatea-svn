using System;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using TypesLibrary;

namespace Server
{
	namespace Hosting
	{
		#region ThreadedHost
		/// <summary>
		/// Summary description for ThreadedHost.
		/// </summary>
		public class ThreadedHost
		{
			#region Public
		
			#region Methods

			#region Constructors
			public ThreadedHost(int port)
			{
				this.mPort = port;
				this.mChannel = new TcpChannel(mPort);
				ChannelServices.RegisterChannel(mChannel);
				
				RemotingConfiguration.RegisterWellKnownServiceType(typeof(ServingObject),String.Empty,
					WellKnownObjectMode.SingleCall);


			}
			#endregion // Constructors

			#region Send
			/// <summary>
			/// 
			/// </summary>
			public void Send()
			{
				
			}
			#endregion 
			
			#endregion // Methods

			#region Properties

			#region Port
			/// <summary>
			///  the Port number
			/// </summary>
			public int Port
			{
				get
				{
					return mPort;
				}
			}
			#endregion // Port

			#endregion // Properties

			#endregion // Public
			
			#region Private 

			#region Fields

			#region mPort
			/// <summary>
			///  the Port number
			/// </summary>
			int mPort = 0;
			#endregion // mPort
			#region  mChannel
			TcpChannel mChannel;
			#endregion 

			#endregion // Fields
			
			#endregion // Private 

			
		}
		#endregion // ThreadedHost
	}
}
//
//// Create an instance of a channel
//TcpChannel channel = new TcpChannel(8080);
//ChannelServices.RegisterChannel(channel);
//
//			// Register as an available service with the name HelloWorld
//RemotingConfiguration.RegisterWellKnownServiceType(
//typeof (SampleObject),
//"HelloWorld",
//WellKnownObjectMode.SingleCall);
//
//Console.WriteLine("Press the enter key to exit...");
//Console.ReadLine();