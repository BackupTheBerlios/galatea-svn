using System;

namespace TypesLibrary
{
	/// <remarks>
	/// Sample object to demonstrate the use of .NET Remoting.
	/// </remarks>
	public class SampleObject : MarshalByRefObject
	{
		/// <summary>
		/// Constructor
		/// </summary> 
		public SampleObject()
		{
		}

		/// <summary>
		/// Return a hello message
		/// </summary>
		/// <returns>Hello world message</returns>
		public string HelloWorld()
		{
			return "Hello World!";
		}
		public string Echo(string msg)
		{
			System.Console.WriteLine("User says :" + msg);
			return " user said : " + msg;
		}
	}
}