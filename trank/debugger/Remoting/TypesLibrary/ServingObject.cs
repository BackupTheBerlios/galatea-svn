using System;

namespace TypesLibrary
{
	public class ServingObject : MarshalByRefObject
	{
		public ServingObject()
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