using System.Management;

namespace MP
{
	namespace Controls
	{
		namespace Utils
		{
			#region FSAdapter class

			/// <summary>
			/// Incapsulates operations On file System
			/// </summary>
			public class FSAdapter
			{
				#region Constructors

				/// <summary>
				/// default constructor
				/// </summary>
				public FSAdapter()
				{
					//
					// TODO: Add constructor logic here
					//
				}
				#endregion

				#region Public Methods

				#region getDrives
				/// <summary>
				/// Query for windows drives 
				/// </summary>
				/// <returns>Collection of available drives</returns>
				public FSDrivePresentationInfo[] getDrives()
				{
					//get drive collection
					ManagementObjectSearcher query = new ManagementObjectSearcher("SELECT * From Win32_LogicalDisk ");
					ManagementObjectCollection queryCollection = query.Get();
					FSDrivePresentationInfo[]  drives = new FSDrivePresentationInfo[queryCollection.Count];
					int i = 0;
					const int Removable = 2;
					const int LocalDisk = 3;
					const int Network = 4;
					const int CD = 5;

					foreach ( ManagementObject mo in queryCollection)
					{
						drives[i] = new FSDrivePresentationInfo();
						drives[i].Name = mo["Name"].ToString() + "\\";
				
						switch (int.Parse( mo["DriveType"].ToString()))
						{
							case Removable:			//removable drives
								drives[i].ImageIndex = 5;
								drives[i].SelectedIndex = 5;
								break;
							case LocalDisk:			//Local drives
								drives[i].ImageIndex = 6;
								drives[i].SelectedIndex = 6;
								break;
							case CD:				//CD rom drives
								drives[i].ImageIndex = 7;
								drives[i].SelectedIndex = 7;
								break;
							case Network:			//Network drives
								drives[i].ImageIndex = 8;
								drives[i].SelectedIndex = 8;
								break;
							default:				//defalut to folder
								drives[i].ImageIndex = 2;
								drives[i].SelectedIndex = 3;
								break;
						}

						// don't  forget to increment the counter
						++i;
					}
					return drives;
				}
				#endregion
		
				#region getFullPath
				/// <summary>
				/// access to full path of the node
				/// </summary>
				/// <param name="stringPath"> string eit path</param>
				/// <returns>reformatted path </returns>
				public string getFullPath(string stringPath)
				{
					//Get Full path
					string stringParse = "";
					//remove My Computer from path.
					stringParse = stringPath.Replace("My Computer\\", "");

					return stringParse;
				}
		
				#endregion
				
				#region GetPathName
				

				/// <summary>
				/// access to file name
				/// </summary>
				/// <param name="stringPath">full path </param>
				/// <returns>jast the file name</returns>
				public string GetPathName(string stringPath)
				{
					//Get Name of folder
					string[] stringSplit = stringPath.Split('\\');
					int _maxIndex = stringSplit.Length;
					return stringSplit[_maxIndex-1];
				}
				#endregion

				#endregion
			}
		
		
			#endregion
		}

	}
}	 

