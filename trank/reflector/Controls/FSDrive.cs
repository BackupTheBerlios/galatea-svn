namespace MP
{
	namespace Controls
	{
		namespace Utils
		{

			#region FSDrive class

			/// <summary>
			/// represent hard drive 
			/// </summary>
			public class FSDrive
			{
				#region enum DriveType declaration

				/// <summary>
				/// Types of disk drives available in the system
				/// </summary>
				public enum DriveType
				{
					/// <summary>
					/// default value
					/// </summary>
					Invalid ,
					/// <summary>
					/// removable disk 
					/// </summary>
					Removable ,
					/// <summary>
					/// Local disk
					/// </summary>
					LocalDisk ,
					/// <summary>
					/// Network disk
					/// </summary>
					Network ,
					/// <summary>
					/// CD drive
					/// </summary>
					CD
				}

				#endregion

				#region Type

				/// <summary>
				/// holds the type of the drive
				/// </summary>
				private DriveType mType;

				/// <summary>
				/// On of the Drive Types
				/// </summary>
				public DriveType Type
				{
					get { return mType; }
					set { mType = value; }
				}

				#endregion

				#region Constructors

				/// <summary>
				/// Default constructor
				/// </summary>
				public FSDrive()
				{
					mType = DriveType.Invalid;
				}

				#endregion
			}

			#endregion

			#region FSDrivePresentationInfo class 

			/// <summary>
			/// Holds graphical presentation for Data drive
			/// </summary>
			public class FSDrivePresentationInfo : FSDrive
			{
				#region ImageIndex

				/// <summary>
				/// the index of item icon
				/// </summary>
				public int ImageIndex
				{
					get { return mImageIndex; }
					set { mImageIndex = value; }
				}

				/// <summary>
				/// the index of item icon
				/// </summary>
				private int mImageIndex = 0;

				#endregion

				#region Selected Index

				/// <summary>
				/// index of selected item Icon
				/// </summary>
				private int mSelectedIndex = 0;

				/// <summary>
				/// index of selected item Icon
				/// </summary>
				public int SelectedIndex
				{
					get { return mSelectedIndex; }
					set { mSelectedIndex = value; }
				}

				#endregion

				#region Name

				private string mName = "";

				/// <summary>
				/// drive name
				/// </summary>
				public string Name
				{
					get { return mName; }
					set { mName = value; }
				}

				#endregion
			}

			#endregion
		}
	}
}
