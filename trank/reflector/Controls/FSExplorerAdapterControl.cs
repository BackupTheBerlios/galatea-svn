using System.ComponentModel;

namespace MP
{
	namespace Controls
	{
		#region FSExplorerAdapterControl class

		/// <summary>
		/// Provide logic layer behind file system explorer control
		/// </summary>
		public class FSExplorerAdapterControl : FSExplorerControl
		{
			private IContainer components = null;

			#region Constructors

			/// <summary>
			/// default constructor
			/// </summary>
			public FSExplorerAdapterControl()
			{
				// This call is required by the Windows Form Designer.
				InitializeComponent();

				// TODO: Add any initialization after the InitializeComponent call
			}
			#endregion
			
			#region Destructor

			/// <summary>
			/// Clean up any resources being used.
			/// </summary>
			protected override void Dispose( bool disposing )
			{
				if ( disposing )
				{
					if ( components != null )
					{
						components.Dispose();
					}
				}
				base.Dispose( disposing );
			}

			
			#endregion

			#region Designer generated code

			/// <summary>
			/// Required method for Designer support - do not modify
			/// the contents of this method with the code editor.
			/// </summary>
			private void InitializeComponent()
			{
				components = new System.ComponentModel.Container();
			}

			#endregion
			
			#region Public methods
			
			
			#endregion

			#region Private methods
			/// <summary>
			/// initializes the control 
			/// </summary>
			private void InitFS()
			{


			}
			#endregion

		}

	
		#endregion
	}
}