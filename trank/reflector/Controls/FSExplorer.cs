using System.ComponentModel;
using System.Windows.Forms;

namespace MP.Controls
{
	/// <summary>
	/// Summary description for UserControl1.
	/// </summary>
	public class FSExplorerControl : UserControl
	{
		private Panel mMainPanel;
		private TreeView treeView1;
		private Splitter mSplitter;
		private ListView mFilesListView;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private Container components = null;

		public FSExplorerControl()
		{
			// This call is required by the Windows.Forms Form Designer.
			InitializeComponent();

			// TODO: Add any initialization after the InitComponent call

		}

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

		#region Component Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.mMainPanel = new System.Windows.Forms.Panel();
			this.mFilesListView = new System.Windows.Forms.ListView();
			this.mSplitter = new System.Windows.Forms.Splitter();
			this.treeView1 = new System.Windows.Forms.TreeView();
			this.mMainPanel.SuspendLayout();
			this.SuspendLayout();
			// 
			// mMainPanel
			// 
			this.mMainPanel.Controls.Add(this.mFilesListView);
			this.mMainPanel.Controls.Add(this.mSplitter);
			this.mMainPanel.Controls.Add(this.treeView1);
			this.mMainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mMainPanel.Location = new System.Drawing.Point(0, 0);
			this.mMainPanel.Name = "mMainPanel";
			this.mMainPanel.Size = new System.Drawing.Size(656, 744);
			this.mMainPanel.TabIndex = 0;
			// 
			// mFilesListView
			// 
			this.mFilesListView.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mFilesListView.Location = new System.Drawing.Point(0, 100);
			this.mFilesListView.Name = "mFilesListView";
			this.mFilesListView.Size = new System.Drawing.Size(656, 644);
			this.mFilesListView.TabIndex = 2;
			// 
			// mSplitter
			// 
			this.mSplitter.Dock = System.Windows.Forms.DockStyle.Top;
			this.mSplitter.Location = new System.Drawing.Point(0, 97);
			this.mSplitter.Name = "mSplitter";
			this.mSplitter.Size = new System.Drawing.Size(656, 3);
			this.mSplitter.TabIndex = 1;
			this.mSplitter.TabStop = false;
			// 
			// treeView1
			// 
			this.treeView1.Dock = System.Windows.Forms.DockStyle.Top;
			this.treeView1.ImageIndex = -1;
			this.treeView1.Location = new System.Drawing.Point(0, 0);
			this.treeView1.Name = "treeView1";
			this.treeView1.SelectedImageIndex = -1;
			this.treeView1.Size = new System.Drawing.Size(656, 97);
			this.treeView1.TabIndex = 0;
			// 
			// FSExplorerControl
			// 
			this.Controls.Add(this.mMainPanel);
			this.Name = "FSExplorerControl";
			this.Size = new System.Drawing.Size(656, 744);
			this.mMainPanel.ResumeLayout(false);
			this.ResumeLayout(false);

		}

		#endregion
	}
}