using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace reflector
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class ReflectorForm : System.Windows.Forms.Form
	{
		#region Declarations
		private System.Windows.Forms.Panel mFileBrowserPanel;
		private System.Windows.Forms.StatusBar mStatusBar;
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem mFileMenuItem;
		private System.Windows.Forms.MenuItem mLoadMenuItem;
		private System.Windows.Forms.TreeView mFileSystemTreeView;
		private System.Windows.Forms.Splitter splitter1;
		private System.Windows.Forms.ListView listView1;
		private System.Windows.Forms.Panel mMainPanel;
		private System.Windows.Forms.Splitter splitter2;
		private System.Windows.Forms.Panel mDataPanel;
		private System.Windows.Forms.Panel mSearchPanel;
		private System.Windows.Forms.ListView mDataListView;
		private System.Windows.Forms.Button mSearchButton;
		private System.Windows.Forms.TextBox mSearchTextBox;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.Panel mFileSysAndComboPanel;
		private System.Windows.Forms.ListBox mDriveListBox;
		#endregion
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public ReflectorForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.mFileBrowserPanel = new System.Windows.Forms.Panel();
			this.mFileSysAndComboPanel = new System.Windows.Forms.Panel();
			this.mDriveListBox = new System.Windows.Forms.ListBox();
			this.mFileSystemTreeView = new System.Windows.Forms.TreeView();
			this.listView1 = new System.Windows.Forms.ListView();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.mStatusBar = new System.Windows.Forms.StatusBar();
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.mFileMenuItem = new System.Windows.Forms.MenuItem();
			this.mLoadMenuItem = new System.Windows.Forms.MenuItem();
			this.mMainPanel = new System.Windows.Forms.Panel();
			this.splitter2 = new System.Windows.Forms.Splitter();
			this.mDataPanel = new System.Windows.Forms.Panel();
			this.mDataListView = new System.Windows.Forms.ListView();
			this.mSearchPanel = new System.Windows.Forms.Panel();
			this.panel1 = new System.Windows.Forms.Panel();
			this.mSearchTextBox = new System.Windows.Forms.TextBox();
			this.mSearchButton = new System.Windows.Forms.Button();
			this.mFileBrowserPanel.SuspendLayout();
			this.mFileSysAndComboPanel.SuspendLayout();
			this.mMainPanel.SuspendLayout();
			this.mDataPanel.SuspendLayout();
			this.mSearchPanel.SuspendLayout();
			this.panel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// mFileBrowserPanel
			// 
			this.mFileBrowserPanel.Controls.Add(this.mFileSysAndComboPanel);
			this.mFileBrowserPanel.Controls.Add(this.listView1);
			this.mFileBrowserPanel.Controls.Add(this.splitter1);
			this.mFileBrowserPanel.Dock = System.Windows.Forms.DockStyle.Left;
			this.mFileBrowserPanel.Location = new System.Drawing.Point(0, 0);
			this.mFileBrowserPanel.Name = "mFileBrowserPanel";
			this.mFileBrowserPanel.Size = new System.Drawing.Size(200, 536);
			this.mFileBrowserPanel.TabIndex = 0;
			// 
			// mFileSysAndComboPanel
			// 
			this.mFileSysAndComboPanel.Controls.Add(this.mDriveListBox);
			this.mFileSysAndComboPanel.Controls.Add(this.mFileSystemTreeView);
			this.mFileSysAndComboPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mFileSysAndComboPanel.Location = new System.Drawing.Point(0, 3);
			this.mFileSysAndComboPanel.Name = "mFileSysAndComboPanel";
			this.mFileSysAndComboPanel.Size = new System.Drawing.Size(200, 333);
			this.mFileSysAndComboPanel.TabIndex = 4;
			// 
			// mDriveListBox
			// 
			this.mDriveListBox.Dock = System.Windows.Forms.DockStyle.Top;
			this.mDriveListBox.HorizontalScrollbar = true;
			this.mDriveListBox.ItemHeight = 16;
			this.mDriveListBox.Items.AddRange(new object[] {
															   "C:",
															   "D:"});
			this.mDriveListBox.Location = new System.Drawing.Point(0, 0);
			this.mDriveListBox.Name = "mDriveListBox";
			this.mDriveListBox.Size = new System.Drawing.Size(200, 20);
			this.mDriveListBox.Sorted = true;
			this.mDriveListBox.TabIndex = 3;
			this.mDriveListBox.SelectedValueChanged += new System.EventHandler(this.mDriveListBoxSelectedValueChanged);
			this.mDriveListBox.SelectedIndexChanged += new System.EventHandler(this.mDriveComboBoxOnSelectedIndexChanged);
			// 
			// mFileSystemTreeView
			// 
			this.mFileSystemTreeView.AccessibleRole = System.Windows.Forms.AccessibleRole.None;
			this.mFileSystemTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mFileSystemTreeView.ImageIndex = -1;
			this.mFileSystemTreeView.Location = new System.Drawing.Point(0, 0);
			this.mFileSystemTreeView.Name = "mFileSystemTreeView";
			this.mFileSystemTreeView.SelectedImageIndex = -1;
			this.mFileSystemTreeView.Size = new System.Drawing.Size(200, 333);
			this.mFileSystemTreeView.TabIndex = 0;
			// 
			// listView1
			// 
			this.listView1.Dock = System.Windows.Forms.DockStyle.Bottom;
			this.listView1.Location = new System.Drawing.Point(0, 336);
			this.listView1.Name = "listView1";
			this.listView1.Size = new System.Drawing.Size(200, 200);
			this.listView1.TabIndex = 2;
			this.listView1.SelectedIndexChanged += new System.EventHandler(this.listView1_SelectedIndexChanged);
			// 
			// splitter1
			// 
			this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
			this.splitter1.Location = new System.Drawing.Point(0, 0);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(200, 3);
			this.splitter1.TabIndex = 1;
			this.splitter1.TabStop = false;
			// 
			// mStatusBar
			// 
			this.mStatusBar.Location = new System.Drawing.Point(0, 536);
			this.mStatusBar.Name = "mStatusBar";
			this.mStatusBar.Size = new System.Drawing.Size(712, 22);
			this.mStatusBar.TabIndex = 2;
			// 
			// mainMenu1
			// 
			this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.mFileMenuItem});
			// 
			// mFileMenuItem
			// 
			this.mFileMenuItem.Index = 0;
			this.mFileMenuItem.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																						  this.mLoadMenuItem});
			this.mFileMenuItem.Text = "File";
			// 
			// mLoadMenuItem
			// 
			this.mLoadMenuItem.Index = 0;
			this.mLoadMenuItem.Text = "Load";
			this.mLoadMenuItem.Click += new System.EventHandler(this.mFileLoadOnCick);
			// 
			// mMainPanel
			// 
			this.mMainPanel.Controls.Add(this.splitter2);
			this.mMainPanel.Controls.Add(this.mDataPanel);
			this.mMainPanel.Controls.Add(this.mFileBrowserPanel);
			this.mMainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mMainPanel.Location = new System.Drawing.Point(0, 0);
			this.mMainPanel.Name = "mMainPanel";
			this.mMainPanel.Size = new System.Drawing.Size(712, 536);
			this.mMainPanel.TabIndex = 3;
			// 
			// splitter2
			// 
			this.splitter2.Location = new System.Drawing.Point(200, 0);
			this.splitter2.Name = "splitter2";
			this.splitter2.Size = new System.Drawing.Size(3, 536);
			this.splitter2.TabIndex = 2;
			this.splitter2.TabStop = false;
			// 
			// mDataPanel
			// 
			this.mDataPanel.Controls.Add(this.mDataListView);
			this.mDataPanel.Controls.Add(this.mSearchPanel);
			this.mDataPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mDataPanel.Location = new System.Drawing.Point(200, 0);
			this.mDataPanel.Name = "mDataPanel";
			this.mDataPanel.Size = new System.Drawing.Size(512, 536);
			this.mDataPanel.TabIndex = 1;
			// 
			// mDataListView
			// 
			this.mDataListView.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mDataListView.Location = new System.Drawing.Point(0, 24);
			this.mDataListView.Name = "mDataListView";
			this.mDataListView.Size = new System.Drawing.Size(512, 512);
			this.mDataListView.TabIndex = 1;
			// 
			// mSearchPanel
			// 
			this.mSearchPanel.Controls.Add(this.panel1);
			this.mSearchPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.mSearchPanel.Location = new System.Drawing.Point(0, 0);
			this.mSearchPanel.Name = "mSearchPanel";
			this.mSearchPanel.Size = new System.Drawing.Size(512, 24);
			this.mSearchPanel.TabIndex = 0;
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this.mSearchTextBox);
			this.panel1.Controls.Add(this.mSearchButton);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.panel1.Location = new System.Drawing.Point(0, 0);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(512, 24);
			this.panel1.TabIndex = 2;
			// 
			// mSearchTextBox
			// 
			this.mSearchTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mSearchTextBox.Location = new System.Drawing.Point(75, 0);
			this.mSearchTextBox.Name = "mSearchTextBox";
			this.mSearchTextBox.Size = new System.Drawing.Size(437, 22);
			this.mSearchTextBox.TabIndex = 1;
			this.mSearchTextBox.Text = "textBox1";
			// 
			// mSearchButton
			// 
			this.mSearchButton.Dock = System.Windows.Forms.DockStyle.Left;
			this.mSearchButton.Location = new System.Drawing.Point(0, 0);
			this.mSearchButton.Name = "mSearchButton";
			this.mSearchButton.Size = new System.Drawing.Size(75, 24);
			this.mSearchButton.TabIndex = 0;
			this.mSearchButton.Text = "Search";
			this.mSearchButton.Click += new System.EventHandler(this.mSearchButtonOnClick);
			// 
			// ReflectorForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
			this.ClientSize = new System.Drawing.Size(712, 558);
			this.Controls.Add(this.mMainPanel);
			this.Controls.Add(this.mStatusBar);
			this.Menu = this.mainMenu1;
			this.Name = "ReflectorForm";
			this.Text = "My Reflector";
			this.mFileBrowserPanel.ResumeLayout(false);
			this.mFileSysAndComboPanel.ResumeLayout(false);
			this.mMainPanel.ResumeLayout(false);
			this.mDataPanel.ResumeLayout(false);
			this.mSearchPanel.ResumeLayout(false);
			this.panel1.ResumeLayout(false);
			this.ResumeLayout(false);

		}
		#endregion
		
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new ReflectorForm());
		}

		#region Callbacks
		private void mFileLoadOnCick(object sender, System.EventArgs e)
		{
			MessageBox.Show( "Sorry but I am still not implemented" );
		}

		private void listView1_SelectedIndexChanged(object sender, System.EventArgs e)
		{
		
		}

		private void mSearchButtonOnClick(object sender, System.EventArgs e)
		{
			MessageBox.Show( "Sorry but I am still not implemented" );
		}

		private void mDriveComboBoxOnSelectedIndexChanged(object sender, System.EventArgs e)
		{
			MessageBox.Show( "Sorry but I am still not implemented" );
		}

		private void mDriveListBoxSelectedValueChanged(object sender, System.EventArgs e)
		{
			MessageBox.Show( "Sorry but I am still not implemented" );
		}
		#endregion
	}
}
