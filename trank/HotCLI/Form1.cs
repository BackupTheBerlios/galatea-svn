using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using SampleNotifyIconApplication;

namespace systemTrayApplication
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class HOTCLIForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.NotifyIcon mNotifyIcon;
		private System.Windows.Forms.ContextMenu mNotifyIconContextMenu;
		private System.Windows.Forms.MenuItem mTrayShowMenuItem;
		private System.Windows.Forms.MenuItem mTrayExitMenuItem;
		private System.ComponentModel.IContainer components;

		public HOTCLIForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			SetHotKey(Keys.J, true,true,false,false);
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
			this.components = new System.ComponentModel.Container();
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(HOTCLIForm));
			this.mNotifyIcon = new System.Windows.Forms.NotifyIcon(this.components);
			this.mNotifyIconContextMenu = new System.Windows.Forms.ContextMenu();
			this.mTrayShowMenuItem = new System.Windows.Forms.MenuItem();
			this.mTrayExitMenuItem = new System.Windows.Forms.MenuItem();
			// 
			// mNotifyIcon
			// 
			this.mNotifyIcon.ContextMenu = this.mNotifyIconContextMenu;
			this.mNotifyIcon.Icon = ((System.Drawing.Icon)(resources.GetObject("mNotifyIcon.Icon")));
			this.mNotifyIcon.Text = "TheNotifyIconText";
			this.mNotifyIcon.Visible = true;
			this.mNotifyIcon.DoubleClick += new System.EventHandler(this.mNotifyIconOnDoubleClickCallback);
			this.mNotifyIcon.Click += new System.EventHandler(this.mNotifyIconOnCLickCallBack);
			// 
			// mNotifyIconContextMenu
			// 
			this.mNotifyIconContextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																								   this.mTrayShowMenuItem,
																								   this.mTrayExitMenuItem});
			// 
			// mTrayShowMenuItem
			// 
			this.mTrayShowMenuItem.Index = 0;
			this.mTrayShowMenuItem.Text = "Show";
			this.mTrayShowMenuItem.Click += new System.EventHandler(this.menuItem1_Click);
			// 
			// mTrayExitMenuItem
			// 
			this.mTrayExitMenuItem.Index = 1;
			this.mTrayExitMenuItem.Text = "Exit";
			this.mTrayExitMenuItem.Click += new System.EventHandler(this.menu_App_Exit);
			// 
			// HOTCLIForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
			this.ClientSize = new System.Drawing.Size(292, 260);
			this.Name = "HOTCLIForm";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Form1";
			this.WindowState = System.Windows.Forms.FormWindowState.Minimized;

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new HOTCLIForm());
		}
		#region Michael
		public void HideApp()
		{
			this.WindowState = FormWindowState.Minimized;
			Hide();
		}

		public void ShowApp()
		{
			Show();
			this.WindowState = FormWindowState.Normal;
		}
		protected override void OnClosing(CancelEventArgs e)
		{
			// method overidden so the form can be minimized, instead of closed

			e.Cancel = true;

			// let's minimize the form, and hide it
			this.WindowState = FormWindowState.Minimized;
			Hide();
		}
		private void menu_App_Exit(object sender, System.EventArgs e)
		{
			NativeWIN32.UnregisterHotKey(Handle, 100);

			// hide icon from the systray
			mNotifyIcon.Visible = false; 


			Application.Exit();
		}
		public void SetHotKey(Keys c, bool bCtrl, bool bShift, bool bAlt, bool bWindows)
		{

			// update hotkey
			NativeWIN32.KeyModifiers modifiers = NativeWIN32.KeyModifiers.None;
			if (bCtrl)
				modifiers |= NativeWIN32.KeyModifiers.Control;
			if (bShift)
				modifiers |= NativeWIN32.KeyModifiers.Shift;
			if (bAlt)
				modifiers |= NativeWIN32.KeyModifiers.Alt;
			if (bWindows)
				modifiers |= NativeWIN32.KeyModifiers.Windows;

			NativeWIN32.RegisterHotKey(Handle, 100, modifiers, c); //Keys.J);
		}
		protected override void WndProc( ref Message m )
		{	
			const int WM_HOTKEY = 0x0312; 	
	
			switch(m.Msg)	
			{	
				case WM_HOTKEY:	
				
					MessageBox.Show("Hotkey pressed");		

					//ProcessHotkey();

					break;	
			} 	
			base.WndProc(ref m );
		}
		#endregion

		private void mNotifyIconOnDoubleClickCallback(object sender, System.EventArgs e)
		{
			if(this.WindowState == FormWindowState.Minimized)
			{
				ShowApp();
			}
			else
			{
				HideApp();
			}
		}

		private void mNotifyIconOnCLickCallBack(object sender, System.EventArgs e)
		{
			//this.mNotifyIconContextMenu.Show(,e. );
		}

		private void menuItem1_Click(object sender, System.EventArgs e)
		{
		
		}
	}
}
