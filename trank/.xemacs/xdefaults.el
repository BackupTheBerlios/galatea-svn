!-*- Mode: xrdb -*-
Emacs*menubar.background: burlywood
Emacs*menubar.foreground: black
Emacs*popup.background: burlywood
Emacs*popup.foreground: black
!!
!! for XEmacs only
!!
! "check box" or "radio button" color
Emacs*menubar.selectColor: gray90
Emacs*popup.selectColor: gray90
Emacs*menubar.highlightForeground: black
Emacs*popup.highlightForeground: gray90
Emacs*popup.titleForeground: black


! Athena dialog boxes are sometimes built with the Xaw3d
! variant of the Athena toolkit.
! XEmacs being nice to 8bit displays, it defaults to:
Emacs*dialog*Command*beNiceToColormap:	true
! If you are shocked by the ugliness of the 3d rendition,
! you may want to set (even on 8bit displays) the above to false.

! Xlw Scrollbar colors
Emacs*XlwScrollBar.Foreground:	wheat
Emacs*XlwScrollBar.Background:  burlywood
Emacs*XmScrollBar.Foreground:   wheat
Emacs*XmScrollBar.Background:	burlywood

!
! The Lucid Scrollbar supports two added resources, SliderStyle is either
! "plain" (default) or "dimple".  Dimple puts a small dimple in the middle
! of the slider that depresses when the slider is clicked on.  ArrowPosition is
! either "opposite" (default) or "same".  Opposite puts the arrows at opposite
! of the scrollbar, same puts both arrows at the same end, like the Amiga.
!
 Emacs*XlwScrollBar.SliderStyle:    dimple
 Emacs*XlwScrollBar.ArrowPosition:  opposite

! This is for buttons in the menubar.  
! Yellow would be better, but that would map to white on monochrome.
Emacs*menubar.buttonForeground:		Blue
Emacs*XlwMenu.selectColor:		ForestGreen
Emacs*XmToggleButton.selectColor:	ForestGreen

! Specify the colors of popup menus.
Emacs*popup*Foreground:			Black
Emacs*popup*Background:			burlywood

! Specify the colors of the various sub-widgets of the dialog boxes.
Emacs*dialog*Foreground:		Black
Emacs*dialog*Background:		burlywood
! The following three are for Motif dialog boxes ...
Emacs*dialog*XmTextField*Background:	wheat
Emacs*dialog*XmText*Background:		wheat
Emacs*dialog*XmList*Background:		wheat
! While this one is for Athena dialog boxes.
Emacs*dialog*Command*Background:	wheat



Emacs*topToolBarShadowColor:		tan
Emacs*bottomToolBarShadowColor:		dimgrey
Emacs*backgroundToolBarColor:		burlywood
Emacs*toolBarShadowThickness:		2
