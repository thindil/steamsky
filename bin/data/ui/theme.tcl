package require Tk 8.6.0

namespace eval ttk::theme::steamsky {

   variable version 0.1
   package provide ttk::theme::steamsky $version

   #
   # Colors
   #

   variable colors
   array set colors {
      -bg             "#1a130c"
      -fg             "#eee8aa"

      -disabledbg     "#4d4d4d"
      -disabledfg     "#7f8c8d"

      -selectbg       "#800000"
      -selectfg       "#eee8aa"

      -window         "#1a130c"
      -focuscolor     "#ffdf00"
      -checklight     "#ffdf00"
   }

   #
   # Fonts
   #

   # Font used in drawing the game map
   font create MapFont -family {Hack NF} -size 16
   # Font used to show all text in game
   font create InterfaceFont -family {Amarante} -size 14
   # Font used to show the game help
   font create HelpFont -family {Roboto} -size 14
   # Font used as icons for buttons/labels
   font create InterfaceIcons -family {Font Awesome 5 Free Solid} -size 14

   #
   # Load theme related images
   #

   proc LoadImages {imgdir} {
       variable I
       foreach file [glob -directory $imgdir *.svg] {
           set img [file tail [file rootname $file]]
           set I($img) [image create photo -file $file]
           set tempimg [image create photo]
           $tempimg copy $I($img)
           $I($img) blank
           $I($img) copy $tempimg -shrink -subsample [expr [image width $tempimg] / 22]
       }
   }

   LoadImages [file join [file dirname [info script]] images]

   #
   # Create theme
   #

   ttk::style theme create steamsky -parent default -settings {
      ttk::style configure . \
         -background $colors(-bg) \
         -foreground $colors(-fg) \
         -troughcolor $colors(-bg) \
         -selectbackground $colors(-selectbg) \
         -selectforeground $colors(-selectfg) \
         -fieldbackground $colors(-window) \
         -font InterfaceFont \
         -borderwidth 1 \
         -focuscolor $colors(-focuscolor)

      ttk::style map . -foreground [list disabled $colors(-disabledfg)]

      #
      # Layouts:
      #

      # Vertical scrollbars
      ttk::style layout Vertical.TScrollbar {
         Vertical.Scrollbar.trough -sticky ns -children {
            Vertical.Scrollbar.thumb -expand true
         }
      }

      # Horizontal scrollbars
      ttk::style layout Horizontal.TScrollbar {
         Horizontal.Scrollbar.trough -sticky ew -children {
            Horizontal.Scrollbar.thumb -expand true
         }
      }

      #
      # Elements:
      #

      ttk::style element create Checkbutton.indicator image [list $I(checkbox-unchecked) \
         selected            $I(checkbox-checked) \
         ] -width 22 -sticky w

      #
      # Settings:
      #

      # Button setting
      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised
      # Menu button setting
      ttk::style configure TMenubutton -padding {8 4 4 4}
      # Flat button setting
      ttk::style configure Toolbutton -padding {6 2} -anchor center
      # Radio button setting
      ttk::style configure TRadiobutton -padding 4
      # Separator setting
      ttk::style configure TSeparator -background $colors(-bg)
      # Not needed state header button (ship info in right top corner)
      ttk::style configure Header.Toolbutton -font InterfaceIcons
      # Alarm state header button (ship info in right top corner)
      ttk::style configure Headerred.Toolbutton -font InterfaceIcons -foreground red
      # Normal state header button (ship info in right top corner)
      ttk::style configure Headergreen.Toolbutton -font InterfaceIcons -foreground green
      # Labels with red text
      ttk::style configure Headerred.TLabel -foreground red
      # Labels with green text
      ttk::style configure Headergreen.TLabel -foreground green
      # Progress bar setting
      ttk::style configure TProgressbar -background red
      # Entry setting
      ttk::style configure TEntry -insertcolor $colors(-fg)

      # Paned window
      ttk::style map TPanedwindow -background [list hover $colors(-checklight)]

      # Combo box setting
      ttk::style map TCombobox -selectbackground [list \
         !focus         $colors(-window) \
         {readonly hover} $colors(-bg) \
         {readonly focus} $colors(-selectbg) \
         ]

      ttk::style map TCombobox -selectforeground [list \
         !focus $colors(-fg) \
         {readonly hover} $colors(-fg) \
         {readonly focus} $colors(-selectfg) \
         ]

      # Tree view (like cargo, trading) setting
      ttk::style configure Treeview -background $colors(-bg)
      ttk::style configure Treeview.Item -padding {2 0 0 0}
      ttk::style map Treeview \
         -background [list selected $colors(-selectbg)] \
         -foreground [list selected $colors(-selectfg)]

      # Check button setting
      ttk::style configure TCheckbutton -padding 4 -indicatorcolor $colors(-bg)
      ttk::style map TCheckbutton -indicatorcolor \
         [list selected $colors(-selectfg)]

      # Texts views (like messages, modules info, etc)
      tk_setPalette background [ttk::style lookup . -background] \
         foreground [ttk::style lookup . -foreground] \
         highlightColor [ttk::style lookup . -focuscolor] \
         selectBackground [ttk::style lookup . -selectbackground] \
         selectForeground [ttk::style lookup . -selectforeground] \
         activeBackground [ttk::style lookup . -selectbackground] \
         activeForeground [ttk::style lookup . -selectforeground] \
         insertbackground [ttk::style lookup . -foreground]
      option add *font [ttk::style lookup . -font]
   }

}
