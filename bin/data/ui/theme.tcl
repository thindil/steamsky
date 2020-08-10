package require Tk 8.6.0

namespace eval ttk::theme::steamsky {

   variable version 0.2
   package provide ttk::theme::steamsky $version

   #
   # Colors
   #

   variable colors
   array set colors {
      -bg             "#1a130c"
      -fg             "#eee8aa"

      -disabledbg     "#000000"
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
      # Settings:
      #

      # Button setting
      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised -bordercolor $colors(-bg)
      ttk::style map TButton -foreground [list active $colors(-selectfg) disabled $colors(-disabledfg)]
      ttk::style map TButton -background [list active $colors(-selectbg)]
      option add *TButton.cursor hand1

      # Menu button setting
      ttk::style configure TMenubutton -padding {8 4 4 4} -relief raised
      ttk::style map TMenubutton -foreground [list active $colors(-selectfg) disabled $colors(-disabledfg)]
      ttk::style map TMenubutton -background [list active $colors(-selectbg)]
      option add *TMenubutton.cursor hand1

      # Flat button setting
      ttk::style configure Toolbutton -padding {6 2} -anchor center
      ttk::style map Toolbutton -background [list active $colors(-selectbg) selected $colors(-selectbg)]

      # Radio button setting
      ttk::style configure TRadiobutton -padding 4
      option add *TRadiobutton.cursor hand1

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

      # Spin box setting
      ttk::style configure TSpinbox -arrowcolor $colors(-fg) -relief flat

      # Scroll bar setting
      ttk::style configure TScrollbar -arrowcolor $colors(-fg)

      # Paned window
      ttk::style configure TPanedwindow -background $colors(-disabledfg)

      # Combo box setting
      ttk::style configure TCombobox -arrowcolor $colors(-fg) -relief flat

      # Tree view (like cargo, trading) setting
      ttk::style configure Treeview -background $colors(-bg)
      ttk::style configure Treeview.Item -padding {2 0 0 0}
      ttk::style configure Treeview -rowheight [expr {[font metrics InterfaceFont -linespace] + 2}]
      ttk::style map Treeview \
         -background [list selected $colors(-selectbg)] \
         -foreground [list selected $colors(-selectfg)]

      # Check button setting
      ttk::style configure TCheckbutton -padding 4 -indicatorcolor $colors(-bg)
      ttk::style map TCheckbutton -indicatorcolor \
         [list selected $colors(-selectfg)]

      # Canvas setting
      option add *Canvas.highlightThickness 0

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
      option add *Text.relief flat
   }
}
