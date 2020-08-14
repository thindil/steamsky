package require Tk 8.6.0

namespace eval ttk::theme::steamsky {

   variable version 0.2
   package provide ttk::theme::steamsky $version

   #
   # Colors
   #

   variable colors
   array set colors {
      -verydarkorange "#1a130c"
      -palegoldenrod  "#eee8aa"
      -gray           "#7f8c8d"
      -darkred        "#600000"
      -goldenyellow   "#ffdf00"
      -blue           "#458588"
      -pink           "#b16286"
      -darkorange     "#372412"
      -almostblackred "#120d0d"
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
   # Images
   #
   variable I
   set I(checkbox-checked) [image create photo -file \
      [file join [file dirname [info script]] images checkbox-checked.svg] \
      -format {svg -scaletoheight 22}]
   set I(checkbox-unchecked) [image create photo -file \
      [file join [file dirname [info script]] images checkbox-unchecked.svg] \
      -format {svg -scaletoheight 22}]

   #
   # Create theme
   #

   ttk::style theme create steamsky -parent clam -settings {
      ttk::style configure . \
         -background $colors(-verydarkorange) \
         -foreground $colors(-palegoldenrod) \
         -troughcolor $colors(-verydarkorange) \
         -selectbackground $colors(-darkred) \
         -selectforeground $colors(-goldenyellow) \
         -fieldbackground $colors(-verydarkorange) \
         -font InterfaceFont \
         -borderwidth 1 \
         -focuscolor $colors(-goldenyellow) \
         -bordercolor $colors(-darkorange) \
         -lightcolor $colors(-verydarkorange) \
         -darkcolor $colors(-verydarkorange)

      ttk::style map . -foreground [list disabled $colors(-gray)]

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
      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised -foreground $colors(-goldenyellow)
      ttk::style map TButton -foreground [list active $colors(-goldenyellow) disabled $colors(-gray)]
      ttk::style map TButton -background [list active $colors(-darkred)]
      option add *TButton.cursor hand1

      # Menubutton setting
      ttk::style configure TMenubutton -padding {8 4 4 4} -relief raised -foreground $colors(-goldenyellow) -arrowcolor $colors(-goldenyellow)
      ttk::style map TMenubutton -foreground [list active $colors(-goldenyellow) disabled $colors(-gray)]
      ttk::style map TMenubutton -background [list active $colors(-darkred)]
      option add *TMenubutton.cursor hand1

      # Flat button setting
      ttk::style configure Toolbutton -padding {6 2} -anchor center -foreground $colors(-goldenyellow)
      ttk::style map Toolbutton -background [list active $colors(-darkred) selected $colors(-almostblackred)] -relief [list selected sunken]

      # Flat button for male gender
      ttk::style configure Male.Toolbutton -foreground $colors(-blue)

      # Flat button for female gender
      ttk::style configure Female.Toolbutton -foreground $colors(-pink)

      # Radiobutton setting
      ttk::style configure TRadiobutton -padding 4
      option add *TRadiobutton.cursor hand1

      # Separator setting
      ttk::style configure TSeparator -background $colors(-verydarkorange)

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

      # Progressbar setting
      ttk::style configure TProgressbar -background red

      # Entry setting
      ttk::style configure TEntry -insertcolor $colors(-goldenyellow) -foreground $colors(-goldenyellow) -fieldbackground $colors(-almostblackred)

      # Spinbox setting
      ttk::style configure TSpinbox -arrowcolor $colors(-palegoldenrod) -relief flat -foreground $colors(-goldenyellow) -insertcolor $colors(-goldenyellow) -fieldbackground $colors(-almostblackred)

      # Scrollbar setting
      ttk::style configure TScrollbar -arrowcolor $colors(-palegoldenrod)

      # Paned window
      ttk::style configure TPanedwindow -background $colors(-gray)

      # Combobox setting
      ttk::style configure TCombobox -arrowcolor $colors(-palegoldenrod) -relief flat
      option add *TCombobox*Listbox.cursor hand1
      bind TCombobox <Motion> {
         %W configure -cursor hand1
      }

      # Treeview (like cargo, trading) setting
      ttk::style configure Treeview -fieldbackground $colors(-almostblackred) -background $colors(-almostblackred)
      ttk::style configure Treeview.Item -padding {2 0 0 0}
      ttk::style configure Treeview -rowheight [expr {[font metrics InterfaceFont -linespace] + 2}]
      ttk::style configure Heading -relief raised
      ttk::style map Treeview \
         -background [list selected $colors(-darkred)] \
         -foreground [list selected $colors(-goldenyellow)]

      # Check button setting
      ttk::style configure TCheckbutton -padding 4 -indicatorcolor $colors(-verydarkorange)
      ttk::style map TCheckbutton -indicatorcolor \
         [list selected $colors(-goldenyellow)]

      # Info frames (like modules, items, etc)
      ttk::style configure TLabelframe.Label -foreground darkgreen
      ttk::style configure TLabelframe -relief raised -bordercolor darkgreen

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
      option add *Text.cursor arrow
   }
}
