package require Tk 8.6.0

namespace eval ttk::theme::steamsky {

   variable version 0.1
   package provide ttk::theme::steamsky $version

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

   ttk::style theme create steamsky -parent default -settings {
      ttk::style configure . \
         -background $colors(-bg) \
         -foreground $colors(-fg) \
         -troughcolor $colors(-bg) \
         -selectbackground $colors(-selectbg) \
         -selectforeground $colors(-selectfg) \
         -fieldbackground $colors(-window) \
         -font {-family Amarante -size 14} \
         -borderwidth 1 \
         -focuscolor $colors(-focuscolor)

      ttk::style map . -foreground [list disabled $colors(-disabledfg)]

      #
      # Layouts:
      #

      ttk::style layout Vertical.TScrollbar {
         Vertical.Scrollbar.trough -sticky ns -children {
            Vertical.Scrollbar.thumb -expand true
         }
      }

      ttk::style layout Horizontal.TScrollbar {
         Horizontal.Scrollbar.trough -sticky ew -children {
            Horizontal.Scrollbar.thumb -expand true
         }
      }

      #
      # Settings:
      #

      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised
      ttk::style configure TMenubutton -padding {8 4 4 4}
      ttk::style configure Toolbutton -padding {6 2} -anchor center
      ttk::style configure TCheckbutton -padding 4
      ttk::style configure TRadiobutton -padding 4
      ttk::style configure TSeparator -background $colors(-bg)
      ttk::style configure Header.Toolbutton -font {-family "Font Awesome 5 Free Solid" -size 14}
      ttk::style configure Headerred.Toolbutton -font {-family "Font Awesome 5 Free Solid" -size 14} -foreground red
      ttk::style configure Headergreen.Toolbutton -font {-family "Font Awesome 5 Free Solid" -size 14} -foreground green
      ttk::style configure Headerred.TLabel -foreground red
      ttk::style configure Headergreen.TLabel -foreground green
      ttk::style configure TProgressbar -background red

      ttk::style map TPanedwindow -background [list hover $colors(-checklight)]
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

      ttk::style configure Treeview -background $colors(-bg)
      ttk::style configure Treeview.Item -padding {2 0 0 0}
      ttk::style map Treeview \
         -background [list selected $colors(-selectbg)] \
         -foreground [list selected $colors(-selectfg)]

      tk_setPalette background [ttk::style lookup . -background] \
         foreground [ttk::style lookup . -foreground] \
         highlightColor [ttk::style lookup . -focuscolor] \
         selectBackground [ttk::style lookup . -selectbackground] \
         selectForeground [ttk::style lookup . -selectforeground] \
         activeBackground [ttk::style lookup . -selectbackground] \
         activeForeground [ttk::style lookup . -selectforeground]
      option add *font [ttk::style lookup . -font]
   }

}
