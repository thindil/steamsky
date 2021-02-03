# Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package require Tk 8.6.0

namespace eval ttk::theme::steamsky {

   variable version 1.0
   package provide ttk::theme::steamsky $version

   #
   # Colors
   #

   variable colors
   array set colors {
      -verydarkorange    "#1a130c"
      -palegoldenrod     "#eee8aa"
      -gray              "#7f8c8d"
      -darkred           "#600000"
      -goldenyellow      "#ffdf00"
      -blue              "#458588"
      -pink              "#b16286"
      -darkorange        "#372412"
      -almostblackred    "#120d0d"
      -darkyellow        "#7d7800"
      -almostblackorange "#291913"
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
   # Font used to show the bold text in help
   font create BoldHelpFont -family {Roboto} -size 14 -weight bold
   # Font used to show the underlined text in help
   font create UnderlineHelpFont -family {Roboto} -underline true
   # Font used to show the italic text in help
   font create ItalicHelpFont -family {Roboto} -size 14 -slant italic
   # Font used as icons for buttons/labels
   font create InterfaceIcons -family {Font Awesome 5 Free Solid} -size 14
   # Overstriked font used for example in combat or crafting menu
   font create OverstrikedFont -family {Amarante} -size 14 -overstrike true


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
         -troughcolor $colors(-almostblackred) \
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
      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised -foreground $colors(-goldenyellow) -background $colors(-almostblackorange) -darkcolor black -lightcolor $colors(-darkorange) -bordercolor $colors(-verydarkorange)
      ttk::style map TButton -foreground [list active $colors(-goldenyellow) disabled $colors(-gray)]
      ttk::style map TButton -background [list active $colors(-darkred)]
      option add *TButton.cursor hand1

      # Menubutton setting
      ttk::style configure TMenubutton -padding {8 4 4 4} -relief raised -foreground $colors(-goldenyellow) -arrowcolor $colors(-goldenyellow) -background $colors(-almostblackorange) -darkcolor black -lightcolor $colors(-darkorange) -bordercolor $colors(-verydarkorange)
      ttk::style map TMenubutton -foreground [list active $colors(-goldenyellow) disabled $colors(-gray)]
      ttk::style map TMenubutton -background [list active $colors(-darkred)]
      option add *TMenubutton.cursor hand1

      # Small buttons settings (like minimize/maximize buttons)
      ttk::style configure Small.TButton -padding {6 2} -width 0 -font InterfaceIcons -background $colors(-almostblackorange) -darkcolor black -lightcolor $colors(-darkorange) -bordercolor $colors(-verydarkorange)

      # Flat buttons setting
      # Default flat button
      ttk::style configure Toolbutton -padding {6 2} -anchor center -foreground $colors(-goldenyellow)
      ttk::style map Toolbutton -background [list active $colors(-darkred) selected $colors(-almostblackred)] -relief [list selected sunken]
      # Flat button for male gender
      ttk::style configure Male.Toolbutton -foreground $colors(-blue) -font InterfaceIcons
      # Flat button for female gender
      ttk::style configure Female.Toolbutton -foreground $colors(-pink) -font InterfaceIcons
      # Not needed state header button (ship info in right top corner)
      ttk::style configure Header.Toolbutton -font InterfaceIcons

      # Radiobutton setting
      ttk::style configure TRadiobutton -padding 4
      option add *TRadiobutton.cursor hand1

      # Separator setting
      ttk::style configure TSeparator -background $colors(-verydarkorange)

      # Labels setting
      # Labels with red text
      ttk::style configure Headerred.TLabel -foreground red
      # Labels with green text
      ttk::style configure Headergreen.TLabel -foreground green
      # Labels with gray text
      ttk::style configure Gray.TLabel -foreground $colors(-gray)

      # Progressbar setting
      # Default progressbars
      ttk::style configure TProgressbar -background red -troughcolor $colors(-almostblackred)
      # Green horizontal progress bar
      ttk::style configure green.Horizontal.TProgressbar -background green
      # Blue horizontal progress bar
      ttk::style configure blue.Horizontal.TProgressbar -background $colors(-blue)
      # Yellow horizontal progress bar
      ttk::style configure yellow.Horizontal.TProgressbar -background $colors(-goldenyellow)
      # Experience progressbars
      ttk::style configure experience.Horizontal.TProgressbar -background $colors(-darkyellow) -troughcolor $colors(-verydarkorange) -bordercolor $colors(-verydarkorange)

      # Entry setting
      ttk::style configure TEntry -insertcolor $colors(-goldenyellow) -foreground $colors(-goldenyellow) -fieldbackground $colors(-almostblackred) -padding 4

      # Spinbox setting
      ttk::style configure TSpinbox -arrowcolor $colors(-palegoldenrod) -relief flat -foreground $colors(-goldenyellow) -insertcolor $colors(-goldenyellow) -fieldbackground $colors(-almostblackred)

      # Scrollbar setting
      ttk::style configure TScrollbar -arrowcolor $colors(-palegoldenrod)
      bind TScrollbar <Motion> {
         if {[%W cget -orient] == "horizontal"} {
            %W configure -cursor sb_h_double_arrow
         } else {
            %W configure -cursor sb_v_double_arrow
         }
      }
      bind TScrollbar <MouseWheel> {
         [lindex [%W cget -command] 0] [lindex [%W cget -command] 1] scroll [expr (-1 * (%D / 120))] units
      }
      bind TScrollbar <Button-4> {
         [lindex [%W cget -command] 0] [lindex [%W cget -command] 1] scroll -1 units
      }
      bind TScrollbar <Button-5> {
         [lindex [%W cget -command] 0] [lindex [%W cget -command] 1] scroll 1 units
      }

      # Combobox setting
      ttk::style configure TCombobox -arrowcolor $colors(-goldenyellow) -relief flat -padding 4 -foreground $colors(-goldenyellow)
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
      # Help topics list
      ttk::style configure Help.Treeview -font HelpFont

      # Check button setting
      ttk::style configure TCheckbutton -padding 4

      # Info frames (like modules, items, etc)
      ttk::style configure TLabelframe.Label -foreground green
      ttk::style configure TLabelframe -relief raised -bordercolor darkgreen

      # Main frames of the game
      ttk::style configure Main.TFrame -relief solid -borderwidth 1

      # Last messages frame
      ttk::style configure LastMessages.TFrame -relief solid -bordercolor $colors(-gray) -borderwidth 1

      # Canvas setting
      option add *Canvas.highlightThickness 0

      # Tooltips setting
      set ::tooltip::labelOpts [list -relief groove -borderwidth 2 \
         -background black -foreground $colors(-palegoldenrod) -padx 5 -pady 5]

      # Dialog with getting string (like name of the ship, modules, etc)
      option add *TkSDialog.background [ttk::style lookup . -background]
      option add *TkSDialog*Button.foreground $colors(-goldenyellow)
      option add *TkSDialog*Entry.foreground $colors(-goldenyellow)

      # Map info label
      ttk::style configure MapInfo.TLabel -background black
      ttk::style configure MapInfoRed.TLabel -background black -foreground red
      ttk::style configure MapInfoGreen.TLabel -background black -foreground green
      ttk::style configure MapInfo.TFrame -bordercolor $colors(-gray)

      # Dialogs (like messages, move map options, etc)
      ttk::style configure Dialog.TFrame -relief solid

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
      option add *Text.cursor left_ptr
   }
}
