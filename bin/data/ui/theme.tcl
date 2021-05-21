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
package require extrafont 1.2

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
      -darkred           "#500000"
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

   # Load needed fonts
   foreach fontfile [glob -directory [file join [file dirname [info script]] fonts] *.*tf] {
      extrafont::load [file normalize $fontfile]
   }

   # Font used in drawing the game map
   font create MapFont -family {Hack NF} -size 16
   # Font used to show all text in game
   font create InterfaceFont -family {Amarante} -size 14
   # Font used to show the game help
   font create HelpFont -family {Roboto} -size 14
   # Font used to show the bold text in help
   font create BoldHelpFont -family {Roboto} -size 14 -weight bold
   # Font used to show the underlined text in help
   font create UnderlineHelpFont -family {Roboto} -underline true -size 14
   # Font used to show the italic text in help
   font create ItalicHelpFont -family {Roboto} -size 14 -slant italic
   # Font used as icons for buttons/labels
   font create InterfaceIcons -family {Font Awesome 5 Free Solid} -size 14
   # Overstriked font used for example in combat or crafting menu
   font create OverstrikedFont -family {Amarante} -size 14 -overstrike true
   # Underlined font uset for buttons with URL's links
   font create UnderlineFont -family {Amarante} -underline true -size 14


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
         -bordercolor $colors(-verydarkorange) \
         -lightcolor $colors(-darkorange) \
         -darkcolor black

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
      ttk::style configure TButton -padding {8 4 8 4} -width -10 -anchor center -relief raised -foreground $colors(-goldenyellow) -background $colors(-almostblackorange)
      ttk::style map TButton -foreground [list active $colors(-palegoldenrod) disabled $colors(-gray)]
      ttk::style map TButton -background [list active $colors(-darkred) disabled $colors(-verydarkorange)]
      ttk::style map TButton -bordercolor [list active $colors(-palegoldenrod)]
      option add *TButton.cursor hand1

      # Menubutton setting
      ttk::style configure TMenubutton -padding {8 4 4 4} -relief raised -foreground $colors(-goldenyellow) -arrowcolor $colors(-goldenyellow) -background $colors(-almostblackorange)
      ttk::style map TMenubutton -foreground [list active $colors(-palegoldenrod) disabled $colors(-gray)]
      ttk::style map TMenubutton -background [list active $colors(-darkred)]
      ttk::style map TMenubutton -bordercolor [list active $colors(-palegoldenrod)]
      ttk::style map TMenubutton -arrowcolor [list active $colors(-palegoldenrod)]
      option add *TMenubutton.cursor hand1

      # Small buttons settings (like minimize/maximize buttons)
      ttk::style configure Small.TButton -padding {6 2} -width 0 -font InterfaceIcons -background $colors(-almostblackorange)
      ttk::style map Small.TButton -font [list active InterfaceIcons]

      # Icon and switch buttons setting
      # Default icon and switch button
      ttk::style configure Toolbutton -padding {6 2} -anchor center -foreground $colors(-goldenyellow) -background $colors(-almostblackorange) -lightcolor $colors(-darkorange) -darkcolor black -relief raised
      ttk::style map Toolbutton -background [list active $colors(-darkred) selected $colors(-almostblackred)] -relief [list active raised selected sunken]
      ttk::style map Toolbutton -foreground [list active $colors(-palegoldenrod) disabled $colors(-gray)]
      ttk::style map Toolbutton -bordercolor [list active $colors(-palegoldenrod)]
      # Icon button for male gender
      ttk::style configure Male.Toolbutton -foreground $colors(-blue) -font InterfaceIcons
      # Icon button for female gender
      ttk::style configure Female.Toolbutton -foreground $colors(-pink) -font InterfaceIcons
      # Icon button for generate random names for player and ship
      ttk::style configure Header.Toolbutton -font InterfaceIcons

      # Radiobutton setting
      ttk::style configure TRadiobutton -padding 4
      option add *TRadiobutton.cursor hand1

      # Link button setting
      ttk::style configure Link.Toolbutton -relief flat -font UnderlineFont -background $colors(-verydarkorange)

      # Map moving buttons
      ttk::style configure Map.Toolbutton -relief flat -background $colors(-verydarkorange)

      # Separator setting
      ttk::style configure TSeparator -background $colors(-verydarkorange)

      # Labels setting
      # Labels with red text
      ttk::style configure Headerred.TLabel -foreground red
      # Labels with green text
      ttk::style configure Headergreen.TLabel -foreground green
      # Labels with gray text
      ttk::style configure Gray.TLabel -foreground $colors(-gray)
      # Dialogs header label
      ttk::style configure Header.TLabel -background $colors(-darkorange) -anchor center -justify center

      # Progressbar setting
      # Default progressbars
      ttk::style configure TProgressbar -background red -troughcolor $colors(-almostblackred) -bordercolor $colors(-darkorange) -borderwidth 0
      # Green horizontal progress bar
      ttk::style configure green.Horizontal.TProgressbar -background green
      # Blue horizontal progress bar
      ttk::style configure blue.Horizontal.TProgressbar -background $colors(-blue)
      # Yellow horizontal progress bar
      ttk::style configure yellow.Horizontal.TProgressbar -background $colors(-goldenyellow)
      # Experience progressbars
      ttk::style configure experience.Horizontal.TProgressbar -background $colors(-darkyellow) -troughcolor $colors(-verydarkorange) -bordercolor $colors(-verydarkorange)
      # Dual progressbar (like reputation in bases)
      ttk::style configure ProgressBar.TFrame -relief solid -bordercolor $colors(-darkorange) -background $colors(-almostblackred)
      ttk::style configure GreenProgressBar.TFrame -background green
      ttk::style configure RedProgressBar.TFrame -background red

      # Entry setting
      ttk::style configure TEntry -insertcolor $colors(-goldenyellow) -foreground $colors(-goldenyellow) -fieldbackground $colors(-almostblackred) -padding 4 -lightcolor black -bordercolor $colors(-darkorange)

      # Spinbox setting
      ttk::style configure TSpinbox -arrowcolor $colors(-goldenyellow) -foreground $colors(-goldenyellow) -insertcolor $colors(-goldenyellow) -fieldbackground $colors(-almostblackred) -padding 4
      ttk::style map TSpinbox -arrowcolor [list active $colors(-palegoldenrod)]

      # Scrollbar setting
      ttk::style configure TScrollbar -arrowcolor $colors(-goldenyellow)
      ttk::style map TScrollbar -arrowcolor [list active $colors(-palegoldenrod)]
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
      ttk::style configure TCombobox -arrowcolor $colors(-goldenyellow) -padding 4 -foreground $colors(-goldenyellow) -selectbackground $colors(-verydarkorange) -bordercolor black
      ttk::style map TCombobox -arrowcolor [list active $colors(-palegoldenrod)]
      option add *TCombobox*Listbox.cursor hand1
      option add *TCombobox*Listbox.highlightThickness 1
      option add *TCombobox*Listbox.highlightColor $colors(-darkorange)
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

      # Table widget (like looting, shipyard) screen
      ttk::style configure Table -headercolor $colors(-darkorange) -rowcolor $colors(-almostblackred)

      # Check button setting
      ttk::style configure TCheckbutton -padding 4
      option add *TCheckbutton.cursor hand1

      # Scale (like setting reward for missions)
      ttk::style configure TScale -troughrelief sunken -bordercolor black
      ttk::style map TScale -foreground [list active $colors(-palegoldenrod)]
      ttk::style map TScale -background [list active $colors(-darkred)]
      ttk::style map TScale -bordercolor [list active $colors(-palegoldenrod)]
      option add *TScale.cursor hand1

      # Info frames (like modules, items, etc)
      ttk::style configure TLabelframe.Label -foreground green
      ttk::style configure TLabelframe -relief raised -bordercolor darkgreen

      # Main frames of the game
      ttk::style configure Main.TFrame -relief solid -borderwidth 1 -bordercolor $colors(-darkorange)

      # Last messages frame
      ttk::style configure LastMessages.TFrame -relief solid -bordercolor $colors(-gray) -borderwidth 1

      # Canvas setting
      option add *Canvas.highlightThickness 0

      # Tooltips setting
      set ::tooltip::labelOpts [list -relief groove -borderwidth 2 \
         -background black -foreground $colors(-palegoldenrod) -padx 5 -pady 5]

      # Map info label
      ttk::style configure MapInfo.TLabel -background black
      ttk::style configure MapInfoRed.TLabel -background black -foreground red
      ttk::style configure MapInfoGreen.TLabel -background black -foreground green
      ttk::style configure MapInfo.TFrame -bordercolor $colors(-gray) -background black

      # Dialogs (like messages, move map options, etc)
      ttk::style configure Dialog.TFrame -relief solid -bordercolor $colors(-darkorange)

      # Menus setting
      option add *Menu.relief raised
      option add *Menu.activeBorderWidth 0

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
      option add *Text.highlightThickness 0
   }
}
