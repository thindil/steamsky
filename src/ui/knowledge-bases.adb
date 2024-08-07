-- Copyright (c) 2020-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases; use Bases;
with CoreUI;
with Game; use Game;
with Maps;
with Table; use Table;
with Utils;
with Utils.UI;

package body Knowledge.Bases is

   -- ****iv* KBases/KBases.Bases_Table
   -- FUNCTION
   -- Table with info about the know bases
   -- SOURCE
   Bases_Table: Table_Widget (Amount => 8);
   -- ****

   -- ****iv* KBases/KBases.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Bases_Indexes: Positive_Container.Vector;
   -- ****

   procedure Update_Bases_List(Base_Name: String := ""; Page: Positive := 1) is
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use CoreUI;

      Bases_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.bases.canvas");
      Bases_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Bases_Canvas & ".frame");
      procedure Update_Ada_Bases_List(B_Name: chars_ptr; P: Positive) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaBasesList";
   begin
      Update_Ada_Bases_List(B_Name => New_String(Str => Base_Name), P => Page);
      Bases_Table :=
        Create_Table
          (Parent => Widget_Image(Win => Bases_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Name"),
              2 => To_Unbounded_String(Source => "Distance"),
              3 => To_Unbounded_String(Source => "Coordinates"),
              4 => To_Unbounded_String(Source => "Population"),
              5 => To_Unbounded_String(Source => "Size"),
              6 => To_Unbounded_String(Source => "Owner"),
              7 => To_Unbounded_String(Source => "Type"),
              8 => To_Unbounded_String(Source => "Reputation")),
           Scrollbar =>
             Get_Widget
               (pathName => ".gameframe.paned.knowledgeframe.bases.scrolly"),
           Command => "SortKnownBases {" & Base_Name & "}",
           Tooltip_Text => "Press mouse button to sort the bases.");
      if Bases_Indexes.Is_Empty then
         Fill_Bases_Indexes_Loop :
         for I in Sky_Bases'Range loop
            Bases_Indexes.Append(New_Item => I);
         end loop Fill_Bases_Indexes_Loop;
      end if;
   end Update_Bases_List;

   -- ****o* KBases/KBases.Show_Bases_Command
   -- FUNCTION
   -- Show the list of known bases to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBases ?basename? ?page?
   -- Basename parameter is a string which will be looking for in the bases
   -- names, page parameter is a index of page from which starts showing
   -- bases.
   -- SOURCE
   function Show_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showBasesCommand";
      -- ****

   -- ****o* KBases/KBases.Show_Base_Info_Command
   -- FUNCTION
   -- Show information about the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseInfo baseindex
   -- BaseIndex is the index of the base to show
   -- SOURCE
   function Show_Base_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showBaseInfoCommand";
      -- ****

   -- ****it* KBases/KBases.Bases_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the known bases list
   -- OPTIONS
   -- NAMEASC        - Sort bases by name ascending
   -- NAMEDESC       - Sort bases by name descending
   -- DISTANCEASC    - Sort bases by distance ascending
   -- DISTANCEDESC   - Sort bases by distance descending
   -- POPULATIONASC  - Sort bases by population ascending
   -- POPULATIONDESC - Sort bases by population descending
   -- SIZEASC        - Sort bases by size ascending
   -- SIZEDESC       - Sort bases by size descending
   -- OWNERASC       - Sort bases by owner ascending
   -- OWNERDESC      - Sort bases by owner descending
   -- TYPEASC        - Sort bases by type ascending
   -- TYPEDESC       - Sort bases by type descending
   -- REPUTATIONASC  - Sort bases by reputation ascending
   -- REPUTATIONDESC - Sort bases by reputation descending
   -- COORDASC       - Sort bases by coordinates ascending
   -- COORDDESC      - Sort bases by coordinates descending
   -- NONE           - No sorting bases (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.4 - Added sorting by coordinates
   -- SOURCE
   type Bases_Sort_Orders is
     (NAMEASC, NAMEDESC, DISTANCEASC, DISTANCEDESC, POPULATIONASC,
      POPULATIONDESC, SIZEASC, SIZEDESC, OWNERASC, OWNERDESC, TYPEASC,
      TYPEDESC, REPUTATIONASC, REPUTATIONDESC, COORDASC, COORDDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* KBases/KBases.Default_Bases_Sort_Order
      -- FUNCTION
      -- Default sorting order for the list of known bases
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Bases_Sort_Order: constant Bases_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* KBases/KBases.Bases_Sort_Order
   -- FUNCTION
   -- The current sorting order for known bases list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Bases_Sort_Order: Bases_Sort_Orders := Default_Bases_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* KBases/KBases.Sort_Bases_Command
   -- FUNCTION
   -- Sort the list of known bases
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKnownBases x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Maps;
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Bases_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Base_Data is record
         Name: Bounded_String;
         Distance: Natural;
         Coords: Unbounded_String;
         Population: Integer;
         Size: Bases_Size;
         Owner: Bounded_String;
         Base_Type: Bounded_String;
         Reputation: Integer;
         Id: Positive;
      end record;
      type Bases_Array is array(Positive range <>) of Local_Base_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Bases: Bases_Array (Bases_Range);
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Base_Data) return Boolean is
      begin
         if Bases_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Bases_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Bases_Sort_Order = DISTANCEASC
           and then Left.Distance < Right.Distance then
            return True;
         end if;
         if Bases_Sort_Order = DISTANCEDESC
           and then Left.Distance > Right.Distance then
            return True;
         end if;
         if Bases_Sort_Order = POPULATIONASC
           and then Left.Population < Right.Population then
            return True;
         end if;
         if Bases_Sort_Order = POPULATIONDESC
           and then Left.Population > Right.Population then
            return True;
         end if;
         if Bases_Sort_Order = SIZEASC and then Left.Size < Right.Size then
            return True;
         end if;
         if Bases_Sort_Order = SIZEDESC and then Left.Size > Right.Size then
            return True;
         end if;
         if Bases_Sort_Order = OWNERASC and then Left.Owner < Right.Owner then
            return True;
         end if;
         if Bases_Sort_Order = OWNERDESC and then Left.Owner > Right.Owner then
            return True;
         end if;
         if Bases_Sort_Order = TYPEASC
           and then Left.Base_Type < Right.Base_Type then
            return True;
         end if;
         if Bases_Sort_Order = TYPEDESC
           and then Left.Base_Type > Right.Base_Type then
            return True;
         end if;
         if Bases_Sort_Order = REPUTATIONASC
           and then Left.Reputation < Right.Reputation then
            return True;
         end if;
         if Bases_Sort_Order = REPUTATIONDESC
           and then Left.Reputation > Right.Reputation then
            return True;
         end if;
         if Bases_Sort_Order = COORDASC
           and then Left.Coords < Right.Coords then
            return True;
         end if;
         if Bases_Sort_Order = COORDDESC
           and then Left.Coords > Right.Coords then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Bases is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Base_Data,
         Array_Type => Bases_Array);
   begin
      case Column is
         when 1 =>
            if Bases_Sort_Order = NAMEASC then
               Bases_Sort_Order := NAMEDESC;
            else
               Bases_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Bases_Sort_Order = DISTANCEASC then
               Bases_Sort_Order := DISTANCEDESC;
            else
               Bases_Sort_Order := DISTANCEASC;
            end if;
         when 3 =>
            if Bases_Sort_Order = COORDASC then
               Bases_Sort_Order := COORDDESC;
            else
               Bases_Sort_Order := COORDASC;
            end if;
         when 4 =>
            if Bases_Sort_Order = POPULATIONASC then
               Bases_Sort_Order := POPULATIONDESC;
            else
               Bases_Sort_Order := POPULATIONASC;
            end if;
         when 5 =>
            if Bases_Sort_Order = SIZEASC then
               Bases_Sort_Order := SIZEDESC;
            else
               Bases_Sort_Order := SIZEASC;
            end if;
         when 6 =>
            if Bases_Sort_Order = OWNERASC then
               Bases_Sort_Order := OWNERDESC;
            else
               Bases_Sort_Order := OWNERASC;
            end if;
         when 7 =>
            if Bases_Sort_Order = TYPEASC then
               Bases_Sort_Order := TYPEDESC;
            else
               Bases_Sort_Order := TYPEASC;
            end if;
         when 8 =>
            if Bases_Sort_Order = REPUTATIONASC then
               Bases_Sort_Order := REPUTATIONDESC;
            else
               Bases_Sort_Order := REPUTATIONASC;
            end if;
         when others =>
            null;
      end case;
      if Bases_Sort_Order = NONE then
         return TCL_OK;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Fill_Local_Bases_Loop :
      for I in Sky_Bases'Range loop
         Local_Bases(I) :=
           (Name => Sky_Bases(I).Name,
            Distance =>
              Count_Distance
                (Destination_X => Sky_Bases(I).Sky_X,
                 Destination_Y => Sky_Bases(I).Sky_Y),
            Coords =>
              To_Unbounded_String
                (Source =>
                   "X:" & Natural'Image(Sky_Bases(I).Sky_X) & " Y:" &
                   Natural'Image(Sky_Bases(I).Sky_Y)),
            Population =>
              (if Sky_Bases(I).Visited = (others => 0) then -1
               else Sky_Bases(I).Population),
            Size =>
              (if Sky_Bases(I).Visited = (others => 0) then UNKNOWN
               else Sky_Bases(I).Size),
            Owner =>
              (if Sky_Bases(I).Visited = (others => 0) then Null_Bounded_String
               else Sky_Bases(I).Owner),
            Base_Type =>
              (if Sky_Bases(I).Visited = (others => 0) then Null_Bounded_String
               else Sky_Bases(I).Base_Type),
            Reputation =>
              (if Sky_Bases(I).Visited = (others => 0) then 200
               else Sky_Bases(I).Reputation.Level),
            Id => I);
      end loop Fill_Local_Bases_Loop;
      Sort_Bases(Container => Local_Bases);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Bases_Indexes.Clear;
      Fill_Bases_Indexes_Loop :
      for Base of Local_Bases loop
         Bases_Indexes.Append(New_Item => Base.Id);
      end loop Fill_Bases_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Update_Bases_List(Base_Name => CArgv.Arg(Argv => Argv, N => 1));
      return TCL_OK;
   end Sort_Bases_Command;

   procedure Add_Knowledge_Bases_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowBases", Ada_Command => Show_Bases_Command'Access);
      Add_Command
        (Name => "ShowBaseInfo", Ada_Command => Show_Base_Info_Command'Access);
      Add_Command
        (Name => "SortKnownBases", Ada_Command => Sort_Bases_Command'Access);
   end Add_Knowledge_Bases_Commands;

end Knowledge.Bases;
