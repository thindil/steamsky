--    Copyright 2016-2018 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Game; use Game;
with Log; use Log;

package body ShipModules is

   procedure LoadShipModules(Reader: Tree_Reader) is
      NodesList: Node_List;
      ModulesData: Document;
      TempRecord: BaseModule_Data;
      Action: Unbounded_String;
      ModuleNode: Node;
   begin
      TempRecord :=
        (Name => Null_Unbounded_String, MType => ENGINE, Weight => 0,
         Value => 0, MaxValue => 0, Durability => 0,
         RepairMaterial => Null_Unbounded_String, RepairSkill => 2, Price => 0,
         InstallTime => 60, Unique => False, Size => 0,
         Description => Null_Unbounded_String, Index => Null_Unbounded_String);
      ModulesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ModulesData, "module");
      for I in 0 .. Length(NodesList) - 1 loop
         ModuleNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(ModuleNode, "index"));
         Action := To_Unbounded_String(Get_Attribute(ModuleNode, "action"));
         if Action = Null_Unbounded_String or
           Action = To_Unbounded_String("add") then
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(ModuleNode, "name"));
            TempRecord.MType :=
              ModuleType'Value(Get_Attribute(ModuleNode, "type"));
            TempRecord.Weight :=
              Natural'Value(Get_Attribute(ModuleNode, "weight"));
            TempRecord.Value :=
              Integer'Value(Get_Attribute(ModuleNode, "value"));
            TempRecord.MaxValue :=
              Integer'Value(Get_Attribute(ModuleNode, "maxvalue"));
            TempRecord.Durability :=
              Integer'Value(Get_Attribute(ModuleNode, "durability"));
            TempRecord.RepairMaterial :=
              To_Unbounded_String(Get_Attribute(ModuleNode, "material"));
            TempRecord.RepairSkill :=
              FindSkillIndex
                (To_Unbounded_String(Get_Attribute(ModuleNode, "skill")));
            TempRecord.Price :=
              Integer'Value(Get_Attribute(ModuleNode, "price"));
            TempRecord.InstallTime :=
              Positive'Value(Get_Attribute(ModuleNode, "installtime"));
            if Get_Attribute(ModuleNode, "unique") /= "" then
               TempRecord.Unique := True;
            end if;
            if Get_Attribute(ModuleNode, "size") /= "" then
               TempRecord.Size :=
                 Integer'Value(Get_Attribute(ModuleNode, "size"));
            end if;
            TempRecord.Description :=
              To_Unbounded_String(Node_Value(First_Child(ModuleNode)));
            Modules_List.Append(New_Item => TempRecord);
            LogMessage
              ("Module added: " & To_String(TempRecord.Name), Everything);
         else
            Modules_List.Delete(Index => FindProtoModule(TempRecord.Index));
            LogMessage
              ("Module removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Name => Null_Unbounded_String, MType => ENGINE, Weight => 0,
            Value => 0, MaxValue => 0, Durability => 0,
            RepairMaterial => Null_Unbounded_String, RepairSkill => 2,
            Price => 0, InstallTime => 60, Unique => False, Size => 0,
            Description => Null_Unbounded_String,
            Index => Null_Unbounded_String);
      end loop;
   end LoadShipModules;

   function FindProtoModule(Index: Unbounded_String) return Natural is
   begin
      for I in Modules_List.Iterate loop
         if Modules_List(I).Index = Index then
            return BaseModules_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindProtoModule;

end ShipModules;
