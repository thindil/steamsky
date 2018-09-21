--    Copyright 2018 Bartek thindil Jasicki
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
with Log; use Log;

package body Careers is

   procedure LoadCareers(Reader: Tree_Reader) is
      TempRecord: CareerRecord;
      NodesList, ChildNodes: Node_List;
      CareersData: Document;
      RemoveIndex: Unbounded_String;
      DeleteIndex: Positive;
      TmpSkills: UnboundedString_Container.Vector;
   begin
      TempRecord :=
        (Index => Null_Unbounded_String,
         Name => Null_Unbounded_String,
         Skills => TmpSkills);
      CareersData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(CareersData, "career");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
         TempRecord.Name :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I),
              "skill");
         for J in 0 .. Length(ChildNodes) - 1 loop
            TempRecord.Skills.Append
            (New_Item =>
               To_Unbounded_String
                 (Get_Attribute(Item(ChildNodes, J), "name")));
         end loop;
         if Get_Attribute(Item(NodesList, I), "remove") = "" then
            Careers_List.Append(New_Item => TempRecord);
            LogMessage
              ("Career added: " & To_String(TempRecord.Name),
               Everything);
         else
            RemoveIndex :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "remove"));
            for J in Careers_List.Iterate loop
               if Careers_List(J).Index = RemoveIndex then
                  DeleteIndex := Careers_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            Careers_List.Delete(Index => DeleteIndex);
            LogMessage
              ("Career removed: " & To_String(RemoveIndex),
               Everything);
         end if;
         TempRecord :=
           (Index => Null_Unbounded_String,
            Name => Null_Unbounded_String,
            Skills => TmpSkills);
      end loop;
   end LoadCareers;

end Careers;
