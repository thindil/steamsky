--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Crew.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Ships.Crew.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GetSkillLevel_f7e690_019f8b (Member: Member_Data; SkillIndex: Positive)  return Natural
   is
   begin
      begin
         pragma Assert
           (SkillIndex <= Skills_List.Last_Index);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(ships-crew.ads:0):Test_GetSkillLevel test requirement violated");
      end;
      declare
         Test_GetSkillLevel_f7e690_019f8b_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.Crew.GetSkillLevel (Member, SkillIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-crew.ads:0:):Test_GetSkillLevel test commitment violated");
         end;
         return Test_GetSkillLevel_f7e690_019f8b_Result;
      end;
   end Wrap_Test_GetSkillLevel_f7e690_019f8b;
--  end read only

--  begin read only
   procedure Test_GetSkillLevel_test_getskilllevel (Gnattest_T : in out Test);
   procedure Test_GetSkillLevel_f7e690_019f8b (Gnattest_T : in out Test) renames Test_GetSkillLevel_test_getskilllevel;
--  id:2.2/f7e690bba6071759/GetSkillLevel/1/0/test_getskilllevel/
   procedure Test_GetSkillLevel_test_getskilllevel (Gnattest_T : in out Test) is
      function GetSkillLevel (Member: Member_Data; SkillIndex: Positive) return Natural renames Wrap_Test_GetSkillLevel_f7e690_019f8b;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetSkillLevel(PlayerShip.Crew(1), 1) = 0, "Failed to get real level of not owned skill.");
      Assert(GetSkillLevel(PlayerShip.Crew(1), 4) = 9, "Failed to get real level of skill.");

--  begin read only
   end Test_GetSkillLevel_test_getskilllevel;
--  end read only

--  begin read only
   procedure Wrap_Test_Death_211a27_9008b1 (MemberIndex: Positive; Reason: Unbounded_String; Ship: in out ShipRecord; CreateBody: Boolean := True) 
   is
   begin
      begin
         pragma Assert
           ((MemberIndex <= Ship.Crew.Last_Index and Reason /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_Death test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Death (MemberIndex, Reason, Ship, CreateBody);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_Death test commitment violated");
      end;
   end Wrap_Test_Death_211a27_9008b1;
--  end read only

--  begin read only
   procedure Test_Death_test_death (Gnattest_T : in out Test);
   procedure Test_Death_211a27_9008b1 (Gnattest_T : in out Test) renames Test_Death_test_death;
--  id:2.2/211a277189388faa/Death/1/0/test_death/
   procedure Test_Death_test_death (Gnattest_T : in out Test) is
   procedure Death (MemberIndex: Positive; Reason: Unbounded_String; Ship: in out ShipRecord; CreateBody: Boolean := True) renames Wrap_Test_Death_211a27_9008b1;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Crew: constant Crew_Container.Vector := PlayerShip.Crew;
      Amount: constant Positive := Positive(PlayerShip.Cargo.Length);

   begin

      Death(2, To_Unbounded_String("Test death"), PlayerShip);
      Assert(PlayerShip.Crew.Length + 1 = Crew.Length, "Failed to remove crew member on death.");
      Assert(Amount + 1 = Positive(PlayerShip.Cargo.Length), "Failed to add body of dead crew member.");
      PlayerShip.Crew := Crew;

--  begin read only
   end Test_Death_test_death;
--  end read only

--  begin read only
   procedure Wrap_Test_DeleteMember_a2fb7d_025eac (MemberIndex: Positive; Ship: in out ShipRecord) 
   is
   begin
      begin
         pragma Assert
           (MemberIndex <= Ship.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_DeleteMember test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.DeleteMember (MemberIndex, Ship);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_DeleteMember test commitment violated");
      end;
   end Wrap_Test_DeleteMember_a2fb7d_025eac;
--  end read only

--  begin read only
   procedure Test_DeleteMember_test_deletemember (Gnattest_T : in out Test);
   procedure Test_DeleteMember_a2fb7d_025eac (Gnattest_T : in out Test) renames Test_DeleteMember_test_deletemember;
--  id:2.2/a2fb7d07cd83aaf9/DeleteMember/1/0/test_deletemember/
   procedure Test_DeleteMember_test_deletemember (Gnattest_T : in out Test) is
   procedure DeleteMember (MemberIndex: Positive; Ship: in out ShipRecord) renames Wrap_Test_DeleteMember_a2fb7d_025eac;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Crew: constant Crew_Container.Vector := PlayerShip.Crew;

   begin

      DeleteMember(2, PlayerShip);
      Assert(Crew.Length = PlayerShip.Crew.Length + 1, "Failed to delete member from the player ship crew.");
      PlayerShip.Crew := Crew;

--  begin read only
   end Test_DeleteMember_test_deletemember;
--  end read only

--  begin read only
   function Wrap_Test_FindMember_b270de_38c9c9 (Order: Crew_Orders; Crew: Crew_Container.Vector := PlayerShip.Crew)  return Natural
   is
   begin
      declare
         Test_FindMember_b270de_38c9c9_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.Crew.FindMember (Order, Crew);
      begin
         return Test_FindMember_b270de_38c9c9_Result;
      end;
   end Wrap_Test_FindMember_b270de_38c9c9;
--  end read only

--  begin read only
   procedure Test_FindMember_test_findmember (Gnattest_T : in out Test);
   procedure Test_FindMember_b270de_38c9c9 (Gnattest_T : in out Test) renames Test_FindMember_test_findmember;
--  id:2.2/b270debda44d8b87/FindMember/1/0/test_findmember/
   procedure Test_FindMember_test_findmember (Gnattest_T : in out Test) is
      function FindMember (Order: Crew_Orders; Crew: Crew_Container.Vector := PlayerShip.Crew) return Natural renames Wrap_Test_FindMember_b270de_38c9c9;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(FindMember(Talk) = 1, "Failed to find crew member with selected order.");
      Assert(FindMember(Defend) = 0, "Failed to not find crew member with selected order.");

--  begin read only
   end Test_FindMember_test_findmember;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Ships.Crew.Test_Data.Tests;
