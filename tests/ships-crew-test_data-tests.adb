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
   procedure Wrap_Test_Death_af2fea_eed6aa
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) is
   begin
      begin
         pragma Assert
           (Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
            Reason /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_Death test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Death
        (Member_Index, Reason, Ship, Create_Body);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_Death test commitment violated");
      end;
   end Wrap_Test_Death_af2fea_eed6aa;
--  end read only

--  begin read only
   procedure Test_Death_test_death(Gnattest_T: in out Test);
   procedure Test_Death_af2fea_eed6aa(Gnattest_T: in out Test) renames
     Test_Death_test_death;
--  id:2.2/af2fea911992db88/Death/1/0/test_death/
   procedure Test_Death_test_death(Gnattest_T: in out Test) is
      procedure Death
        (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
         Ship: in out Ship_Record; Create_Body: Boolean := True) renames
        Wrap_Test_Death_af2fea_eed6aa;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Crew: constant Crew_Container.Vector := Player_Ship.Crew;
      Amount: constant Positive :=
        Positive(Inventory_Container.Length(Container => Player_Ship.Cargo));

   begin

      Death(2, To_Unbounded_String("Test death"), Player_Ship);
      Assert
        (Player_Ship.Crew.Length + 1 = Crew.Length,
         "Failed to remove crew member on death.");
      Assert
        (Amount + 1 =
         Positive(Inventory_Container.Length(Container => Player_Ship.Cargo)),
         "Failed to add body of dead crew member.");
      Player_Ship.Crew := Crew;

--  begin read only
   end Test_Death_test_death;
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
