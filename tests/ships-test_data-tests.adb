--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Test_Data.

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
package body Ships.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Create_Ship_14edf1_469250
     (Proto_Index, Name: Unbounded_String; X: Map_X_Range; Y: Map_Y_Range;
      Speed: Ship_Speed; Random_Upgrades: Boolean := True)
      return Ship_Record is
   begin
      begin
         pragma Assert(Proto_Ships_List.Contains(Key => Proto_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_CreateShip test requirement violated");
      end;
      declare
         Test_Create_Ship_14edf1_469250_Result: constant Ship_Record :=
           GNATtest_Generated.GNATtest_Standard.Ships.Create_Ship
             (Proto_Index, Name, X, Y, Speed, Random_Upgrades);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_CreateShip test commitment violated");
         end;
         return Test_Create_Ship_14edf1_469250_Result;
      end;
   end Wrap_Test_Create_Ship_14edf1_469250;
--  end read only

--  begin read only
   procedure Test_Create_Ship_test_createship(Gnattest_T: in out Test);
   procedure Test_Create_Ship_14edf1_469250(Gnattest_T: in out Test) renames
     Test_Create_Ship_test_createship;
--  id:2.2/14edf110e8654721/Create_Ship/1/0/test_createship/
   procedure Test_Create_Ship_test_createship(Gnattest_T: in out Test) is
      function Create_Ship
        (Proto_Index, Name: Unbounded_String; X: Map_X_Range; Y: Map_Y_Range;
         Speed: Ship_Speed; Random_Upgrades: Boolean := True)
         return Ship_Record renames
        Wrap_Test_Create_Ship_14edf1_469250;
--  end read only

      pragma Unreferenced(Gnattest_T);
      TestShip: constant Ship_Record :=
        Create_Ship
          (To_Unbounded_String("2"), Null_Unbounded_String, 5, 5, FULL_SPEED);

   begin

      Assert
        (TestShip.Name = To_Unbounded_String("Tiny pirates ship"),
         "Failed to create a new NPC ship.");

--  begin read only
   end Test_Create_Ship_test_createship;
--  end read only

--  begin read only
   function Wrap_Test_Count_Ship_Weight_dec0b9_0591fd
     (Ship: Ship_Record) return Positive is
   begin
      declare
         Test_Count_Ship_Weight_dec0b9_0591fd_Result: constant Positive :=
           GNATtest_Generated.GNATtest_Standard.Ships.Count_Ship_Weight(Ship);
      begin
         return Test_Count_Ship_Weight_dec0b9_0591fd_Result;
      end;
   end Wrap_Test_Count_Ship_Weight_dec0b9_0591fd;
--  end read only

--  begin read only
   procedure Test_Count_Ship_Weight_test_countshipweight
     (Gnattest_T: in out Test);
   procedure Test_Count_Ship_Weight_dec0b9_0591fd
     (Gnattest_T: in out Test) renames
     Test_Count_Ship_Weight_test_countshipweight;
--  id:2.2/dec0b99ac9e9a6b9/Count_Ship_Weight/1/0/test_countshipweight/
   procedure Test_Count_Ship_Weight_test_countshipweight
     (Gnattest_T: in out Test) is
      function Count_Ship_Weight(Ship: Ship_Record) return Positive renames
        Wrap_Test_Count_Ship_Weight_dec0b9_0591fd;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      if Count_Ship_Weight(Player_Ship) = 1 then
         Assert(True, "This test can only crash.");
         return;
      end if;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Count_Ship_Weight_test_countshipweight;
--  end read only

--  begin read only
   function Wrap_Test_Generate_Ship_Name_8ae92d_6fbc99
     (Owner: Tiny_String.Bounded_String) return Unbounded_String is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Owner) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_GenerateShipName test requirement violated");
      end;
      declare
         Test_Generate_Ship_Name_8ae92d_6fbc99_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Ships.Generate_Ship_Name
             (Owner);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_GenerateShipName test commitment violated");
         end;
         return Test_Generate_Ship_Name_8ae92d_6fbc99_Result;
      end;
   end Wrap_Test_Generate_Ship_Name_8ae92d_6fbc99;
--  end read only

--  begin read only
   procedure Test_Generate_Ship_Name_test_generateshipname
     (Gnattest_T: in out Test);
   procedure Test_Generate_Ship_Name_8ae92d_6fbc99
     (Gnattest_T: in out Test) renames
     Test_Generate_Ship_Name_test_generateshipname;
--  id:2.2/8ae92d5b6d9d8451/Generate_Ship_Name/1/0/test_generateshipname/
   procedure Test_Generate_Ship_Name_test_generateshipname
     (Gnattest_T: in out Test) is
      function Generate_Ship_Name
        (Owner: Tiny_String.Bounded_String) return Unbounded_String renames
        Wrap_Test_Generate_Ship_Name_8ae92d_6fbc99;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Generate_Ship_Name(Tiny_String.To_Bounded_String("POLEIS")) /=
         Null_Unbounded_String,
         "Failed to generate ship name.");

--  begin read only
   end Test_Generate_Ship_Name_test_generateshipname;
--  end read only

--  begin read only
   function Wrap_Test_Count_Combat_Value_145322_424a30 return Natural is
   begin
      declare
         Test_Count_Combat_Value_145322_424a30_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.Count_Combat_Value;
      begin
         return Test_Count_Combat_Value_145322_424a30_Result;
      end;
   end Wrap_Test_Count_Combat_Value_145322_424a30;
--  end read only

--  begin read only
   procedure Test_Count_Combat_Value_test_countcombatvalue
     (Gnattest_T: in out Test);
   procedure Test_Count_Combat_Value_145322_424a30
     (Gnattest_T: in out Test) renames
     Test_Count_Combat_Value_test_countcombatvalue;
--  id:2.2/145322adc54fa48a/Count_Combat_Value/1/0/test_countcombatvalue/
   procedure Test_Count_Combat_Value_test_countcombatvalue
     (Gnattest_T: in out Test) is
      function Count_Combat_Value return Natural renames
        Wrap_Test_Count_Combat_Value_145322_424a30;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      if Count_Combat_Value = 0 then
         Assert(True, "This test can only crash");
         return;
      end if;
      Assert(True, "This test can only crash");

--  begin read only
   end Test_Count_Combat_Value_test_countcombatvalue;
--  end read only

--  begin read only
   function Wrap_Test_Get_Cabin_Quality_3a9d5d_bc7a0e
     (Quality: Natural) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_GetCabinQuality test requirement violated");
      end;
      declare
         Test_Get_Cabin_Quality_3a9d5d_bc7a0e_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Ships.Get_Cabin_Quality
             (Quality);
      begin
         begin
            pragma Assert
              (Test_Get_Cabin_Quality_3a9d5d_bc7a0e_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_GetCabinQuality test commitment violated");
         end;
         return Test_Get_Cabin_Quality_3a9d5d_bc7a0e_Result;
      end;
   end Wrap_Test_Get_Cabin_Quality_3a9d5d_bc7a0e;
--  end read only

--  begin read only
   procedure Test_Get_Cabin_Quality_test_getcabinquality
     (Gnattest_T: in out Test);
   procedure Test_Get_Cabin_Quality_3a9d5d_bc7a0e
     (Gnattest_T: in out Test) renames
     Test_Get_Cabin_Quality_test_getcabinquality;
--  id:2.2/3a9d5d1acf73079a/Get_Cabin_Quality/1/0/test_getcabinquality/
   procedure Test_Get_Cabin_Quality_test_getcabinquality
     (Gnattest_T: in out Test) is
      function Get_Cabin_Quality(Quality: Natural) return String renames
        Wrap_Test_Get_Cabin_Quality_3a9d5d_bc7a0e;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Cabin_Quality(10) = "Empty room",
         "Failed to get quality of cabin.");

--  begin read only
   end Test_Get_Cabin_Quality_test_getcabinquality;
--  end read only

--  begin read only
   procedure Wrap_Test_Damage_Module_222cf0_819b51
     (Ship: in out Ship_Record; Module_Index: Modules_Container.Extended_Index;
      Damage: Positive; Death_Reason: String) is
   begin
      begin
         pragma Assert
           (Module_Index in
              Ship.Modules.First_Index .. Ship.Modules.Last_Index and
            Death_Reason'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_DamageModule test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Damage_Module
        (Ship, Module_Index, Damage, Death_Reason);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships.ads:0:):Test_DamageModule test commitment violated");
      end;
   end Wrap_Test_Damage_Module_222cf0_819b51;
--  end read only

--  begin read only
   procedure Test_Damage_Module_test_damagemodule(Gnattest_T: in out Test);
   procedure Test_Damage_Module_222cf0_819b51(Gnattest_T: in out Test) renames
     Test_Damage_Module_test_damagemodule;
--  id:2.2/222cf0d00b136333/Damage_Module/1/0/test_damagemodule/
   procedure Test_Damage_Module_test_damagemodule(Gnattest_T: in out Test) is
      procedure Damage_Module
        (Ship: in out Ship_Record;
         Module_Index: Modules_Container.Extended_Index; Damage: Positive;
         Death_Reason: String) renames
        Wrap_Test_Damage_Module_222cf0_819b51;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldDurability: constant Positive := Player_Ship.Modules(1).Durability;

   begin

      Damage_Module(Player_Ship, 1, 10, "during tests");
      AUnit.Assertions.Assert
        (Player_Ship.Modules(1).Durability + 10 = OldDurability,
         "Failed to damage player ship hull.");
      Player_Ship.Modules(1).Durability := OldDurability;

--  begin read only
   end Test_Damage_Module_test_damagemodule;
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
end Ships.Test_Data.Tests;
