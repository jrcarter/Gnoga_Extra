-- Helper types and Create operations for frequently combined Gnoga widgets
--
-- Copyright (C) by PragmAda Software Engineering
--
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

package body Gnoga_Extra is
   procedure Create (Box     : in out Check_Info;
                     Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Label   : in     String;
                     Checked : in     Boolean := False;
                     Name    : in     String  := "";
                     ID      : in     String  := "")
   is
      -- Empty
   begin -- Create
      Box.Box.Create (Form => Form, Checked => Checked, Name => Name, ID => ID);
      Box.Label.Create (Form => Form, Label_For => Box.Box, Content => Label, Auto_Place => False);
   end Create;

   procedure Create (Graphic : in out Graphic_Area;
                     Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     ID      : in     String := "")
   is
      -- Empty
   begin -- Create
      Graphic.Canvas.Create (Parent => Parent, Width => Width, Height => Height, ID => ID);
      Graphic.Context.Get_Drawing_Context_2D (Canvas => Graphic.Canvas);
   end Create;


   use Ada.Strings.Unbounded;

   procedure Create (Radio        : in out Radio_Info;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Name         : in     String;
                     Orientation  : in     Orientation_ID := Vertical;
                     ID           : in     String         := "")
   is
      function Radio_ID (I : Positive) return String is (if ID = "" then "" else I'Image & 'R' & ID);
   begin -- Create
      All_Buttons : for I in Radio.List'Range loop
         Radio.List (I).Button.Create (Form => Form, Checked => I = Radio.List'First, Name => Name, ID => Radio_ID (I) );
         Radio.List (I).Label.Create
            (Form => Form, Label_For => Radio.List (I).Button, Content => To_String (Radio.List (I).Text), Auto_Place => False);

         if I < Radio.List'Last and Orientation = Vertical then
            Form.New_Line;
         end if;
      end loop All_Buttons;
   end Create;

   procedure Create (Box          : in out Text_Info;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Text         : in     String   := "";
                     Label        : in     String   := "";
                     Placeholder  : in     String   := "";
                     Width        : in     Positive := 20;
                     Name         : in     String   := "";
                     ID           : in     String   := "")
   is
      -- Empty
   begin -- Create
      Box.Box.Create (Form => Form, Size => Width, Value => Text, Name => Name, ID => ID);
      Box.Box.Place_Holder (Value => Placeholder);
      Box.Label.Create (Form => Form, Label_For => Box.Box, Content => Label);
   end Create;
end Gnoga_Extra;
