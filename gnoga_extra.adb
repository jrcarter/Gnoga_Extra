-- Helper types and Create operations for frequently combined Gnoga widgets
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

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

   use Ada.Strings.Unbounded;

   procedure Create (Radio        :    out Radio_Lists.Vector;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Label        : in     Text_List;
                     Name         : in     String;
                     Orientation  : in     Orientation_ID := Vertical;
                     ID           : in     String         := "")
   is
      function Radio_ID (I : Positive) return String is (if ID = "" then "" else I'Image & 'R' & ID);

      Button : Radio_Info;
   begin -- Create
      Radio.Clear;

      All_Buttons : for I in Label'Range loop
         Button.Button := new Gnoga.Gui.Element.Form.Radio_Button_Type;
         Button.Button.Dynamic;
         Button.Button.Create (Form => Form, Checked => I = Label'First, Name => Name, ID => Radio_ID (I) );
         Button.Label := new Gnoga.Gui.Element.Form.Label_Type;
         Button.Label.Dynamic;
         Button.Label.Create (Form => Form, Label_For => Button.Button.all, Content => To_String (Label (I) ), Auto_Place => False);
         Radio.Append (New_Item => Button);

         if I < Label'Last and Orientation = Vertical then
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
