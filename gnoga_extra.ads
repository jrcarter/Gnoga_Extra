-- Helper types and Create operations for frequently combined Gnoga widgets
--
-- Copyright (C) 2018 by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Gnoga.Gui.Element.Form;

package Gnoga_Extra is
   type Check_Info is tagged limited record
      Box   : Gnoga.Gui.Element.Form.Check_Box_Type;
      Label : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   procedure Create (Box     : in out Check_Info;
                     Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Label   : in     String;
                     Checked : in     Boolean := False;
                     Name    : in     String  := "";
                     ID      : in     String  := "");
   -- Creates a check box with a label of Label in Form
   -- Checked, Name, and ID are passed to Create for Box.Box

   type Text_List is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   type Orientation_ID is (Horizontal, Vertical);

   type Radio_Info is record
      Button : Gnoga.Gui.Element.Form.Radio_Button_Access;
      Label  : Gnoga.Gui.Element.Form.Label_Access;
   end record;

   package Radio_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Radio_Info);

   procedure Create (Radio        :    out Radio_Lists.Vector;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Label        : in     Text_List;
                     Name         : in     String;
                     Orientation  : in     Orientation_ID := Vertical;
                     ID           : in     String         := "");
   -- Creates a set of radio buttons in Form, one for each element of Label
   -- For I in Label'Range, the Ith button will have Label (I) as its label
   -- The Label'First button will be checked; the others will not
   -- All radio buttons in a set must have the same Name
   -- If Orientation = Horizontal, the button will be on the same line; otherwise, Form.New_Line will be called
   -- between each pair of buttons

   type Text_Info is tagged limited record
      Box   : Gnoga.Gui.Element.Form.Text_Type;
      Label : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   procedure Create (Box          : in out Text_Info;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Text         : in     String   := "";
                     Label        : in     String   := "";
                     Placeholder  : in     String   := "";
                     Width        : in     Positive := 20;
                     Name         : in     String   := "";
                     ID           : in     String   := "");
   -- Creates a text box with a label of Label in Form
   -- Text is the initial text in the box
   -- Placeholder will be the placeholder text for the box
   -- Width is the width of the box in characters
   -- Name and ID are passed to Create for Box.Box
end Gnoga_Extra;
