-- Helper types and Create operations for frequently combined Gnoga widgets
-- These are for Gnoga 1.x; those wishing to use Gnoga 2.x will need to replace String with Gnoga.String
--
-- Copyright (C) by PragmAda Software Engineering
--
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;
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

   type Graphic_Area is tagged limited record
      Canvas  : Gnoga.Gui.Element.Canvas.Canvas_Type;
      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   end record;

   procedure Create (Graphic : in out Graphic_Area;
                     Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     ID      : in     String := "");
   -- Creates Graphic.Canvas and gets a Context_2d for it in Graphic.Context
   -- Parent, Width, Height, and ID are passed to Create for Graphic.Canvas

   type Radio_Data is record
      Button : Gnoga.Gui.Element.Form.Radio_Button_Type;
      Label  : Gnoga.Gui.Element.Form.Label_Type;
      Text   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Radio_List is array (Positive range <>) of Radio_Data;

   type Radio_Info (Length : Positive) is tagged limited record
      List : Radio_List (1 .. Length);
   end record;

   type Orientation_ID is (Horizontal, Vertical);

   procedure Create (Radio        : in out Radio_Info;
                     Form         : in out Gnoga.Gui.Element.Form.Form_Type'Class;
                     Name         : in     String;
                     Orientation  : in     Orientation_ID := Vertical;
                     ID           : in     String         := "") with Pre => Name /= "";
   -- Creates a set of radio buttons in Form, one for each element of Radio.List
   -- For each value in Radio.List, the Button will have Label with Text as its label
   -- The Radio.List'First button will be checked; the others will not
   -- All radio buttons in a set must have the same Name
   -- If Orientation = Horizontal, the buttons will be on the same line; otherwise, Form.New_Line will be called
   -- between each pair of buttons
   -- If ID = "", ID is used as the ID parameter when creating the buttons;
   -- otherwise, the button for Radio.List (I) will have the ID I'Image & 'R' & ID

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
