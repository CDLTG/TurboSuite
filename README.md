# TurboSuite
TurboSuite is a collection of AutoLISP routines developed to automate repetitive AutoCAD tasks.

## Table of Contents
- [TurboTape](#turbotape)
  - [TurboTape (Horizontal)](#turbotape-horizontal)
  - [TurboTape (Vertical)](#turbotape-vertical)
  - [TurboTape (Fixture)](#turbotape-fixture)
  - [TurboTape (Custom)](#turbotape-custom)
  - [TurboTape (Array)](#turbotape-array)
- [TurboDriver](#turbodriver)
- [TurboBubble](#turbobubble)
- [TurboScale](#turboscale)
- [TurboChange](#turbochange)
- [TurboName](#turboname)

## TurboTape
TurboTape is comprised of five subroutines which allow the user to construct any object that requires a linear quantity. While this can also be achieved using dynamic blocks, TurboTape names each block independently for use in conjunction with the counts sheet. During creation, you will be prompted for a `Type` and a `Tag`. The `Type` will be your block name and is a required entry. The `Tag` is a brief description of the block, and is an optional entry. For certain options, you will also be prompted for a `Quantity`. The `Quantity` value will default to `1`, but can be given any desired integer value.

  - ### TurboTape (Horizontal)
    Draw horizontal linear tape of any kind.
     - **Commands:** `tt;` `tt;h`
     - **Limitations:**
       - Requires a polyline as the base object.
       - Requires a new base polyline if lengths need to be adjusted.
       - Circles must be converted into polylines with arcs.
  
      ![TurboTape Horizontal gif](/GIF/TurboTape(Horizontal).gif)

  - ### TurboTape (Vertical)
    Draw vertical linear tape of any kind. TurboTape Vertical can be called as 1,2,3 or 4 runs depending on size and available drafting space.
     - **Command:** `tt;v`
     - **Limitations:**
       - Requires manual input of tape lengths; follow prompts closely.
       - Once created, the new block will need to be moved into place manually.

      ![TurboTape Vertical gif](/GIF/TurboTape(Vertical).gif)

  - ### TurboTape (Fixture)
    Draw linear horizontal fixtures including a `Width` dimension. This value will default to `2`, but can be given any desired real value.
     - **Command:** `tt;f`
     - **Limitations:**
       - Requires a polyline as the base object.
       - Requires a new base polyline if lengths need to be adjusted.
       - Circles must be converted into polylines with arcs.

      ![TurboTape Fixture gif](/GIF/TurboTape(Fixture).gif)

  - ### TurboTape (Custom)
    Draw allowance for a custom linear tape length.
     - **Command:** `tt;c`
     - **Limitations:**
       - Requires a polyline as the base object.
       - Requires manual input of tape lengths; follow prompts closely.

      ![TurboTape Fixture gif](/GIF/TurboTape(Custom).gif)

  - ### TurboTape (Array)
    Draw an array of linear tape, primarily used for stairs. You will be prompted for a `Stair Direction`, which will default to `Down`. This value is used to establish the offset direction of each line. Select a `Stair Direction` value relative to the location of your base polyline. You will also be prompted to select `Additional Stairs`. Select any point along the stair lines in the xref.
     - **Command:** `tt;a`
     - **Limitations:**
       - Requires a polyline as the base object.
       - **_Do not use for stairs of different lengths!_**

      ![TurboTape Array gif](/GIF/TurboTape(Array).gif)

## TurboDriver
TurboDriver is used to quickly calculate and copy driver information to the Windows clipboard for use in external programs. When initiating TurboDriver, you will be able to set a `Watts` value which will default to `5.5`. This value will persist until changed or the drawing is closed. Blocks processed by TurboDriver will be passed through exacting logic to determine the "ideal" driver setup, which can sometimes be impractical. _Verify all TurboDriver results when dealing with complex scenarios!_
- **Command:** `td`
- **Limitations:**
  - Can only be used with official [TurboTape](#turbotape) blocks.
  - Can generate results that conflict with the drawing when the TurboDriver logic is impractical or the drawing is incorrect.
  - Only configured for Environmental Lights drivers.
  - Requires all three selection sets to function (TurboTape / Leaders / Room Name).

![TurboDriver gif](/GIF/TurboDriver.gif)

## TurboBubble
TurboBubble is used to quickly place a switchleg bubble in the correct location relative to the block. TurboBubble operates using a long series of conditions established for CDL Toolbar blocks only. _TurboBubble will not work with foreign or complex custom blocks!_ After the switchleg is placed, you will have the option to flip the placement by pressing `f`. TurboBubble will scale the switchleg placement if used on a block that has been scaled using [TurboScale](#turboscale), but it will not scale the switchleg block itself. Use [TurboScale](#turboscale) on the switchleg block to establish the proper scale in the drawing.
- **Command:** `tb`
- **Limitations:**
  - Use for CDL Toolbar blocks only.
  - Due to the abundant variations of block sizes and configurations, TurboBubble will place the switchleg bubble but will not draw the connecting wire for 'ZC' blocks.

![TurboBubble gif](/GIF/TurboBubble.gif)

## TurboScale
TurboScale is used to scale objects to the correct size for the desired drawing scale. Once scaled, the block definition is saved and any blocks that are inserted or pulled from the toolbar will be scaled appropriately. When used on official [TurboTape](#turbotape) blocks or CDL Toolbar blocks with specific linear dimensions, TurboScale will only scale the text. TurboScale will record any rotation or attribute parameters and re-establish them after resetting the block.

- **Command:** `ts`
- **Limitations:**
  - Will only scale Blocks, Text, Leaders, and Dimensions. Any wiring, text boxes, etc. will need to be scaled manually.
  - Only use for setting objects to common architectural scales. Setting objects to custom scales will need to be done manually.
  - If used on a block, be aware that the block will be reset using `RESETBLOCK`.
  - If used on a block with Attributes, be aware that the attributes will be syncd using `ATTSYNC`.
  - **_Pulling scaled blocks from the toolbar will require a_** `RESETBLOCK`. **_New blocks placed using the_** `INSERT` **_command will not require a reset._**
  - Allow for ample time to cycle through all objects when used on a large selection set.

![TurboScale gif](/GIF/TurboScale.gif)

## TurboChange
TurboChange is used to quickly generate a new block definition from an existing block definition. TurboChange will also re-center attribute grips based on the new text sizes.
- **Command:** `tc`

![TurboChange gif](/GIF/TurboChange.gif)

## TurboName
TurboName is used to quickly build room names by clicking on the text objects in the xref. Follow the prompts closely to see which object the program is expecting. If certain objects are not provided in the xref, simply skip that object by pressing `space`. When the selected objects turn orange, the program is ready to place the text and is expecting a user-supplied insertion point.
- **Command:** `tn`
- **Limitations:**
  - Currently does not work with room names that are multiple lines or multiple objects.
  - Currently does not work with reference text that has excesive text formatting.

![TurboName gif](/GIF/TurboName.gif)

## Acknowledgements
https://lee-mac.com/
