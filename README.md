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
    Draw linear horizontal fixtures with a `Width` dimension. This value will default to `2`, but can be given any desired real value.
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

      ![TurboTape Fixture gif](/GIF/TurboTape(Custom).gif)

  - ### TurboTape (Array)
    Draw an array of linear tape, primarily used for stairs.
     - **Command:** `tt;a`
     - **Limitations:**
       - Requires a polyline as the base object.
       - **_Do not use for stairs of different lengths!_**
       - limit 3

      ![TurboTape Array gif](/GIF/TurboTape(Array).gif)

## TurboDriver
TurboDriver description
- **Command:** `td`
- **Limitations:**
  - limit 1
  - limit 2
  - limit 3

![TurboDriver gif](/GIF/TurboDriver.gif)

## TurboBubble
TurboBubble description
- **Command:** `tb`
![TurboBubble gif](/GIF/TurboBubble.gif)

## TurboScale
TurboScale description
- **Command:** `ts`
![TurboScale gif](/GIF/TurboScale.gif)

## TurboChange
TurboChange description
- **Command:** `tc`
![TurboChange gif](/GIF/TurboChange.gif)

## TurboName
TurboName description
- **Command:** `tn`
![TurboName gif](/GIF/TurboName.gif)
