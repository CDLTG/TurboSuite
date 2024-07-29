# Turbo Suite
Turbo Suite is a collection of AutoLISP routines developed to automate repetitive AutoCAD tasks.

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
TurboTape is comprised of five subroutines which allow the user to construct any object that requires a linear quantity. While this can also be achieved using dynamic blocks, TurboTape names each block independently for use in conjunction with the counts sheet. During creation, you will be prompted for a `Type` and a `Tag`

  - ### TurboTape (Horizontal)
     - **Commands:** `tt;` `tt;h`
     - **Function:** Draw horizontal linear tape of any kind.
     - **Limitations:**
       - Requires a polyline as the base object.
       - Requires a new base polyline if lengths need to be adjusted.
       - Circles must be converted into polylines with arcs.
  
      ![TurboTape Horizontal gif](/GIF/TurboTape(Horizontal).gif)

  - ### TurboTape (Vertical)
     - **Command:** `tt;v`
     - **Function:** Draw vertical linear tape of any kind. TurboTape Vertical can be called as 1,2,3 or 4 runs depending on size and available drafting space.
     - **Limitations:**
       - Requires manual input of tape lengths; follow prompts closely.
       - Once created, the new block will need to be moved into place manually.
       - Works best when using reference lines from an xref.

      ![TurboTape Vertical gif](/GIF/TurboTape(Vertical).gif)

  - ### TurboTape (Fixture)
     - **Command:** `tt;f`
     - **Function:** Draw linear horizontal fixtures with a width dimension.
     - **Limitations:**
       - limit 1
       - limit 2
       - limit 3

      ![TurboTape Fixture gif](/GIF/TurboTape(Fixture).gif)

  - ### TurboTape (Custom)
     - **Command:** `tt;c`
     - **Function:** Draw allowance for a custom linear tape length.
     - **Limitations:**
       - limit 1
       - limit 2
       - limit 3

      ![TurboTape Fixture gif](/GIF/TurboTape(Custom).gif)

  - ### TurboTape (Array)
     - **Command:** `tt;a`
     - **Function:** Draw an array of linear tape, primarily used for stairs.
     - **Limitations:**
       - limit 1
       - limit 2
       - limit 3

      ![TurboTape Array gif](/GIF/TurboTape(Array).gif)

## TurboDriver
TurboDriver description
- **Command:** `td`
- **Function:** Draw an array of linear tape, primarily used for stairs.
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
