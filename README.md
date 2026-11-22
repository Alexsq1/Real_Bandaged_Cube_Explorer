# Real Bandaged Rubik's Cube Solver
This project is a general solver for Bandaged Rubik's Cubes. It finds optimal solutions for any cube with any block configuration (if exists).
Based on Korf's algorithm of a 3x3x3 solver.
It was part of a bachelor thesis in computer engineering (ingeniería informática).

## Project dependencies:
- Haskell
- Cabal (3.16.1.0 recommended)
- Python 3 (only for video generation)
- Python packages: manim and manim-rubikscube
- (GHC version recommended is 9.12.2. Newer versions can slow down the execution time.)

## How to use:
- Install all the dependencies: 
- Haskell: ghcup recommended ([link](https://www.haskell.org/ghcup/))
```bash
ghcup install ghc 9.12.2
ghcup install cabal 3.16.1.0
```

- Python: pip is recommended. It can have other dependencies (cairo, pango, ffmpeg). 
It is optional, you can comment the last line of src-exe/ZExamples/InputAndSolve.hs for avoiding video generation.
```bash
pip install manim
pip install manim-rubikscube
```

- Clone the repo, compile and run:
``` bash
git clone git@github.com:Alexsq1/Real_Bandaged_Cube_Explorer.git
cd Real_Bandaged_Cube_Explorer
cabal run
```

- Usage:

- Pattern databases must be generated for the first time.
- [Optional]: Insert aliases for each face (suggestion: initial letter of the colour).
- [Optional]: Insert a colour per face in hexadecimal, defaults to white on top, green on front.
- Insert the colour of each sticker (ignoring the blocks) in this order:
``` text
               ______________
               |            |
               | U1  U2  U3 |
               |            |
               | U4  U5  U6 |
               |            |
               | U7  U8  U9 |
  _____________|____________|__________________________
  |            |            |            |            |
  | L1  L2  L3 | F1  F2  F3 | R1  R2  R3 | B1  B2  B3 |
  |            |            |            |            |
  | L4  L5  L6 | F4  F5  F6 | R4  R5  R6 | B4  B5  B6 |
  |            |            |            |            |
  | L7  L8  L9 | F7  F8  F9 | R7  R8  R9 | B7  B8  B9 |
  |____________|____________|____________|____________|
               |            |
               | D1  D2  D3 |
               |            |
               | D4  D5  D6 |
               |            |
               | D7  D8  D9 |
               |____________|
```

- Insert the blocks: insert each piece separated by '+', and each colour of the piece by '-'.
For example, white center + white-orange edge + white-orange-blue corner is encoded as "w + w-o + w-o-b".
- Note: the program processes the blocks: duplicates are eliminated, and per each block,
the minimum parallelepiped that contains all pieces is computed.

## Project structure (simplified):
``` text
├── Bandaged-Cube-TFG.cabal
├── doc : files of documentation of the project
|
├── media : generated videos will be stored here
|
├── scripts : automation of certain development tasks (profiling, generating documentation, testing...)
|
├── src-exe : implementation
│   ├── BuildCubes : generate certain cubes
│   │   ├── CubeCreator.hs
│   │   └── ExpandBlocks.hs
│   ├── Data : representation of the information
│   │   ├── Bandaged.hs
│   │   ├── Cube.hs
│   │   ├── MathematicalNotation.hs
│   │   └── Moves.hs
│   ├── Heuristics
│   │   ├── CalculateKorfHeuristics.hs
│   │   ├── Combinatorics.hs
│   │   ├── IndexHeuristics.hs
│   │   ├── KorfHeuristic.hs
│   │   ├── LoadKorfHeuristics.hs
│   │   ├── pdb : the pattern databases of the heuristics will be stored here!
│   │   └── StoreKorfHeuristics.hs
│   ├── IO_Visualizator : Input-output and video generation
│   │   ├── InputCube.hs
│   │   ├── manim_cube_visualizator.py
│   │   ├── ManimHsConversion.hs
│   │   └── Visualizator.hs
│   ├── Main.hs
│   ├── Search_Engine : algorithm that searches a solution
│   │   ├── MoveGeneration.hs
│   │   ├── Search.hs
│   │   ├── Solvability.hs
│   │   └── SolvingStrategies.hs
│   └── ZExamples
│       └── InputAndSolve.hs
|
├── test
|
└── TFG : files of the documents presented at bachelor thesis (in Spanish)
    ├── Defensa_TFG.pdf
    ├── Memoria_TFG_Corregida.pdf
    └── Memoria_TFG_Entrega.pdf
```

## Run tests: 
This project is tested with property-based testing.
``` bash
bash scripts/test.sh
```

## Algorithm:
This project adapts Korf's algorithm from a 3x3x3 solver, but only generate moves that do not break any block.
The search is done by an IDA* (iterative-deepening A*), where the heuristics
are precomputed heuristics in pattern-databases (sometimes called pruning tables in other projects). 
They contain an admissible number of moves to solve the corners, or each half of the edges (the maximum is selected).
They are strategically indexed with Lehmer codes and numbers in base 2 or 3 for fast accesses.

## Maximum average branching factor of cubes where some layers are blocked:

| Turnable layers | Average branching factor |
| --- | --- | 
| < R, U > | 3 | 
| < R, L, U > | 4.85 |
| < R, U, F > | 6 |
| < R, L, U, F > | 8.19 |
| < R, L, U, D, F, B > | 13.34 |
