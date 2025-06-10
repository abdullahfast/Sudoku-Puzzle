# Sudoku-Puzzle

# 🧩 Sudoku Game in x86 Assembly (8086)

This project is a fully functional **Sudoku Game** implemented in **x86 Assembly (8086 architecture)**. Designed for the DOS environment, it provides an interactive console-based Sudoku experience, complete with difficulty levels, music, visual interface, score tracking, undo support, and real-time timing.

---

## 🎮 Features

- **Interactive Gameplay**
  - Move selection using `W`, `A`, `S`, `D`
  - Enter numbers with keyboard (1–9)
  - Toggle *Notes Mode* with `N`
  - Undo moves with `Z`
  - Switch between grids using arrow keys

- **Game UI**
  - Start, Help, Difficulty, and End screens
  - Real-time score and mistake tracking
  - Timer display
  - Win/Lose logic and visual feedback
  - Dynamic rendering using BIOS video services

- **Technical Highlights**
  - Grid rendering using extended ASCII characters
  - Dual grid pages for full board navigation
  - Music playback using an IMF (AdLib) binary
  - Custom random number generator
  - Stack-based undo system for game state

---

## 🧰 Technologies Used

- **Language:** x86 Assembly (TASM/MASM-compatible)
- **Architecture:** 16-bit Real Mode
- **Platform:** DOS / DOSBox / Emulators
- **Graphics:** BIOS Video Interrupts, Text Mode (80x25)
- **Audio:** IMF music playback using AdLib-compatible ports

---

Controls:

- W, A, S, D → Move selection

- 1–9 → Enter number

- N → Toggle Notes Mode

- Z → Undo move

- ↑ ↓ → Switch between grid pages

- ESC → Exit game

