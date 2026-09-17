<div align="center">

# LogicalCircuit

![OCaml](https://img.shields.io/badge/OCaml-5.3-EC6608?logo=ocaml&logoColor=white)
![dune](https://img.shields.io/badge/dune-3.21-blue)
![GTK](https://img.shields.io/badge/GTK-3-4A90D9?logo=gtk&logoColor=white)
![lablgtk3](https://img.shields.io/badge/lablgtk3-3.1-informational)
![cairo2](https://img.shields.io/badge/cairo2-0.6-lightgrey)
![Platform](https://img.shields.io/badge/platform-Linux-yellow?logo=linux&logoColor=white)

A combinational logic circuit simulator with a native desktop editor, written in OCaml.

Place `AND`, `OR` and `NOT` gates and inputs on a dot grid, wire them together, click an
input to toggle it, and watch every wire carry its value live — then read the truth table
of the circuit you built.

**This is a learning project.** Its only goal is practice: OCaml itself, plus dune,
lablgtk3 and cairo2. It was written as a practice project with the help of AI, and the
entire GUI is native OCaml — no web technologies involved.

</div>

## What's inside

- **`core/` — the logic, with no UI knowledge at all**
  - `gate.ml` — the three gates (`And`, `Or`, `Not`) and the single source of truth for
    their semantics, including three-valued evaluation (`Low` / `High` / `Unknown`).
  - `circuit.ml` — a directed acyclic graph of inputs and gates with fan-out: wires,
    cycle detection, and evaluation in one O(V+E) pass.
  - `truth_table.ml` — enumerates every input combination and formats the table.
- **`ui/` — the editor** — a GTK window: menu bar, toolbar, dot-grid canvas, status bar.
- **`bin/` — a small CLI demo** — a three-input majority voter, plus the failure cases a
  circuit can run into.

Two details worth mentioning:

- **Partial evaluation.** A circuit still under construction is evaluated anyway: an
  unwired port drives `Unknown`, and a gate with a decisive input still resolves (an
  `AND` with a wired low is low, an `OR` with a wired high is high).
- **Feedback is rejected.** A cycle makes the simulation report the nodes on the cycle,
  and the editor paints the offending wires red.

## Build and run on Linux

Everything runs natively; nothing is web-based.

| Tool | Used for | Debian/Ubuntu package |
|------|----------|-----------------------|
| OCaml (≥ 5) | the compiler | `ocaml` |
| findlib | library lookup | `ocaml-findlib` |
| GTK 3 development files | the windowing toolkit | `libgtk-3-dev` |
| lablgtk3 | OCaml bindings to GTK 3 | `liblablgtk3-ocaml-dev` |
| cairo2 | OCaml bindings to Cairo, for drawing | `libcairo2-ocaml-dev` |

One line installs all of them:

```bash
sudo apt install ocaml ocaml-findlib libgtk-3-dev liblablgtk3-ocaml-dev libcairo2-ocaml-dev
```

You also need **dune** (≥ 3.21): install it with your package manager if it packages it
(`sudo apt install dune`), otherwise through [opam](https://opam.ocaml.org) or a
[release binary](https://github.com/ocaml/dune/releases).

Then, from the project directory:

```bash
cd logical_circuit

dune build                     # compile everything

dune exec ./ui/ui_main.exe     # the graphical editor — needs a graphical session
dune exec ./bin/main.exe       # the CLI demo (majority voter)
```

A test suite lives in `test/`, which is deliberately kept out of the repository; run it
locally with `dune test --force` when it is present.

## Using the editor

- Pick a gate from the toolbar (`AND` / `OR` / `NOT` / `Input`), then click the grid to
  drop it; the tool then switches back to `Select` automatically.
- Drag a node with the mouse to move it — positions snap to the grid.
- Click an `Input` circle to toggle it between 0 and 1; the whole circuit recalculates
  instantly.
- Drag from an output port to wire two nodes: the loose end settles on the nearest dot
  as you move, and snaps onto the input port under the pointer, which lights up. The
  wire is politely refused if the port is already taken.
- Wires route themselves as clean right angles between the ports. To lay one out
  yourself, drag any segment of it: the segment follows the mouse, its bends land on the
  dots, and the ends stay on the ports.
- Click a wire to select it (its bends show as handles), then press `Delete` (or
  right-click) to remove it. Right-click a node to delete it together with its wires.
- `Escape` cancels a wire drag and returns to `Select`.

Wire colours show the value they carry: **green** = 1, **grey** = 0, **amber** = unknown
(nothing wired there yet), **red** = part of a feedback cycle.

Menu: `File → Quit` (Ctrl+Q), `View → Truth table` (Ctrl+T), `Help → Keys` (Ctrl+K).

---

Written for learning. Corrections and suggestions are welcome.
