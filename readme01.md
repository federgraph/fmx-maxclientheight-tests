From post to the Forum.

## ClientArea painting problem when MainMenu wraps/unwraps

The goal:
- Maximize ClientHeight, but keep in control of aspect ratio of ClientArea.
- Height should be Screen.WorkAreaHeight.
- Width should be either Portrait (smaller, 800) or Landscape (larger, 1000).
- Need to set Width/Height or ClientWidth/ClientHeight in code.
- Prefer to set ClientWidth/ClientHeight over Width/Height.

The plot thickens:
- Have big TMainMenu component.
- In Portrait MainMenu will wrap into two lines!
- In Landscape MainMenu will fit on one line!

The Problem:
- ClientArea painting problem when MainMenu wraps/unwraps,
  when you toggle between Landscape and Portrait:

Odd:
- MainMenu.Height is not a property.
- Changing MainMenu-Height seems to be part of the problem.
- Missing space to grow the window is another part of the problem?

Usage:
- Press buttons in order given below...

### Problem cases

a) Reset, Portrait, Landscape:
- Black (unpainted) area at the bottom edge.

b) Reset, Button2, Button1:
- Black unpainted area at bottom edge of ClientArea in Window.
- Button1 and Button2 not painted correctly, around bottom edge.

c) Reset, Landscape, Button1, Landscape:
- Window too big, bottom edge behind task bar.

### God cases

d) Reset, Portrait

e) Reset, Landscape

f) Reset, Button 1 (like Portrait but via using Height property)

g) Reset, Button 2 (like Landscape but via using Height property)

Steps:
- In new, empty FMX project
- rename Form to FormMain
- paste test code (from forum post)
- connect FormCreate, FormDestroy, FormKeyUp
- run
- hit keys instead of clicking buttons.
