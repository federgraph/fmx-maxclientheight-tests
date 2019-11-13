Just some notes, may be wrong (test case error).

Changing Window Size manually (with mouse) works, programmatically changing does not, when MainMenu wraps/unwraps.

I added Wide and Narrow commands, used to toggle ClientWidth only, so that the Menu will wrap/unwrap.

When I toggle programmatically,
- ClientHeight stays the same
- Content of window is squeezed (scaled down) when in narrow situation.

If I drag with mouse to make Window wider then press Narrow key (n)
- Black unpainted area at bottom edge of Window appears
- Content is wrongly scaled (scaled down)

If I change Window size with mouse to some other values for Width and Height,
then press Reset, Wide, Narrow keys in order
- Height changes
- ClientHeight stays unchanged
- black unpainted area does NOT appear
- but content is wrongly scaled.

The Height of the Window may change or not, depending on history!

When I resize the Window by dragging the corner, correct scaling is restored!

Drawing  seems to operate on false assumption of actual ClientHeight!

I can see that the  content is distorted by looking at the debug-output-text.
I have also seen Buttons squeezed (when I still had buttons on the test form.)

Workaround:
I have so far worked around the problems except for one remaining:

If I start up my real application with a narrower ClientWidth so that MainMenu wraps around,

the content of the Window is initially scaled too big in y-direction,
so that part of the Content is clipped at the bottom of the Window.

I made sure the App starts wide enough to not wrap the MainMenu, but this does not count as a workaround.

Note that I do set the size of a layout component manually in code (optimizing resize count) and I need the reported ClientHeight to be correct.

Updated problem description:
- Scaling may be wrong under certain conditions.
- This is related to MainMenu wrapping and automatic resizing!
- I understand that Delphi is not drawing the MainMenu.
- Delphi does not know whether the Menu will wrap or not?
- How do we query the current height of the MainMenu?
- The point in time when Delphi queries Height/ClientHeight is too early?
