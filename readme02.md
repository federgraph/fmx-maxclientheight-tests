Just some notes, may be wrong (test case errors).

> Changing Window Size manually (with mouse) works, programmatically changing does not, when MainMenu wraps/unwraps.

## Wide and Narrow

I added Wide and Narrow commands, used to toggle ClientWidth only, so that the Menu will wrap/unwrap.

When I toggle programmatically,
- ClientHeight stays the same
- Content of Window is squeezed (scaled down) when in narrow situation.

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

Updated problem description:
- *Scaling* may be wrong under certain conditions.
- This is sometimes related to MainMenu wrapping and automatic resizing!
- I understand that Delphi is not drawing the MainMenu.
- Delphi does not know whether the Menu will wrap or not?
- How do we query the current height of the MainMenu?
- The point in time when Delphi queries Height/ClientHeight is too early?
