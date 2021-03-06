This readme file corresponds to the *Initial Test App* commit.

# Initial Test App

When the app is running you hit keyboard keys to control it.

At minimum this will allow you to run 3 different tests.

## A B Test

Well, you hit key a, then key b, and then repeat, toggle between the two states.
The problem will show.

When you hit a key, the test app will use hardcoded values to set ClientWidth and ClientHeight.
These value are meant to be bigger then the Height of the Screen.

## E Test

- Hit key e,
- then you drag the right border of the window to change the width,
- repeat.

The problem or a slightly different version of the problem will show.

## W N Test

Toggle between Wide and Narrow mode using keys.

When you hit a key, only ClientWidth is changed.
ClientHeight is not touched.

But you can still run into a problem, when there is a MainMenu which wraps in narrow mode, and unwraps in wide mode.

Note that key **m** will create or destroy a TMainMenu component, for testing purposes.

## Square Tests

Again, you hit a keyboard key, 0, 1, 6, 7, 8, 9.

```pascal
  else if KeyChar = '9' then
  begin
    ClientWidth := 900;
    ClientHeight := 900;
  end
```

This should yield a square ClientArea, usually without problems.

But if it gets too big, then there might be a problem.

- On a normal HD-Screen (1920, 1080) you can have a ClientHeight of 900 pixels.
- But on a Surface tablet ClientHeight assignment of 900 may already be too big.