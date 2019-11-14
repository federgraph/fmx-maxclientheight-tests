## Using WorkAreaHeight for MaxClientHeight

I wanted to precompute the max possible value of ClientHeight, based on Screen.WorkAreaHeight,
and I have tested on a normal HD-Screen and on a Surface tablet screen,
and I have been surprised that
- on the Surface screen WorkAreaHeight is always the same, no matter if TaskBar is configured to automatically disappear or not.
- Screen.WorkAreaHeight is given in real pixels,
- whereas Height and ClientHeight are given in scaled pixels.

Normal Desktop HD Screen Report:
```
Handle.Scale = 1.0
Screen.WorkAreaHeight = 1040 = wah
MaxClientHeight = 1001
mch = MaxClientHeight / scale = 1001

Screen-W-H = (1920, 1080)
(Form)-W-H = ( 616,  639)
Client-W-H = ( 600,  600)

MaxClientHeight :=  wah - Round(scale *(h - ch));
           1001 := 1040 - Round(1.0 *(639 - 600));

```

Surface-Pro Screen Report:
```
Handle.Scale = 2.0
Screen.WorkAreaHeight = 1824
MaxClientHeight = 876

Screen-W-H = (2736, 1824)
(Form)-W-H = ( 613,  636)
Client-W-H = ( 600,  600)
mch = MaxClientHeight / scale = 876

MaxClientHeight :=  wah - Round(scale *(h - ch));
           1752 := 1824 - Round(2.0 *(636 - 600));
```

From (1680 x 1050) Monitor screen, rotated:
```
Handle.Scale = 1.0
Screen.WorkAreaHeight = 1640
MaxClientHeight = 1601

Screen-W-H = (1050, 1680)
(Form)-W-H = (1070, 1700)
Client-W-H = (1054, 1661)
```
Notice that I have resized the Window manually to the max, it is 20 pixels larger in both directions as the screen itself. It always seems to be 20 pixels.

- What do you think about Screen.WorkAreaHeight?
- Is this the best thing to use?
- How should I compute the maximum of ClientHeight that can be visible on a given screen?

### Note 1

Found out that, if you change the rotation of the form on the Surface tablet, it does not update.
If you change the taskbar mode, auto hiding or not, it does not update either.
Any change is only reflected after restart of app.