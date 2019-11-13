> Noted while testing, and posted to the Forum, 06.11.2019.

Program changes:
- Added OnPaint handler, in which I record LastPaintRect: TRectF.
- I handle KeyChar i, calling InvalidateRect(self.ClientRect);
- I print LastPaintRect in the Textual Report.
- I update the Report in OnResize

Now focusing on this test case:
1. change window width with mouse and watch MainMenu and ClientHeight
2. stop when MainMenu does not wrap (it is wide enough)
3. press key n to set ClientWidth to narrow
4. press space bar or key i to update Report and show LastPaintRect
5. evaluate situation

Observation:
- Height stays the same.
- ClientHeight changes when dragging the Width.
- ClientHeight does NOT change when setting ClientWidth in code (using KeyChar w and n)
- PaintRect is in sync with ClientWidth/ClientHeight.

Expected:
- Window.Height should not change
- MainMenu wraps and uses more vertical space
- ClientHeight should change
- Content should update and adapt to new ClientHeight.

Actual:
- Window.Height does not change, ok
- MainMenu wraps and uses more space, ok
- ClientHeight property value remains the same, wrong! ( = PerceivedClientHeight)
- *PerceivedClientHeight* is too big in this case.
- Content is drawn y-distorted, condensed.
- Scaling factor seems to be ActualClientHeight/PerceivedClientHeight.
- Black unpainted strip appears

### Discussion

Delphi sometimes gets behind the reality in terms of what it thinks ClientHeight is right now, and then the painting gets distorted.

I have a situation in my real App, where I drop Images to a DropTarget and then set ClientWidth and ClientHeight to match the Image size. Sometimes I need to drag the same Image up to three times to get good result.

Again, this only happens when MainMenu.Height (not a property) changes.