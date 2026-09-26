
##  Chart1.Legend.Centered property demo

There are 3 different ways to position the chart legend, both in horizontal and vertical alignments.

### Automatic

```delphi
Chart1.Legend.Centered := lcAutomatic
```
This is the current behaviour, the Legend TopLeftPos property is used as a margin, no centering happens.

<img width="841" height="709" alt="image" src="https://github.com/user-attachments/assets/9909d188-3d76-4760-8088-f0169c619c30" />


### Panel

```delphi
Chart1.Legend.Centered := lcPanel
```

The legend will always be centered (horizontally or vertically) using the whole chart panel width or height.

<img width="973" height="599" alt="image" src="https://github.com/user-attachments/assets/e6b0ff04-e175-4efe-92ea-f462fea30711" />


### Chart

```delphi
Chart1.Legend.Centered := lcChart
```

The legend will always be centered (horizontally or vertically) using the chart rectangle width or height.

<img width="1031" height="599" alt="image" src="https://github.com/user-attachments/assets/92785cc5-44dc-4215-a92b-0007e1f324e9" />


### Notes

Legend.CustomPosition is only used when Centered is Automatic.

This demo uses Chart1 MarginLeft and MarginTop to move the chart rectangle origin to show how lcChart centering happens.







