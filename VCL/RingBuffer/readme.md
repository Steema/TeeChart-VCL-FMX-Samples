## TeeChart Ring Buffer Series

This example uses a "Ring Buffer" structure to display data as fast as possible, as lines or points.

The ring ([circular](https://en.wikipedia.org/wiki/Circular_buffer)) buffer is an array you can specify its type and length, and then just keep adding points to the buffer.
When the buffer is full, points will be added from the start of the buffer again.

Horizontal scrolling is done by code as a speed optimization.

---


<img width="470" height="355" alt="image" src="https://github.com/user-attachments/assets/a2a22157-13f8-44f8-b366-24e35ae9d10c" />
<img width="470" height="355" alt="image" src="https://github.com/user-attachments/assets/27d21e7c-bd79-449f-9383-9eee53d87269" />


```delphi

// Creating the line series:
 var Line := TRingBuffer<Single>.Create(Self);
     Line.Buffer.Resize(1000);
     Line.ParentChart := Chart1;

// Adding new points to the series:
     Line.Buffer.Append(123);
     Line.Buffer.Append(456);
     ...
```

**Notes:**

OpenGL canvas is the fastest way in Windows and VCL to paint many lines and points.
Skia canvas is faster than GDI+.
Old legacy GDI is not that slow, but lacks antialias so lines become "jaggy".
See MainUnit.pas source to enable those canvases.
Firemonkey (specially 64bit with RAD 13.0 and up) is faster than VCL.

Example for [Lazarus FreePascal compiler](https://github.com/Steema/TeeChart-VCL-FMX-Samples/tree/main/VCL/RingBuffer/Lazarus)

Example for [Firemonkey FMX](https://github.com/Steema/TeeChart-VCL-FMX-Samples/tree/main/FMX/RingBuffer) (all platforms)
