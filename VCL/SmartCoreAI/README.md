## TeeChart and SmartCore AI components

This example shows how to use the [Embarcadero SmartCore AI](https://docwiki.embarcadero.com/RADStudio/Florence/en/SmartCore_AI_Component_Pack) components and charting the results using TeeChart.

Components used:

- TAIConnection
- TAIChatAgent

```delphi
  AIChatAgent1.Instructions.Text :=
    'You are a careful business data analyst. Reply first with one valid JSON array of metric objects, no markdown fences.
     Each object has metric (short name), value (number), unit (short unit), notes (short explanation).
     Example: [{"metric":"Revenue","value":1250.5,"unit":"USD","notes":"Q1 estimate"}].
     Then add a short plain-English explanation after the array.
     Use numerical values grounded in the user request; when data is missing,
     clearly state assumptions and use an illustrative estimate.';

```

<img width="1282" height="1118" alt="image" src="https://github.com/user-attachments/assets/d8cea48e-3b2b-454a-93a8-7b1748618dad" />
