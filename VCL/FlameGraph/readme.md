## TeeChart TFlameSeries

### Usage:

```pascal
uses TeeFlameSeries;  // Firemonkey: FMXTee.Series.Flame

var
  Flame1:=TFlameSeries.Create(Self);
  Flame1.ParentChart:=Chart1;

// Add data:

var tmp1, tmp2, tmp3 : Integer;
begin
  tmp1:=Flame1.Add(-1,'TApplication.Run',1290);

    tmp2:=Flame1.Add(tmp1,'TApplication.HandleMessage',870);
           tmp3:=Flame1.Add(tmp2,'TApplication.Idle',423);
            Flame1.Add(tmp3,'TApplication.DoMouseIdle',423);
           Flame1.Add(tmp2,'TApplication.ProcessMessage',447);

    tmp2:=Flame1.Add(tmp1,'TCustomForm.SetVisible',320);
           tmp3:=Flame1.Add(tmp2,'TControl.SetVisible',320);
            Flame1.Add(tmp3,'TControl.Perform',300);
            Flame1.Add(tmp3,'TCustomForm.RequestAlign',20);

    tmp2:=Flame1.Add(tmp1,'TObject.Free',100);
           Flame1.Add(tmp2,'TApplication.Destroy',90);
           Flame1.Add(tmp2,'TCustomForm.Destroy',10);

```


<img width="1685" height="790" alt="image" src="https://github.com/user-attachments/assets/0facd71d-505f-4dfd-9855-0a24e95f022d" />
