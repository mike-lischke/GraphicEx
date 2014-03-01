unit Main;

interface

uses
  Windows, Messages, SysUtils, GraphicEx, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ExtDlgs, StdCtrls, JPEG, Menus, ComCtrls, ToolWin;

type
  TMainForm = class(TForm)
    OPD: TOpenPictureDialog;
    ControlBar1: TControlBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    SaveButton: TToolButton;
    StatusBar: TStatusBar;
    ToolBar2: TToolBar;
    FilterBox: TComboBox;
    WidthEdit: TEdit;
    WidthUpDown: TUpDown;
    HeightEdit: TEdit;
    HeightUpDown: TUpDown;
    ScaleButton: TToolButton;
    PopupMenu1: TPopupMenu;
    TruevisionTarga1: TMenuItem;
    SPD: TSavePictureDialog;
    ContextPopup: TPopupMenu;
    PropertyItem: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    WindowsBitmap1: TMenuItem;
    JPEGImage1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ToolButton1Click(Sender: TObject);
    procedure ScaleClick(Sender: TObject);
    procedure TruevisionTarga1Click(Sender: TObject);
    procedure WidthEditKeyPress(Sender: TObject; var Key: Char);
    procedure ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: String);
    procedure StatusBarResize(Sender: TObject);
    procedure PropertyItemClick(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
  private
    FProgressBar: TProgressBar;
    FUpDownUpdating: Boolean;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure DoLoad(const FileName: String);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI, Properties;

{$R *.DFM}                               

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoLoad(const FileName: String);

var
  Start: DWORD;
  GraphicClass: TGraphicExGraphicClass;
  Graphic: TGraphic;

begin
  Screen.Cursor := crHourGlass;
  try
    try
      Start := GetTickCount;
      // determine true file type from content rather than extension
      GraphicClass := FileFormatList.GraphicFromContent(FileName);
      if GraphicClass = nil then Image1.Picture.LoadFromFile(FileName)
                            else
      begin
        // GraphicFromContent always returns TGraphicExGraphicClass
        Graphic := GraphicClass.Create;
        Graphic.OnProgress := ImageLoadProgress;
        Graphic.LoadFromFile(FileName);
        Image1.Picture.Graphic := Graphic;
      end;
      Statusbar.Panels[0].Text := Format('%d x %d', [Image1.Picture.Width, Image1.Picture.Height]);
      Statusbar.Panels[1].Text := 'load time: ' + IntToStr(GetTickCount - Start) + 'ms';
      Statusbar.Panels[2].Text := FileName;
      FUpDownUpdating := True;
      WidthUpDown.Position := Image1.Picture.Width;
      HeightUpDown.Position := Image1.Picture.Height;
      FUpDownUpdating := False;
      PropertyItem.Enabled := True;
    except
      PropertyItem.Enabled := False;
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  OPD.Filter := FileFormatList.GetGraphicFilter([], fstBoth, [foCompact, foIncludeAll, foIncludeExtension], nil);
  FilterBox.ItemIndex := 5;
  DragAcceptFiles(Handle, True);

  if (ParamCount > 0) and FileExists(ParamStr(1)) then DoLoad(ParamStr(1));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);

var
  Buffer: array[0..MAX_PATH] of Char;
  Count: Cardinal;

begin
  Count := DragQueryFile(Msg.Drop, DWORD(-1), nil, 0);
  if Count > 0 then
  begin
    DragQueryFile(Msg.Drop, 0, Buffer, MAX_PATH);
    DoLoad(Buffer);
    DragFinish(Msg.Drop);
    Msg.Result := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton1Click(Sender: TObject);

begin
  if OPD.Execute then DoLoad(OPD.FileName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ScaleClick(Sender: TObject);

var
  Start: DWORD;
  NewX, NewY: Integer;

begin
  if not Image1.Picture.Bitmap.Empty then
  begin
    Screen.Cursor := crHourGlass;
    try
      Image1.Picture.Bitmap.PixelFormat := pf24Bit;
      Start := GetTickCount;
      if WidthUpDown.Position = 0 then NewX := Image1.Picture.Width
                                  else NewX := WidthUpDown.Position;
      if HeightUpDown.Position = 0 then NewY := Image1.Picture.Height
                                   else NewY := HeightUpDown.Position;
      Stretch(NewX, NewY, TResamplingFilter(FilterBox.ItemIndex), 0, Image1.Picture.Bitmap);
      Statusbar.Panels[1].Text := 'stretch time: ' + IntToStr(GetTickCount - Start) + 'ms';
      Statusbar.Panels[0].Text := Format('%d x %d', [Image1.Picture.Width, Image1.Picture.Height]);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TruevisionTarga1Click(Sender: TObject);

var
  Target: TTargaGraphic;

begin
  with SPD do
  begin
    Filter := FileFormatList.GetGraphicFilter([], fstBoth, [foCompact, foIncludeAll, foIncludeExtension],
                                              TTargaGraphic);
    if Execute then
    begin
      Target := TTargaGraphic.Create;
      try
        if Image1.Picture.Graphic is TBitmap then Target.Assign(Image1.Picture.Graphic)
                                             else
        begin
          Target.PixelFormat := pf24Bit;
          Target.Width := Image1.Picture.Width;
          Target.Height := Image1.Picture.Height;
          Target.Canvas.Draw(0, 0, Image1.Picture.Graphic);
        end;
        Target.SaveToFile(FileName);
      finally
        Target.Free;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WidthEditKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #13 then ScaleButton.Click;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: String);

var
  X: Integer;

begin
  with Statusbar do
  begin
    case Stage of
      psStarting:
        begin
          Panels[2].Bevel := pbNone;
          SizeGrip := False;
          FProgressBar := TProgressBar.Create(nil);
          FProgressBar.Parent := StatusBar;
          FProgressBar.Max := 100;
          Statusbar.Panels[1].Text := Msg;
          X := 4 + Panels[0].Width + Panels[1].Width;
          FProgressBar.SetBounds(X, 4, Panels[2].Width, Height - 6);
          FProgressBar.Show;
          Application.ProcessMessages;
        end;
      psEnding:
        begin
          FProgressBar.Free;
          FProgressBar := nil;
          Panels[2].Bevel := pbLowered;
          SizeGrip := True;
        end;
      psRunning:
        begin
          FProgressBar.Position := PercentDone;
          FProgressBar.Update;
          Application.ProcessMessages;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarResize(Sender: TObject);

begin
  with StatusBar do
  begin
    StatusBar.Panels[2].Width := Width - Panels[0].Width - Panels[1].Width - 8;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PropertyItemClick(Sender: TObject);

begin
  PropertyDialog.Graphic := TGraphicExGraphic(Image1.Picture.Graphic);
  PropertyDialog.ShowModal;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);

var
  OldValue: SmallInt;

begin
  if not FUpDownUpdating then
  begin
    FUpDownUpdating := True; // recursion stop
    OldValue := (Sender as TUpDown).Position;
    if OldValue = 0 then OldValue := 1;

    if Sender = WidthUpDown then
    begin
      HeightUpDown.Position := Round(HeightUpDown.Position * NewValue / OldValue);
    end
    else
    begin
      WidthUpDown.Position := Round(WidthUpDown.Position * NewValue / OldValue);
    end;
    FUpDownUpdating := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.


