unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList;

type
  TMainForm = class(TForm)
    TreeView: TTreeView;
    ListView: TListView;
    Image: TImage;
    edDir: TEdit;
    btnChooseDir: TBitBtn;
    edImagePath: TEdit;
    ImageList1: TImageList;
    cbOnlyHandledExtensions: TCheckBox;
    memoErr: TMemo;
    cbEnableExtension: TCheckBox;
    procedure btnChooseDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure cbOnlyHandledExtensionsClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure cbEnableExtensionClick(Sender: TObject);
  private
    FRootDir: string;
    FFileDir: string;
    FImagePath: string;
    function nodePath(ANode: TTreeNode): string;
    procedure FillNode(Parent: TTreeNode; path: string);
    procedure FillDirectoryTree(rootDir: string);
    procedure FillFileList(dir: string);
    procedure LoadImage(path: string);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  proj_common, IniFiles, GraphicEx, Math;

{$R *.dfm}

procedure TMainForm.btnChooseDirClick(Sender: TObject);
begin
  if SelectDirectory('Select folder to browse', edDir.Text, '', False, FRootDir) then
  begin
    edDir.Text := FRootDir;
    FillDirectoryTree(edDir.Text);
  end;
end;


procedure TMainForm.ReadIniSettings;
var
  IniPath: string;
  iniFile: TIniFile;
begin
  IniPath:=ChangeFileExt(AppPath,'.ini');
  iniFile := TIniFile.Create(IniPath);
  edDir.Text:=iniFile.ReadString('Paths','RootDir','');
end;

procedure TMainForm.WriteIniSettings;
var
  IniPath: string;
  iniFile: TIniFile;
begin
  IniPath:=ChangeFileExt(AppPath,'.ini');
  iniFile := TIniFile.Create(IniPath);
  iniFile.WriteString('Paths','RootDir', edDir.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReadIniSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIniSettings;
end;

procedure TMainForm.FillNode(Parent: TTreeNode; path: string);
var
  SR: TSearchRec;
  node: TTreeNode;
  countDir: integer;
begin
  countDir:=0;
  if FindFirst(IncludeTrailingPathDelimiter(path) + '*.*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory <>0)and(SR.Name<>'.')and(SR.Name<>'..') then
      begin
        node:=TreeView.Items.AddChild(Parent, SR.Name);
        node.ImageIndex:=0;
        node.HasChildren:=true;
        inc(countDir);
      end;
    until FindNext(SR) <> 0;
    FindCLose(SR);
  end;
  if countDir=0 then
    Parent.HasChildren:=false;
end;

procedure TMainForm.FillDirectoryTree(rootDir: string);
begin
  TreeView.Items.Clear;
  FillNode(nil, FRootDir);
end;

function TMainForm.nodePath(ANode: TTreeNode): string;
var
  node: TTreeNode;
begin
  node:=ANode;
  result:='';
  while node<>nil do
  begin
    result:=node.Text+PathDelim+result;
    node:=node.Parent;
  end;
  result:=IncludeTrailingPathDelimiter(FRootDir)+result;
end;

procedure TMainForm.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  FillNode(Node, nodePath(Node));
end;

procedure TMainForm.FillFileList(dir: string);
var
  SR: TSearchRec;
  Extensions: TStringList;
  function filter: boolean;
  var
    n: integer;
    Ext: string;
  begin
    if cbOnlyHandledExtensions.Checked then
    begin
      Ext := ExtractFileExt(SR.Name);
      result:= Extensions.Find(Ext, n);
    end else
     result:=true;
  end;
var
  item: TListItem;
  i: integer;
begin
  ListView.OnChange:=nil;
  FFileDir := dir;
  ListView.Items.Clear;
  Extensions := TStringList.Create;
  FileFormatList.GetExtensionList(Extensions);
  for i := 0 to Extensions.Count - 1 do
    Extensions[i] := '.' + UpperCase(Extensions[i]);
  Extensions.Sort;
  if FindFirst(IncludeTrailingPathDelimiter(dir) + '*.*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory = 0)and filter() then
      begin
        item:=ListView.Items.Add;
        item.Caption:=SR.Name;
        item.SubItems.Add(IntToStr(SR.Size));
       end;
    until FindNext(SR) <> 0;
    FindCLose(SR);
  end;
  Extensions.Free;
  ListView.OnChange:=ListViewChange;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FillFileList(nodePath(Node));
end;

procedure TMainForm.cbOnlyHandledExtensionsClick(Sender: TObject);
begin
  FillFileList(FFileDir);
end;

procedure TMainForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if FFileDir+Item.Caption=FImagePath then exit;
  FImagePath:=FFileDir+Item.Caption;
  LoadImage(FImagePath);
end;

procedure TMainForm.LoadImage(path: string);
var
  Picture: TPicture;
  scale, scaleX, scaleY: real;
  R: TRect;
  W,H: integer;
begin
  Image.Canvas.Brush.Color:=clWhite;
  Image.Canvas.Pen.Color:=clBlack;
  Image.Canvas.FillRect(Image.ClientRect);
  Picture := TPicture.Create;
  try
   try
     Picture.LoadFromFile(path);
     scaleX:=Image.Width/Picture.Width;
     scaleY:=Image.Height/Picture.Height;
     scale:=Min(scaleX, scaleY);
     if not cbEnableExtension.Checked then
       scale:=min(scale,1);
     W := round(Picture.Width*Scale);
     H := round(Picture.Height*Scale);
     R.Left:=(Image.Width-W) div 2;
     R.Top:= (Image.Height-H) div 2;
     R.Right:= R.Left+W;
     R.Bottom:=R.Top+H;
     except
       on E: EInvalidGraphic do memoErr.Lines.Add(E.Message);
       on E: EReadError do memoErr.Lines.Add(E.Message);
       on E: EFOpenError do memoErr.Lines.Add(E.Message);
     end;
     Image.Canvas.StretchDraw(R, Picture.Graphic);
  finally
    Picture.Free;
  end;
end;

procedure TMainForm.cbEnableExtensionClick(Sender: TObject);
begin
  LoadImage(FImagePath);
end;

end.
