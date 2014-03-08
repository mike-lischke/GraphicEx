unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList;

type
  TMainForm = class(TForm)
    TreeView: TTreeView;
    ListView: TListView;
    Image1: TImage;
    edDir: TEdit;
    btnChooseDir: TBitBtn;
    edImagePath: TEdit;
    ImageList1: TImageList;
    procedure btnChooseDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    FDirectory: string;
    function nodePath(ANode: TTreeNode): string;
    procedure FillNode(Parent: TTreeNode; path: string);
    procedure FillDirectoryTree(rootDir: string);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  proj_common, IniFiles;

{$R *.dfm}

procedure TMainForm.btnChooseDirClick(Sender: TObject);
begin
  if SelectDirectory('Select folder to browse', edDir.Text, '', False, FDirectory) then
  begin
    edDir.Text := FDirectory;
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
  FillNode(nil, FDirectory);
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
  result:=IncludeTrailingPathDelimiter(FDirectory)+result;
end;

procedure TMainForm.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  FillNode(Node, nodePath(Node));
end;

end.
