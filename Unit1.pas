unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.Dwmapi,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

type
  TTransparencyMethod = (
    tmNone,
    tmSetWindowCompositionAttribute,
    tmDwmBlur,
    tmMagnification
  );

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMagWnd: HWND;
    FMagLib: HMODULE;

    procedure ApplyMethod(Method: TTransparencyMethod);
    procedure ClearEffects;

    procedure ApplySWCA;
    procedure ApplyDwmBlur;
    procedure ApplyMagnifier;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// SWCA TYPES

type
  TAccentState = (
    ACCENT_DISABLED = 0,
    ACCENT_ENABLE_GRADIENT = 1,
    ACCENT_ENABLE_TRANSPARENTGRADIENT = 2,
    ACCENT_ENABLE_BLURBEHIND = 3,
    ACCENT_ENABLE_ACRYLICBLURBEHIND = 4
  );

  TAccentPolicy = record
    AccentState: TAccentState;
    AccentFlags: DWORD;
    GradientColor: DWORD;
    AnimationId: DWORD;
  end;

  TWindowCompositionAttributeData = record
    Attribute: DWORD;
    Data: Pointer;
    SizeOfData: SIZE_T;
  end;

const
  WCA_ACCENT_POLICY = 19;

type
  TSetWindowCompositionAttribute = function(
    hWnd: HWND;
    const Data: TWindowCompositionAttributeData
  ): BOOL; stdcall;

// Magnification API

type
  TMagInitialize = function: BOOL; stdcall;
  TMagUninitialize = function: BOOL; stdcall;

const
  WC_MAGNIFIER = 'Magnifier';

//////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Add('None');
  ComboBox1.Items.Add('SetWindowCompositionAttribute (Acrylic)');
  ComboBox1.Items.Add('DWM Blur Behind');
  ComboBox1.Items.Add('Windows Magnification API');
  ComboBox1.ItemIndex := 0;

  FMagWnd := 0;
  FMagLib := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  MagUninitialize: TMagUninitialize;
begin
  if FMagWnd <> 0 then
    DestroyWindow(FMagWnd);

  if FMagLib <> 0 then
  begin
    @MagUninitialize := GetProcAddress(FMagLib, 'MagUninitialize');
    if Assigned(MagUninitialize) then
      MagUninitialize;
    FreeLibrary(FMagLib);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  ApplyMethod(TTransparencyMethod(ComboBox1.ItemIndex));
end;

procedure TForm1.ApplyMethod(Method: TTransparencyMethod);
begin
  ClearEffects;
  case Method of
    tmSetWindowCompositionAttribute: ApplySWCA;
    tmDwmBlur: ApplyDwmBlur;
    tmMagnification: ApplyMagnifier;
  end;
end;

procedure TForm1.ClearEffects;
var
  Blur: DWM_BLURBEHIND;
  Accent: TAccentPolicy;
  Data: TWindowCompositionAttributeData;
  SWCA: TSetWindowCompositionAttribute;
  User32: HMODULE;
  MagUninitialize: TMagUninitialize;
begin
  if FMagWnd <> 0 then
  begin
    DestroyWindow(FMagWnd);
    FMagWnd := 0;
  end;

  if FMagLib <> 0 then
  begin
    @MagUninitialize := GetProcAddress(FMagLib, 'MagUninitialize');
    if Assigned(MagUninitialize) then
      MagUninitialize;
    FreeLibrary(FMagLib);
    FMagLib := 0;
  end;

  SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);

  ZeroMemory(@Blur, SizeOf(Blur));
  Blur.dwFlags := DWM_BB_ENABLE;
  Blur.fEnable := False;
  Blur.hRgnBlur := 0;
  DwmEnableBlurBehindWindow(Handle, Blur);

  User32 := LoadLibrary('user32.dll');
  if User32 <> 0 then
  try
    @SWCA := GetProcAddress(User32, 'SetWindowCompositionAttribute');
    if Assigned(SWCA) then
    begin
      ZeroMemory(@Accent, SizeOf(Accent));
      Accent.AccentState := ACCENT_DISABLED;
      Data.Attribute := WCA_ACCENT_POLICY;
      Data.Data := @Accent;
      Data.SizeOfData := SizeOf(Accent);
      SWCA(Handle, Data);
    end;
  finally
    FreeLibrary(User32);
  end;
end;

// Acrylic (SWCA)

procedure TForm1.ApplySWCA;
var
  Accent: TAccentPolicy;
  Data: TWindowCompositionAttributeData;
  SWCA: TSetWindowCompositionAttribute;
  User32: HMODULE;
begin
  User32 := LoadLibrary('user32.dll');
  if User32 = 0 then Exit;

  @SWCA := GetProcAddress(User32, 'SetWindowCompositionAttribute');
  if not Assigned(SWCA) then Exit;

  ZeroMemory(@Accent, SizeOf(Accent));
  Accent.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND;
  Accent.GradientColor := $80202020;
  Accent.AccentFlags := 2;

  Data.Attribute := WCA_ACCENT_POLICY;
  Data.Data := @Accent;
  Data.SizeOfData := SizeOf(Accent);

  SWCA(Handle, Data);
end;

// DWM Blur

procedure TForm1.ApplyDwmBlur;
var
  Blur: DWM_BLURBEHIND;
begin
  ZeroMemory(@Blur, SizeOf(Blur));
  Blur.dwFlags := DWM_BB_ENABLE;
  Blur.fEnable := True;
  Blur.hRgnBlur := 0;
  DwmEnableBlurBehindWindow(Handle, Blur);
end;

// Magnification

procedure TForm1.ApplyMagnifier;
var
  MagInitialize: TMagInitialize;
begin
  FMagLib := LoadLibrary('Magnification.dll');
  if FMagLib = 0 then Exit;

  @MagInitialize := GetProcAddress(FMagLib, 'MagInitialize');
  if not Assigned(MagInitialize) then Exit;

  if not MagInitialize then Exit;

  SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  SetLayeredWindowAttributes(Handle, 0, 220, LWA_ALPHA);

  FMagWnd := CreateWindow(
    WC_MAGNIFIER,
    nil,
    WS_CHILD or WS_VISIBLE,
    0,
    0,
    ClientWidth,
    ClientHeight,
    Handle,
    0,
    HInstance,
    nil);

  if FMagWnd <> 0 then
  begin
    SetWindowLong(FMagWnd, GWL_EXSTYLE,
      GetWindowLong(FMagWnd, GWL_EXSTYLE) or WS_EX_TRANSPARENT);

    SetWindowPos(FMagWnd, HWND_BOTTOM, 0, 0, ClientWidth, ClientHeight, SWP_SHOWWINDOW);
  end;
end;

end.

