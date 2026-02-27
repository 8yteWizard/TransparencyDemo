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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Math, Vcl.ComCtrls;

type
  TTransparencyMethod = (
    tmNone,
    tmAlphaBlend,
    tmComplexAlphaBlend,
    tmSetWindowCompositionAttribute,
    tmDwmBlur,
    tmMagnification,
    tmSmoothAcrylic
  );

  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMagWnd: HWND;
    FMagLib: HMODULE;
    FOriginalWndProc: Pointer;

    procedure ApplyMethod(Method: TTransparencyMethod);
    procedure ClearEffects;

    procedure ApplyAlphaBlend;
    procedure ApplyComplexAlphaBlend;
    procedure ApplySWCA;
    procedure ApplyDwmBlur;
    procedure ApplyMagnifier;
    procedure ApplySmoothAcrylic;
    procedure SetSWCAAccent(hWnd: HWND; State: Integer);
    function SmoothAcrylicWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Global forward WndProc for smooth acrylic
function GlobalSmoothAcrylicWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if Assigned(Form1) then
    Result := Form1.SmoothAcrylicWndProc(hWnd, Msg, wParam, lParam)
  else
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;

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
  WC_MAGNIFIER = 'Magnifier';

type
  TSetWindowCompositionAttribute = function(
    hWnd: HWND;
    const Data: TWindowCompositionAttributeData
  ): BOOL; stdcall;

  // Magnification API
  TMagInitialize = function: BOOL; stdcall;
  TMagUninitialize = function: BOOL; stdcall;

//////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Add('None');
  ComboBox1.Items.Add('AlphaBlend (Simple)');
  ComboBox1.Items.Add('AlphaBlend (Complex)');
  ComboBox1.Items.Add('SetWindowCompositionAttribute (Acrylic)');
  ComboBox1.Items.Add('DWM Blur Behind');
  ComboBox1.Items.Add('Windows Magnification API');
  ComboBox1.Items.Add('Smooth Acrylic (Hybrid)');
  ComboBox1.ItemIndex := 0;

  FMagWnd := 0;
  FMagLib := 0;
  FOriginalWndProc := nil;
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

  // Restore original WndProc if subclassed
  if Assigned(FOriginalWndProc) then
    SetWindowLongPtr(Handle, GWLP_WNDPROC, LONG_PTR(FOriginalWndProc));
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  ApplyMethod(TTransparencyMethod(ComboBox1.ItemIndex));
end;

procedure TForm1.ApplyMethod(Method: TTransparencyMethod);
begin
  ClearEffects;
  case Method of
    tmNone: ;
    tmAlphaBlend: ApplyAlphaBlend;
    tmComplexAlphaBlend: ApplyComplexAlphaBlend;
    tmSetWindowCompositionAttribute: ApplySWCA;
    tmDwmBlur: ApplyDwmBlur;
    tmMagnification: ApplyMagnifier;
    tmSmoothAcrylic: ApplySmoothAcrylic;
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

  if Assigned(FOriginalWndProc) then
  begin
    SetWindowLongPtr(Handle, GWLP_WNDPROC, LONG_PTR(FOriginalWndProc));
    FOriginalWndProc := nil;
  end;
end;

// Simple AlphaBlend
procedure TForm1.ApplyAlphaBlend;
const
  ALPHA_VALUE: Byte = 150;
begin
  SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, 0, ALPHA_VALUE, LWA_ALPHA);
end;

// Complex AlphaBlend
procedure TForm1.ApplyComplexAlphaBlend;
var
  bmp: TBitmap;
  x, y: Integer;
  Row: PCardinal;
  A, R, G, B: Byte;
  Blend: BLENDFUNCTION;
  ptSrc, ptDst: TPoint;
  sz: TSize;
  srcDC: HDC;
  oldBmp: HGDIOBJ;
begin
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(ClientWidth, ClientHeight);

    for y := 0 to bmp.Height - 1 do
    begin
      Row := bmp.ScanLine[y];
      for x := 0 to bmp.Width - 1 do
      begin
        A := Round(255 * (x / Max(1, bmp.Width-1)));
        R := (50 * A) div 255;
        G := (120 * A) div 255;
        B := (200 * A) div 255;
        Row^ := (A shl 24) or (R shl 16) or (G shl 8) or B;
        Inc(Row);
      end;
    end;

    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    ptSrc := Point(0, 0);
    ptDst := Point(Left, Top);
    sz.cx := bmp.Width;
    sz.cy := bmp.Height;

    ZeroMemory(@Blend, SizeOf(Blend));
    Blend.BlendOp := AC_SRC_OVER;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := 255;
    Blend.AlphaFormat := AC_SRC_ALPHA;

    srcDC := CreateCompatibleDC(0);
    if srcDC <> 0 then
    try
      oldBmp := SelectObject(srcDC, bmp.Handle);
      try
        if not UpdateLayeredWindow(Handle, 0, @ptDst, @sz, srcDC, @ptSrc, 0, @Blend, ULW_ALPHA) then
          RaiseLastOSError;
      finally
        SelectObject(srcDC, oldBmp);
      end;
    finally
      DeleteDC(srcDC);
    end;
  finally
    bmp.Free;
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
  if not Assigned(SWCA) then
  begin
    FreeLibrary(User32);
    Exit;
  end;

  ZeroMemory(@Accent, SizeOf(Accent));
  Accent.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND;
  Accent.GradientColor := $80202020;
  Accent.AccentFlags := 2;

  Data.Attribute := WCA_ACCENT_POLICY;
  Data.Data := @Accent;
  Data.SizeOfData := SizeOf(Accent);

  SWCA(Handle, Data);
  FreeLibrary(User32);
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
  MagUninitialize: TMagUninitialize;
begin
  FMagLib := LoadLibrary('Magnification.dll');
  if FMagLib = 0 then Exit;

  @MagInitialize := GetProcAddress(FMagLib, 'MagInitialize');
  @MagUninitialize := GetProcAddress(FMagLib, 'MagUninitialize');

  if not Assigned(MagInitialize) then
  begin
    if Assigned(MagUninitialize) then MagUninitialize;
    FreeLibrary(FMagLib);
    FMagLib := 0;
    Exit;
  end;

  if not MagInitialize then
  begin
    if Assigned(MagUninitialize) then MagUninitialize;
    FreeLibrary(FMagLib);
    FMagLib := 0;
    Exit;
  end;

  SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, 0, 150, LWA_ALPHA);

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

/////////////////////////////////////////////////////////

procedure TForm1.SetSWCAAccent(hWnd: HWND; State: Integer);
var
  User32: HMODULE;
  SWCA: TSetWindowCompositionAttribute;
  Accent: TAccentPolicy;
  Data: TWindowCompositionAttributeData;
begin
  User32 := LoadLibrary('user32.dll');
  if User32 = 0 then Exit;

  @SWCA := GetProcAddress(User32, 'SetWindowCompositionAttribute');
  if Assigned(SWCA) then
  begin
    ZeroMemory(@Accent, SizeOf(Accent));
    Accent.AccentState := TAccentState(State);
    Accent.GradientColor := $80202020;
    Accent.AccentFlags := 2;

    Data.Attribute := WCA_ACCENT_POLICY;
    Data.Data := @Accent;
    Data.SizeOfData := SizeOf(Accent);

    SWCA(hWnd, Data);
  end;
  FreeLibrary(User32);
end;

procedure TForm1.ApplySmoothAcrylic;
begin
  SetSWCAAccent(Handle, Integer(ACCENT_ENABLE_ACRYLICBLURBEHIND));

  if not Assigned(FOriginalWndProc) then
    FOriginalWndProc := Pointer(
      SetWindowLongPtr(Handle, GWLP_WNDPROC, LONG_PTR(@GlobalSmoothAcrylicWndProc))
    );
end;

function TForm1.SmoothAcrylicWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  case Msg of
    WM_ENTERSIZEMOVE:
      SetSWCAAccent(hWnd, Integer(ACCENT_ENABLE_BLURBEHIND));
    WM_EXITSIZEMOVE:
      SetSWCAAccent(hWnd, Integer(ACCENT_ENABLE_ACRYLICBLURBEHIND));
  end;

  Result := CallWindowProc(FOriginalWndProc, hWnd, Msg, wParam, lParam);
end;

end.
