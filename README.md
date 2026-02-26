# Delphi Transparency Demo

A Delphi VCL application demonstrating multiple window transparency and blur techniques on Windows.

This project showcases three distinct methods to apply window transparency and blur effects to a native Delphi form: **SetWindowCompositionAttribute (Acrylic)**, **DWM Blur Behind**, and **Windows Magnification API**.

---

![](/TransparencyDemo.gif)

## Overview

This tool allows developers to experiment with different Windows transparency techniques in a single VCL form. Users can switch between methods via a combo box, and the application ensures that switching methods **fully resets previous effects**, preventing overlaps or visual artifacts.

Supported methods:

1. **None** — No transparency applied  
2. **SetWindowCompositionAttribute (Acrylic)** — Modern blur/alpha effect  
3. **DWM Blur Behind** — Classic Aero blur  
4. **Windows Magnification API** — Custom alpha blending using the Magnifier window  

---

## Features

- Native Delphi VCL Win32 / Win64 implementation  
- Switchable transparency methods at runtime  
- Automatic clearing of previous effects  
- ComboBox and form controls remain interactive  
- Magnifier API window placed behind controls to avoid input blocking  
- Lightweight and self-contained (no external dependencies)

---

## Requirements

- Windows 10 / 11  
- `Magnification.dll` available (built-in to Windows)  
- Delphi 12 or compatible version for compilation  

---

## Example Usage

Run the compiled application and select a transparency method from the drop-down menu:

1. Launch the application:
```
DelphiTransparencyDemo.exe
```

2. Select a method from the combo box:  
   - None  
   - SetWindowCompositionAttribute (Acrylic)  
   - DWM Blur Behind  
   - Windows Magnification API  

---

## How It Works

### Method Isolation

The application ensures that **switching transparency methods fully undoes any previous effect**, including:

- Resetting **SWCA / Acrylic** via `ACCENT_DISABLED`  
- Disabling **DWM Blur Behind**  
- Destroying and uninitializing the **Magnifier window**  

This prevents issues such as controls being hidden or multiple blur layers stacking.

### Magnifier Window Fix

To prevent UI elements (like the combo box) from being blocked:

- Magnifier window uses `WS_EX_TRANSPARENT`  
- Positioned behind all child controls using `HWND_BOTTOM`  
- Allows interaction with form controls while showing the magnification alpha effect

---

## Project Structure

Core components:

- `ApplySWCA` — Sets Acrylic blur using `SetWindowCompositionAttribute`  
- `ApplyDwmBlur` — Enables DWM Blur Behind  
- `ApplyMagnifier` — Uses Windows Magnification API for alpha blending  
- `ClearEffects` — Resets all previously applied transparency effects  
- `ApplyMethod` — Switches between methods safely  
- ComboBox for runtime selection of transparency technique  

---

## Intended Use

- Learning Windows transparency APIs  
- UI experimentation with blur and alpha effects  
- Prototype effects for Delphi VCL applications  

---

## Limitations

- All methods have potential for performance impact
- SWCA/Acrylic effects require Windows 10 / 11  
- Visual appearance may vary based on OS theme and hardware acceleration  

---

> ⚠️ This readme (documentation) was generated with the assistance of AI.  

> ⚠️ All code is human written.
