# Delphi Transparency Demo

A Delphi VCL application demonstrating multiple window transparency and blur techniques on Windows.

This project showcases several distinct methods to apply window transparency and blur effects to a native Delphi form: **SetWindowCompositionAttribute (Acrylic)**, **DWM Blur Behind**, **Windows Magnification API**, **AlphaBlend (Simple)**, **AlphaBlend (Complex)**, and **Smooth Acrylic (Hybrid)**.

---

![](/Demo.gif)

## Overview

This tool allows developers to experiment with different Windows transparency techniques in a single VCL form. Users can switch between methods via a combo box, and the application ensures that switching methods **fully resets previous effects**, preventing overlaps or visual artifacts.

Supported methods:

1. **None** — No transparency applied  
2. **AlphaBlend (Simple)** — Standard layered window alpha blending  
3. **AlphaBlend (Complex)** — Gradient alpha blending with per-pixel control  
4. **SetWindowCompositionAttribute (Acrylic)** — Modern blur/alpha effect (*requires HDR-capable hardware for best results*)  
5. **DWM Blur Behind** — Classic Aero blur (*performance may vary on HDR displays*)  
6. **Windows Magnification API** — Custom alpha blending using the Magnifier window  
7. **Smooth Acrylic (Hybrid)** — Dynamic SWCA blur that adapts during window resizing (*optimized for HDR-capable GPUs*)  

---

## Features

- Native Delphi VCL Win32 / Win64 implementation  
- Switchable transparency methods at runtime  
- Automatic clearing of previous effects  
- ComboBox and form controls remain interactive  
- Magnifier API window placed behind controls to avoid input blocking  
- Smooth Acrylic dynamically adapts during resize  
- Lightweight and self-contained (no external dependencies)  
- Certain blur effects may require HDR-capable hardware for proper rendering

---

## Requirements

- Windows 10 / 11  
- `Magnification.dll` available (built-in to Windows)  
- Delphi 12 or compatible version for compilation  
- HDR-capable hardware (GPU + Monitor) recommended for Acrylic / Smooth Acrylic effects

---

## Project Structure

Core components:

- `ApplyAlphaBlend` — Applies simple layered window alpha blending  
- `ApplyComplexAlphaBlend` — Creates per-pixel alpha gradient effects  
- `ApplySWCA` — Sets Acrylic blur using `SetWindowCompositionAttribute`  
- `ApplyDwmBlur` — Enables DWM Blur Behind  
- `ApplyMagnifier` — Uses Windows Magnification API for alpha blending  
- `ApplySmoothAcrylic` — Hybrid Acrylic effect with dynamic WndProc adjustments  
- `SetSWCAAccent` — Helper to set specific SWCA accent states  
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
- SWCA/Acrylic and Smooth Acrylic effects perform best on **HDR-capable hardware**  
- Visual appearance may vary based on OS theme and hardware acceleration  
- Some older GPUs & Monitors may not fully support advanced rendering methods

---

> ⚠️ This readme (documentation) was generated with the assistance of AI.  

> ⚠️ All code is human written.
