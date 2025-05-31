# emacs-print-to-file

Generate PDF (1-up or 2-up) or PostScript files from the current Emacs buffer using `ps-print`, `psnup`, and `ps2pdf`.

## Features

This package provides four interactive commands for pdf output:

- `pdf-print-buffer-to-file`:        Create a 1-up PDF with faces (color, if enabled)
- `pdf-print-buffer-to-file-no-faces`:   Create a 1-up PDF without faces (black & white)
- `pdf-print-buffer-to-file-2up`:    Create a 2-up PDF with faces
- `pdf-print-buffer-to-file-2up-no-faces`: Create a 2-up PDF without faces

Each function:
- Converts the current buffer to PostScript (`.ps`)
- Optionally converts the PostScript to 2-up using `psnup`
- Converts `.ps` or `.2up.ps` to `.pdf` using `ps2pdf`
- Prompts before overwriting any existing files
- Deletes intermediate `.ps` files after successful PDF creation

## Also Supports PostScript Output

For PostScript-only versions (no PDF conversion), use:

- `ps-print-buffer-to-file`
- `ps-print-buffer-to-file-no-faces`
- `ps-print-buffer-to-file-2up`
- `ps-print-buffer-to-file-2up-no-faces`

## Requirements

- **Emacs** with `ps-print` (built-in)
- `ps2pdf` (part of [Ghostscript](https://ghostscript.com/))
- `psnup` (part of [psutils](https://wiki.debian.org/psutils))

Install Ghostscript and psutils via your package manager:

```bash
# Debian/Ubuntu
sudo apt install ghostscript psutils

# macOS (Homebrew)
brew install ghostscript psutils
