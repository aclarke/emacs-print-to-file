# emacs-print-to-file

Generate PDF (1-up or 2-up) or PostScript files from the current Emacs buffer using `ps-print`, `psnup`, and `ps2pdf`.

## Features

This package provides four interactive commands for PDF output:

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

These commands do not invoke `ps2pdf` and leave the `.ps` or `.2up.ps` file intact.

## File Naming Convention

The output files are named based on the current buffer's file name. For example, if the buffer is visiting `hello_world.c`, the resulting files will be:

- `hello_world.c.ps`        ← 1-up PostScript
- `hello_world.c.2up.ps`    ← 2-up PostScript
- `hello_world.c.pdf`       ← 1-up PDF
- `hello_world.c.2up.pdf`   ← 2-up PDF

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
```

### ⚠️ Note: You May Not Need This Package for Simple PostScript Output

Emacs already provides a built-in way to write PostScript directly to a file:

```elisp
C-u M-x ps-print-buffer-with-faces
```

This prompts for a file name and writes PostScript output immediately.  
If you only need basic `.ps` output and don't care about 2-up layout or PDF conversion,  
you can use that built-in functionality without installing this package.

This package is primarily helpful if you:

- Frequently convert buffers to **PDF**
- Want **2-up layout** formatting
- Prefer **automated file naming**

