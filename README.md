# emacs-print-to-file

Generate PDF (1-up or 2-up) or PostScript files from the current Emacs buffer using `ps-print`, `psnup`, and `ps2pdf`.

## Features

This package provides four interactive commands for **PDF** output:

- `pdf-print-buffer-to-file`
- `pdf-print-buffer-to-file-with-faces`
- `pdf-print-buffer-to-file-2up`
- `pdf-print-buffer-to-file-2up-with-faces`

And four commands for **PostScript** output:

- `ps-print-buffer-to-file`
- `ps-print-buffer-to-file-with-faces`
- `ps-print-buffer-to-file-2up`
- `ps-print-buffer-to-file-2up-with-faces`

Commands ending in -with-faces preserve syntax highlighting and use
color output (if enabled in Emacs).  The others generate plain
black-and-white output.

## Behavior

Each function:
- Converts the current buffer to PostScript (`.ps`)
- Optionally formats the `.ps` as 2-up using `psnup`
- Converts `.ps` or `.2up.ps` to `.pdf` using `ps2pdf` (PDF only)
- Prompts before overwriting any existing files
- Deletes intermediate `.ps` files for PDF output
- Leaves `.ps` files for PostScript-only output

## File Naming Convention

Output filenames are based on the current buffer’s filename. For example, with `hello_world.c`:

- `hello_world.c.ps`        ← 1-up PostScript
- `hello_world.c.2up.ps`    ← 2-up PostScript
- `hello_world.c.pdf`       ← 1-up PDF
- `hello_world.c.2up.pdf`   ← 2-up PDF

## Requirements

- **Emacs** with `ps-print` (built-in)
- [`ps2pdf`](https://ghostscript.com/) (from Ghostscript)
- [`psnup`](https://wiki.debian.org/psutils) (from psutils)

Install Ghostscript and psutils via your package manager:

```bash
# Debian/Ubuntu
sudo apt install ghostscript psutils

# macOS (Homebrew)
brew install ghostscript psutils
```

## ⚠️ Note: You Do Not Need This for Basic PostScript Output

Emacs already includes many built-in ps- functions that can write
PostScript directly to a file.  They support this via an optional
prefix argument, which enables a prompt for the output file name.

For example:

```elisp
C-u M-x ps-print-buffer
```
This prompts for a file name and writes PostScript output immediately.  

This package is primarily helpful if you:
- Regularly convert buffers to **PDF**
- Want **2-up formatting**
- Prefer **automated file naming** and cleanup
