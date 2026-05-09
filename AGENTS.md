# Repository Guidelines

## Project Structure & Module Organization
`d/` contains the Emacs configuration that gets installed into `~/.emacs.d`. The main entry point is `d/init.el`, with platform-specific overrides in `d/darwin.el` and `d/linux.el`. Custom local packages and helpers live under `d/lisp/`. Operational scripts are in `bin/`, including install, compile, publish, and package update helpers. The root `Makefile` is the normal entry point for local development tasks.

## Build, Test, and Development Commands
Use `make install` to copy `d/` into `~/.emacs.d` and wire the Gnus symlink. Use `make compile` to byte-compile the installed configuration. Use `make load` to batch-load `setup-package.el` and `init.el`; this installs packages and has side effects in `~/.emacs.d`. Use `make install-and-compile` for the full install and verification path. Use `make clean` to remove local Emacs state and generated `.elc` files. `make publish` runs the repo’s publish helper.

## Coding Style & Naming Conventions
This repository is primarily Emacs Lisp with small POSIX shell scripts. Follow standard Lisp indentation and keep forms compact and readable; align keyword blocks under `use-package` declarations as in `d/init.el`. Prefer kebab-case for functions and variables, and keep personal helpers namespaced with the existing `aar/` prefix. Shell scripts in `bin/` should stay portable `sh`, not Bash-specific unless necessary.

## Testing Guidelines
There is no formal `test/` directory or ERT suite in this repository. Verify changes by running `make load` for startup/package issues and `make compile` for byte-compilation errors. For changes in `bin/` scripts or install logic, run `make install-and-compile` in a disposable Emacs setup if possible, since it writes to `~/.emacs.d`.

## Commit & Pull Request Guidelines
Recent history favors short, imperative commit subjects, often sentence case or a `fix:` prefix, for example `fix: build from destination and refresh packages`. Keep commits focused on one logical change. Pull requests should describe the user-visible effect, note any changes to package installation or files under `~/.emacs.d`, and include manual verification steps such as `make load` or `make compile`.

## Security & Configuration Tips
Treat `make load`, `make install`, and update scripts as stateful operations: they modify your home-directory Emacs setup. Avoid hardcoding machine-specific paths unless they are already part of the repo’s platform-specific files.
