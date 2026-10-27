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

## Performance & Startup Tuning
The config targets sub-3s interactive startup and zero `*Warnings*` after launch. Several hard-won behaviors live here; don't undo them casually:

- **`d/early-init.el`** turns off GC and file-name handlers during startup (restored via a 5s timer after launch) and enables `package-quickstart`. It also caps `xterm-query-timeout` at 0.1s — terminals that don't answer OSC fg/bg queries (SSH, tmux, ptys) otherwise cost 2s each in `tty-run-terminal-initialization`.
- **Lazy loading**: `use-package-expand-minimally` is intentionally left off so `:defer`/`:commands`/`:bind` work. Any eagerly-loaded package that has no trigger should get `:defer t`; watch for chains — one eager package can pull `org`/`ox`/`lsp-mode` transitively (traced via an advice-on-`require` stack dump).
- **Stale `.elc` is expensive**: a compiled file older than its source prints a warning *and* blocks with `(sit-for 1)`. Always `make install && make compile` after editing `d/` files, or startup will stall mysteriously.
- **GC after startup**: Emacs 31's native-comp runtime allocates aggressively enough that the 32MB default threshold caused a 50-100ms full-GC hitch ~every second of typing. Post-startup restores to a 256MB `gc-cons-threshold`; raise rather than lower this.
- **Missing commands after upgrading Emacs** (e.g. `org-attach-attach` void): newer org ships `org-modules` without `org-attach`. Require the missing feature *inside the functions/commits that call it* — remember `use-package :after` delays `:config` until that feature loads, so configs gated on `:after (org-agenda)` don't run when org loads by another path.
- **Compile-time warnings** for functions defined elsewhere: add `declare-function` at the top of the file rather than eager-requiring the package.
- Known non-startup pre-existing byte-compile failures (do NOT chase): `lisp/clocktable-by-tag.el`, `lisp/d-mode.el`, `lisp/durendal.el` (slime), `lisp/gist.el` (gh-gist). Identical on base commit `17ce81a`.
- `emacs-init-time` is a float; `emacs-uptime` is a string. `make load` refreshes `package-quickstart`; interactive tests need a pty harness (`emacs -nw` + `sit-for` blockers don't show in batch mode).

## Security & Configuration Tips
Treat `make load`, `make install`, and update scripts as stateful operations: they modify your home-directory Emacs setup. Avoid hardcoding machine-specific paths unless they are already part of the repo’s platform-specific files.
