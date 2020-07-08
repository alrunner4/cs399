
# One-Dimensional Battle Simulator

Actors in this simulation can take the following actions:
 * move to any unoccupied index
 * view the world at an offset
 * attack an adjacent index

# Concurrency: IORef vs STM

While IORef does provide atomic modification, coordination between multiple variables is left to the
library user. Your task for this project is to convert the uses of IORef to STM's TVar to mitigate
the identified race conditions.

Look for comments in the source documenting REQUIREMENTs and NOTEs.

* TVar is the STM equivalent of IORef
* TMVar is the STM equivalent of MVar

Look up the Haddock documentation on hackage.haskell.org when you run into a new function.

As always, let me know if you run into something you can't figure out with a bit of your own
research!

# Windows Build Instructions
* install MSYS2 (if you haven't already): https://www.msys2.org/
* launch an msys terminal via the "MSYS2 MinGW 64-bit" shortcut
    (this sets needed environment variables)
* install dependencies via pacman, the MSYS package manager:
    ```
    pacman -S pkg-config glib2-devel mingw-w64-x86_64-{cairo,gdk-pixbuf,gobject-introspection,gtk3}
    ```
* if you don't have the cabal build tool installed yet:
    ```
    choco install cabal
    ```
* build the project:
    ```
    export XDG_DATA_DIRS=/mingw64/share
    cabal -j new-build concurrency.cabal
    ```
	If the build encounters errors, you might try without -j, which runs builds in parallel.
* run the built executable:
    ```
    cabal new-run concurrency
    ```

# Linux Build Instructions (for comparison)
* install the Nix package manager:
    ```
    curl -L https://nixos.org/nix/install | sh
    ```
* build the project:
    ```
    nix-build -A concurrency
    ```
* run the built executable:
    ```
    result/bin/concurrency +RTS -N
    ```

