
# Project Goal
Given this graphical framework, you will implement some stateful Haskell procedures using IORef and
the ExceptT monad transformer. Examine Main.hs for instructions following the text "REQUIREMENT"
within comments.

# Windows Build Instructions
* install MSYS2 (if you haven't already): https://www.msys2.org/
* launch an msys terminal via the "MSYS2 MinGW 64-bit" shortcut
    (this sets needed environment variables)
* install dependencies via pacman, the MSYS package manager:
    ```
    pacman -S pkg-config glib2-devel mingw-w64-x86_64-{cairo,gobject-introspecton,gtk3}
    ```
* build the project:
    ```
    SET XDG_DATA_DIRS=/mingw64/share
    cabal -j new-build statemachine.hs
    ```
* run the built executable:
    ```
    cabal new-run statemachine
    ```

# Linux Build Instructions (for comparison)
* install the Nix package manager:
    ```
    curl -L https://nixos.org/nix/install | sh
    ```
* build the project:
    ```
    nix-build -A statemachine
    ```
* run the built executable:
    ```
    result/bin/statemachine
    ```

