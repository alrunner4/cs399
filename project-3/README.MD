# Setup Instructions

## Install glfw-idris

```
cd glfw-idris
```

Edit src/Graphics/Util/GlfwConfig.idr to reflect the absolute paths of the project include and lib
directories on your system.

```
path/to/idris --install glfw.ipkg
```

## Install gl-idris

```
cd gl-idris
```

Edit src/Graphics/Rendering/Config.idr to reflect the absolute paths of the project include and lib
directories on your system.

```
PROJECT_ROOT=/absolute/path/to/project-3 IDRIS_RTS=/absolute/path/to/idris/lib/rts path/to/idris --install gl.ipkg
```

The makefile for gl-idris (src/MakefileGl) uses the above two environment variables to find includes
as part of building gl_idris.o.
