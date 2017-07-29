with import <nixpkgs> {}; {
    profunEnv = stdenv.mkDerivation {
        name = "profun-env";
        buildInputs = [ stdenv
                        # Haskell dependencies
                        ghc
                        stack
                        cabal-install
                        mesa
                        freeglut
                        # C dependencies
                        gcc
                        SDL2
                        SDL2_gfx
                        cmake
                        pkgconfig
                      ];
        LD_LIBRARY_PATH="${mesa}/lib:${freeglut}/lib";
    };
}
