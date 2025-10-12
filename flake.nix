{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # unstable Nixpkgs

  outputs =
    { self, ... }@inputs:

    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSupportedSystem =
        f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            pkgs = import inputs.nixpkgs { inherit system; };
          }
        );

    in
    {
      devShells = forEachSupportedSystem (
        { pkgs }:
        let
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --nix \
                  --no-nix-pure \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
        in
        {
          default = pkgs.mkShellNoCC rec {
            buildInputs = with pkgs; [
              ghcid
              stack-wrapped
              (ghc.withPackages (
                p: with p; [
                  haskell-language-server
                ]
              ))
              haskellPackages.random
            ];
            NIX_PATH = "nixpkgs=" + pkgs.path;
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
          };
        }
      );

      packages = forEachSupportedSystem (
        { pkgs }:
        {
          default = pkgs.haskell.lib.buildStackProject {
            inherit (pkgs) ghc;
            name = "sudoku-hs";
            src = ./.;
            buildInputs = with pkgs; [
              zlib
              haskellPackages.random
            ];
          };
        }
      );

      apps = forEachSupportedSystem (
        { pkgs }:
        {
          default = {
            type = "app";
            program = "${self.packages.${pkgs.system}.default}/bin/sudoku-hs";
          };
        }
      );
    };
}
