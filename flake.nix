{
  description = "sudoku-hs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; 

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      eachSystem =
        f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = eachSystem (
        pkgs:
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
            buildInputs = with pkgs; [
              ghcid
              stack-wrapped
              (ghc.withPackages (
                p: with p; [
                  haskell-language-server
                ]
              ))
            ];
        in
        {
          default = pkgs.mkShellNoCC {
            inherit buildInputs;
            NIX_PATH = "nixpkgs=" + pkgs.path;
            #LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
          };
        }
      );

      packages = eachSystem (
        pkgs:
        let
          fs = pkgs.lib.fileset;
        in
        {
          default = pkgs.haskell.lib.buildStackProject {
            inherit (pkgs) ghc;
            name = "sudoku-hs";
            src = fs.toSource {
              root = ./.;
              fileset = fs.unions [
                ./src/Main.hs
                ./stack.yaml
                ./stack.yaml.lock
                ./package.yaml
              ];
            };
            # buildInputs = with pkgs; [ haskellPackages.random ];
          };
        }
      );

      apps = eachSystem (pkgs: {
        default = {
          type = "app";
          program = "${self.packages.${pkgs.system}.default}/bin/sudoku-hs";
        };
      });
    };
}
