{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs = {
      nixpkgs.url = "nixpkgs";
    };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ (self.overlay system) ];
      });
    in
    {
      overlay = (system: final: prev: {
        data-forced = final.haskellPackages.callPackage (import ./default.nix) {};
      });
      packages = forAllSystems (system: {
        data-forced = nixpkgsFor.${system}.data-forced;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.data-forced);
      checks = self.packages;
      devShell = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
          pkgs.haskellPackages.shellFor {
            packages = p: [self.packages.${system}.data-forced];
            withHoogle = true;
            buildInputs = with pkgs.haskellPackages; [
              haskell-language-server
              cabal-install
              pkgs.zlib
            ];
            # Change the prompt to show that you are in a devShell
            # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
          });
  };
}
