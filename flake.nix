{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    inputs@{
      self,
      flake-parts,
      nixpkgs,
      devenv,
      treefmt-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        { pkgs, system, ... }:
        let
          treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;

          # TODO: For future use doing nix builds
          _customEmacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
            epkgs:
            with epkgs.melpaPackages;
            [
              citeproc
              htmlize
              ox-rss
            ]
            ++ (with epkgs.elpaPackages; [
              org
            ])
          );
        in
        {
          # This sets `pkgs` to a nixpkgs with allowUnfree option set.
          _module.args.pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          # TODO:
          # nix build
          packages = {
          };

          # TODO:
          # nix run
          apps = {
          };

          # nix develop
          devShells = {
            # `nix develop --impure`
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                (
                  { pkgs, lib, ... }:
                  {
                    packages = with pkgs; [
                      bash
                      sbcl
                    ];

                    languages.lua = {
                      enable = true;
                      package = pkgs.lua.withPackages (
                        ps: with ps; [
                          fennel
                          luafilesystem
                        ]
                      );
                    };

                    languages.python = {
                      enable = true;
                    };

                    languages.typescript = {
                      enable = true;
                    };
                  }
                )
              ];
            };
          };

          # nix fmt
          formatter = treefmtEval.config.build.wrapper;
        };

    };
}
