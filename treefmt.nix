# treefmt.nix
{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs = {
    # TODO: Add more languages later, for now just format
    # nix files
    nixfmt.enable = true;
  };
}
