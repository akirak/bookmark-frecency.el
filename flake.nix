{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    emacs-ci.url = "github:purcell/nix-emacs-ci";
    twist.url = "github:emacs-twist/twist.nix";
    # See https://github.com/NixOS/nix/issues/9339
    elisp-lock-dir.url = "path:./lock";
    emacs-builtins.url = "github:emacs-twist/emacs-builtins";
    emacs-builtins.inputs = {
      # This reduces the number of entries in flake.lock but functionally has no
      # effect.
      emacs-ci.follows = "emacs-ci";
      twist.follows = "twist";
    };
    melpa.url = "github:akirak/melpa/bookmark-frecency";
    melpa.flake = false;
  };

  outputs = {
    flake-parts,
    systems,
    nixpkgs,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        (
          {flake-parts-lib, ...}:
            flake-parts-lib.importApply ./module.nix {
              inherit (inputs) emacs-builtins;
            }
        )
      ];

      easy-elisp = {
        localPackages = ["bookmark-frecency"];
        melpa = inputs.melpa.outPath;
        lockDir = inputs.elisp-lock-dir.outPath;
        lockInputName = "elisp-lock-dir";
      };

      perSystem = {
        system,
        config,
        pkgs,
        ...
      }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.twist.overlays.default
          ];
        };

        easy-elisp = {
          emacsPackageSet = inputs.emacs-ci.packages.${system};
          defaultEmacsPackage = inputs.emacs-ci.packages.emacs-snapshot;
        };

        devShells.default = config.pre-commit.devShell;

        # elisp-byte-compile runs a local version of Emacs, which is available
        # in the Nix sandbox
        pre-commit.check.enable = false;
        pre-commit.settings.excludes = ["^lock/"];

        # pre-commit checks for non-elisp files
        pre-commit.settings.hooks.actionlint.enable = true;
        pre-commit.settings.hooks.alejandra.enable = true;
        pre-commit.settings.hooks.deadnix.enable = true;
        pre-commit.settings.hooks.nil.enable = true;
        # statix is slow, so I won't use it.

        # pre-commit checks for elisp files
        pre-commit.settings.hooks.elisp-byte-compile.enable = true;
        pre-commit.settings.hooks.elisp-byte-compile.stages = ["push"];
      };
    };
}
