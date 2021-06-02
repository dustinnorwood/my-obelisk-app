{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = with pkgs.haskell.lib; (self: super: {
    monad-alter = dontCheck ((import ./dep/monad-alter) self super);
    servant-reflex = dontCheck ((import ./dep/servant-reflex) self super);
    servant-snap = dontCheck ((import ./dep/servant-snap) self super);
    scrypt = dontCheck super.scrypt;
  });
  packages = {
    obelisk-oauth-backend = hackGet ./dep/obelisk-oauth + /backend;
    obelisk-oauth-common = hackGet ./dep/obelisk-oauth + /common;
  };
})
