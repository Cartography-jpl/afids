This has a basic pixi.toml and pixi.lock file for creating a build environment for testing.
We have linux and osx versions of this.

A simple script "setup_env.sh" copies over the pixi files, and then run "pixi run configure"
for setting this up. Run this from the build directory you want. This is pretty basic,
if you want to tweak anything just run the command in setup_env.sh directly.

After running, you can build with

   pixi shell
   make -j 20 all && make install
