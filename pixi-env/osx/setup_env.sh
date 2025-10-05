cp ../../afids/pixi-env/osx/pixi* .
mkdir -p .pixi
cp ../../afids/pixi-env/osx/config.toml .pixi
pixi install
pixi run configure


