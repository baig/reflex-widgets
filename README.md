# reflex-widgets
Reflex FRP widgets at Atidot

## NOTE
We're currently working on refactoring our old widgets code base  
to use `jsaddle` so some of the widgets still won't build with `reflex-platform`  

# [ChartJS]
~~~ shell
21:01 barak@berkos:~/Development/atidot/reflex-widgets (master) $ make chartjs
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.reflex-chartjs
trace: WARNING: ignoring dependency on doctest
/nix/store/x91ja0nczrvryyjdzqrxp3d9y68i8iy6-reflex-chartjs-0.1.0.0

21:01 barak@berkos:~/Development/atidot/reflex-widgets (master) $ chromium ./result/bin/reflex-chartjs-exe.jsexe/index.html
Fontconfig warning: "/etc/fonts/fonts.conf", line 86: unknown element "blank"
~~~
![alt text][chartjs_gif]

# [CodeMirror]
- TODO

# [JsonEditor]
- TODO

[ChartJS]: https://www.chartjs.org/
[chartjs_gif]: reflex-chartjs/reflex-chartjs.gif
[CodeMirror]: https://codemirror.net/
[JsonEditor]: https://github.com/josdejong/jsoneditor
