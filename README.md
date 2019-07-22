# reflex-widgets
Reflex FRP widgets at Atidot
- Using `Nixpkgs`
- Pinned down to 19.03 release and a specific commit of `reflex-platform`

# [ChartJS]
- [Main.hs](./reflex-chartjs/app/Main.hs)
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
- [Main.hs](./reflex-codemirror/app/Main.hs)
~~~ shell
13:00 barak@berkos:~/Development/atidot/reflex-widgets (master) $ make codemirror
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.reflex-codemirror
trace: WARNING: ignoring dependency on doctest
/nix/store/zfj7yig8wchn6sdlba9fa36920qacq13-reflex-codemirror-0.1.0.0

13:00 barak@berkos:~/Development/atidot/reflex-widgets (master) $ chromium ./result/bin/reflex-codemirror-exe.jsexe/index.html
Fontconfig warning: "/etc/fonts/fonts.conf", line 86: unknown element "blank"
Opening in existing browser session.
13:00 barak@berkos:~/Development/atidot/reflex-widgets (master) $
~~~
![alt text][codemirror_gif]

# [JsonEditor]
- [Main.hs](./reflex-jsoneditor/app/Main.hs)
~~~ shell
13:11 barak@berkos:~/Development/atidot/reflex-widgets (master) $ make jsoneditor
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.reflex-jsoneditor
/nix/store/41wymhh7ynzr484xvc8q7ycz3awzr0bg-reflex-jsoneditor-0.1.0.0

13:13 barak@berkos:~/Development/atidot/reflex-widgets (master) $ chromium ./result/bin/reflex-jsoneditor-exe.jsexe/index.html
Fontconfig warning: "/etc/fonts/fonts.conf", line 86: unknown element "blank"
Opening in existing browser session.
~~~
![alt text][jsoneditor_gif]

# [JExcel]
- [Main.hs](./reflex-jexcel/app/Main.hs)
~~~ shell
09:58 barak@berkos:~/Development/atidot/reflex-widgets (master) $ make jexcel
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.reflex-jexcel
trace: WARNING: ignoring dependency on doctest
/nix/store/7jrwsbfkz73b8mghlnh962qbhykva7p0-reflex-jexcel-0.1.0.0
09:58 barak@berkos:~/Development/atidot/reflex-widgets (master) $ chromium ./result/bin/reflex-jexcel-exe.jsexe/index.html
Fontconfig warning: "/etc/fonts/fonts.conf", line 86: unknown element "blank"
~~~
![alt text][jexcel_gif]

# [HTML5 FileReader]
- [Main.hs](./reflex-fileapi/app/Main.hs)
~~~ shell
01:59 barak@berkos:~/Development/atidot/reflex-widgets (master) $ make fileapi
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.reflex-fileapi
trace: WARNING: ignoring dependency on doctest
/nix/store/lhbbxdbik3j4yg9cwpfv9dpxfrgl9yig-reflex-fileapi-0.1.0.0
01:59 barak@berkos:~/Development/atidot/reflex-widgets (master) $ chromium ./result/bin/reflex-fileapi-exe.jsexe/index.html
~~~
![alt text][fileapi_gif]

# PowerQuery parser editor
- [PowerQuery_editor]

[ChartJS]: https://www.chartjs.org/
[chartjs_gif]: https://media.giphy.com/media/KbBhOsvOpV3E2SmYeC/giphy.gif
[CodeMirror]: https://codemirror.net/
[codemirror_gif]: https://media.giphy.com/media/H1MMpzOlyb2kne4M7i/giphy.gif
[JsonEditor]: https://github.com/josdejong/jsoneditor
[jsoneditor_gif]: https://media.giphy.com/media/MEpNi9paiNYld5AXHi/giphy.gif
[JExcel]: https://bossanova.uk/jexcel/v2/
[jexcel_gif]: https://media.giphy.com/media/YSY5IbCmhhGQJQSyAh/giphy.gif
[HTML5 FileReader]: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
[fileapi_gif]: https://media.giphy.com/media/WOHPlzKN1bxGZ7BGxx/giphy.gif
[PowerQuery_editor]: https://github.com/Atidot/language-powerquery/tree/master/language-powerquery-editor#language-powerquery-editor
