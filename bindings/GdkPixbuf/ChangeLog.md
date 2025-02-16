### 2.0.29

+ Relax constraint on text

### 2.0.28

+ Refresh list of nullables

### 2.0.27

+ Update to haskell-gi(-base)-0.26

### 2.0.25

+ Update to haskell-gi(-base)-0.25

### 2.0.24

+ Upgrade to haskell-gi-base-0.24

### 2.0.23

+ Update for haskell-gi(-base)-0.23

### 2.0.22

+ Add support for inherited dependencies

### 2.0.21

+ Tighten bounds in haskell-gi(-base)

### 2.0.20

+ List autogenerated modules in .cabal file

### 2.0.19

+ Update to haskell-gi(-base) 0.22

### 2.0.18

+ The return value of [pixbufLoaderGetPixbuf](https://hackage.haskell.org/package/gi-gdkpixbuf-2.0.18/docs/GI-GdkPixbuf-Objects-PixbufLoader.html#v:pixbufLoaderGetPixbuf) is nullable, but was not marked as such.

### 2.0.17

+ Update stack version to 12.10

### 2.0.16

+ Fix introspection data for the `filename` parameters of [`pixbufAnimationNewFromFile`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-PixbufAnimation.html#v:pixbufAnimationNewFromFile), [`pixbufNewFromFile`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufNewFromFile), [`pixbufNewFromFileAtSize`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufNewFromFileAtSize), [`pixbufNewFromFileAtScale`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufNewFromFileAtScale), [`pixbufGetFileInfo`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufGetFileInfo), [`pixbufGetFileInfoAsync`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufGetFileInfoAsync) and [`pixbufSavev`](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufSavev). Fixes [issue #160](https://github.com/haskell-gi/haskell-gi/issues/160).

### 2.0.15

+ Remove enable-overloading flags, and use instead explicit CPP checks for 'haskell-gi-overloading-1.0', see [how to disable overloading](https://github.com/haskell-gi/haskell-gi/wiki/Overloading\#disabling-overloading).

### 2.0.15

+ Fix introspection info for [pixbufNew](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufNew), [pixbufCopy](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufCopy), [pixbufCompositeColorSimple](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufCompositeColorSimple) and [pixbufScaleSimple](https://hackage.haskell.org/package/gi-gdkpixbuf/docs/GI-GdkPixbuf-Objects-Pixbuf.html#v:pixbufScaleSimple). The return values in all these cases can be null, but were not marked as such. See https://github.com/haskell-gi/haskell-gi/issues/127.
