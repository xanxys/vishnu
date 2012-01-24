multi-view 3d playground

vishnu executable
-----

Currently vishnu provides a 3d canvas where you can draw or read from views. A view is a single eucledian
coordinate with pre-defined interface. Currently supported view types are:

* CUI: this is what you get when you run vishnu.
* IPR: agent controlled by a script in a window
* FlyThrough: a window with FPS-like control
* MsgPack: externally defined interface w/ simple protocol based on msgpack datagrams

Views can spawn other views with relative coordinate (interface not implemented yet).
Notable thing about vishnu is, its underlying lattice structure is hidden with appropriate Gaussian kernel,
so all views are equal in their privillege.

Future direction is to add sound and light propagation and implement in symmetrical P2P
(i.e. no single node holds complete world structure, yet as a network they represent a single world.)

example (not working)
    spawn fly
    light

VsThin
-----
A Haskell wrapper for msgpack-view suitable for interactive use in GHCi. Currently it's draw-only.

    x<-serve 30000 (in GHCi; listen on TCP port 30000)
    mp 127.0.0.1 30000 (in CUI view; create a MsgPack view with given view-server)
    put x 0.1 (0,0,0) (1,1,1) (in GHCi; draw white blob at view origin)

