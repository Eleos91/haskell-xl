# haskell-xl
Simple way for screen locking written in haskell

It's my first haskell programm. I forked the c code i took as a reference. 

# Dependencies
- Unixutils-shadow (1.0.0)
- base (4.12.0.0)
- unix (2.7.2.2)
- time (1.8.0.2)
- split (0.2.3.3)
- X11 (1.9)


# TODO:
- Don't use setuid. Find something more appropriate to get the shadow entries (maybe pam?)
- The hash should be fetshed when the user tries to unlock the xcreen and should be deleted 'till the next request, so the hashed password can't be read from memory
- implement xkbcommon to let keys through like VolUp and stuff.
- Creating black windows for every screen or let all screens fade to black (I want nothing too fancy)

# BUGS:
- Works perfectly fine when i start it in my terminal emulator (gnome-shell) but if i start it via a shortcut in a gnome-session, weird stuff happens... I have to investigate.
