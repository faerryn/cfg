.PHONY: bspwm firefox fish fontconfig git inputrc mpv neovim picom rofi sh

all: bspwm firefox fish fontconfig git inputrc mpv neovim picom rofi sh

bspwm:
	stow bspwm/

FIREFOX_PROFILE:="$(HOME)"/.mozilla/firefox/"$(shell sed -n -e '/Path/s/^Path=//p' ~/.mozilla/firefox/profiles.ini)"

firefox:
	stow firefox -t "$(FIREFOX_PROFILE)"

fish:
	stow fish/

fontconfig:
	stow fontconfig/

git:
	stow git/

inputrc:
	stow inputrc/

mpv:
	stow mpv/

neovim:
	stow neovim/

picom:
	stow picom/

rofi:
	stow rofi/

sh:
	stow sh/

